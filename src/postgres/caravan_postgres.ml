open Caravan
module Pg = Pg

let src = Logs.Src.create "caravan.postgres" ~doc:"Caravan PostgreSQL backend"

module Log = (val Logs.src_log src)

type t = {
  pool : Pg.pool;
  pending : (string, unit) Hashtbl.t;  (** Queues notified since last looked. *)
  pending_mutex : Mutex.t;
  wakeup : Eio.Condition.t;
  with_timeout : float -> (unit -> unit) -> unit;
}

let name = "postgres"

(* --- Encoding ------------------------------------------------------------- *)

let p s = Some s
let p_int i = Some (string_of_int i)
let p_id id = Some (Int64.to_string id)
let p_time t = Some (Printf.sprintf "%.6f" (Ptime.to_float_s t))
let p_json j = Some (Yojson.Safe.to_string j)
let p_opt f = function None -> None | Some x -> f x
let rfc3339 t = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 t

let columns =
  {sql|id, state, queue, worker, args::text, meta::text, tags::text, priority,
attempt, max_attempts, errors::text,
extract(epoch FROM inserted_at)::float8, extract(epoch FROM scheduled_at)::float8,
extract(epoch FROM attempted_at)::float8, attempted_by,
extract(epoch FROM finished_at)::float8, unique_key|sql}

let decode_error s = failwith ("caravan_postgres: malformed row: " ^ s)
let req = function Some v -> v | None -> decode_error "unexpected NULL"

let time s =
  match Ptime.of_float_s (float_of_string s) with
  | Some t -> t
  | None -> decode_error ("bad timestamp " ^ s)

let json s = Yojson.Safe.from_string s

let row_of (r : string option array) : Row.t =
  let get i = req r.(i) in
  let state =
    match State.of_string (get 1) with Ok s -> s | Error e -> decode_error e
  in
  let tags =
    match json (get 6) with
    | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
    | _ -> []
  in
  let errors =
    match json (get 10) with
    | `List l ->
        List.filter_map (fun j -> Result.to_option (Row.error_of_json j)) l
    | _ -> []
  in
  {
    id = Int64.of_string (get 0);
    state;
    queue = get 2;
    worker = get 3;
    args = json (get 4);
    meta = json (get 5);
    tags;
    priority = int_of_string (get 7);
    attempt = int_of_string (get 8);
    max_attempts = int_of_string (get 9);
    errors;
    inserted_at = time (get 11);
    scheduled_at = time (get 12);
    attempted_at = Option.map time r.(13);
    attempted_by = r.(14);
    finished_at = Option.map time r.(15);
    unique_key = r.(16);
  }

let rows_of (res : Pg.result) = Array.to_list (Array.map row_of res.rows)
let use t f = Pg.use t.pool f
let query t ?params sql = use t (fun c -> Pg.query c ?params sql)

(* --- Insert ----------------------------------------------------------------- *)

let insert_sql =
  Printf.sprintf
    {sql|INSERT INTO caravan_jobs
  (state, queue, worker, args, meta, tags, priority, max_attempts,
   inserted_at, scheduled_at, unique_key)
VALUES
  (CASE WHEN to_timestamp($9::float8) > to_timestamp($8::float8)
        THEN 'scheduled' ELSE 'available' END,
   $1, $2, $3::jsonb, $4::jsonb, $5::jsonb, $6, $7,
   to_timestamp($8::float8), to_timestamp($9::float8), $10)
ON CONFLICT (unique_key)
  WHERE unique_key IS NOT NULL
    AND state IN ('available', 'scheduled', 'executing', 'retryable')
  DO NOTHING
RETURNING %s|sql}
    columns

let find_active_unique_sql =
  Printf.sprintf
    {sql|SELECT %s FROM caravan_jobs
WHERE unique_key = $1
  AND state IN ('available', 'scheduled', 'executing', 'retryable')|sql}
    columns

let insert_params ~now (i : Row.insert) =
  let scheduled_at = Option.value i.i_scheduled_at ~default:now in
  [
    p i.i_queue;
    p i.i_worker;
    p_json i.i_args;
    p_json i.i_meta;
    p_json (`List (List.map (fun s -> `String s) i.i_tags));
    p_int i.i_priority;
    p_int i.i_max_attempts;
    p_time now;
    p_time scheduled_at;
    i.i_unique_key;
  ]

let insert_one conn ~now (i : Row.insert) =
  let rec go tries =
    match rows_of (Pg.query conn ~params:(insert_params ~now i) insert_sql) with
    | [ r ] -> Row.Inserted r
    | _ -> (
        (* Conflict: return the job holding the key. It may have finished in
           between, in which case the key is free again: retry the insert. *)
        let key = Option.get i.i_unique_key in
        match
          rows_of (Pg.query conn ~params:[ p key ] find_active_unique_sql)
        with
        | r :: _ -> Row.Duplicate r
        | [] when tries < 5 -> go (tries + 1)
        | [] -> failwith "caravan_postgres: unique insert kept conflicting")
  in
  go 0

let insert_in conn ~now inserts =
  List.iter
    (fun i ->
      match Row.validate_insert i with
      | Ok _ -> ()
      | Error e -> invalid_arg ("Caravan_postgres.insert: " ^ e))
    inserts;
  List.map (insert_one conn ~now) inserts

let insert t ~now inserts =
  match inserts with
  | [] -> []
  | [ _ ] -> use t (fun c -> insert_in c ~now inserts)
  | _ ->
      use t (fun c ->
          Pg.with_transaction c (fun () -> insert_in c ~now inserts))

(* --- Claiming and outcomes ------------------------------------------------- *)

let fetch_sql =
  Printf.sprintf
    {sql|WITH claimed AS (
  SELECT id AS claimed_id FROM caravan_jobs
  WHERE state = 'available' AND queue = $1
    AND NOT EXISTS (SELECT 1 FROM caravan_queues q WHERE q.queue = $1 AND q.paused)
  ORDER BY priority, scheduled_at, id
  LIMIT $2
  FOR UPDATE SKIP LOCKED)
UPDATE caravan_jobs
SET state = 'executing', attempt = attempt + 1,
    attempted_at = to_timestamp($3::float8), attempted_by = $4
FROM claimed WHERE id = claimed.claimed_id
RETURNING %s|sql}
    columns

let fetch t ~now ~queue ~limit ~node =
  if limit <= 0 then []
  else
    query t ~params:[ p queue; p_int limit; p_time now; p node ] fetch_sql
    |> rows_of
    |> List.sort (fun (a : Row.t) (b : Row.t) ->
        compare
          (a.priority, Ptime.to_float_s a.scheduled_at, a.id)
          (b.priority, Ptime.to_float_s b.scheduled_at, b.id))

(* Apply [set] to the job iff it is still executing under [claim]. Parameters
   $1-$3 are the claim; [params] continue from $4. *)
let claimed t (claim : Row.claim) ~set params =
  let sql =
    Printf.sprintf
      "UPDATE caravan_jobs SET %s WHERE id = $1 AND state = 'executing' AND \
       attempted_by = $2 AND attempt = $3"
      set
  in
  let res =
    query t
      ~params:
        ([ p_id claim.claim_id; p claim.claim_node; p_int claim.claim_attempt ]
        @ params)
      sql
  in
  res.affected = 1

let error_array (e : Row.error) = p_json (`List [ Row.error_to_json e ])

let complete t ~now claim =
  claimed t claim
    ~set:"state = 'completed', finished_at = to_timestamp($4::float8)"
    [ p_time now ]

let retry t ~now:_ claim ~error ~at =
  claimed t claim
    ~set:
      "state = 'retryable', scheduled_at = to_timestamp($4::float8), errors = \
       errors || $5::jsonb"
    [ p_time at; error_array error ]

let discard t ~now claim ~error =
  claimed t claim
    ~set:
      "state = 'discarded', finished_at = to_timestamp($4::float8), errors = \
       errors || $5::jsonb"
    [ p_time now; error_array error ]

let snooze t ~now:_ claim ~at =
  claimed t claim
    ~set:
      "state = 'scheduled', scheduled_at = to_timestamp($4::float8), attempt = \
       attempt - 1"
    [ p_time at ]

let cancel_claimed t ~now claim ~error =
  claimed t claim
    ~set:
      "state = 'cancelled', finished_at = to_timestamp($4::float8), errors = \
       errors || $5::jsonb"
    [ p_time now; error_array error ]

let release t ~node =
  (query t
     ~params:[ p node ]
     "UPDATE caravan_jobs SET state = 'available', attempt = attempt - 1 WHERE \
      state = 'executing' AND attempted_by = $1")
    .affected

(* --- Operator actions ----------------------------------------------------- *)

let cancel t ~now id =
  (query t
     ~params:[ p_id id; p_time now ]
     "UPDATE caravan_jobs SET state = 'cancelled', finished_at = \
      to_timestamp($2::float8) WHERE id = $1 AND state IN ('available', \
      'scheduled', 'executing', 'retryable')")
    .affected = 1

let requeue t ~now id =
  match
    query t
      ~params:[ p_id id; p_time now ]
      {sql|UPDATE caravan_jobs j SET
  state = 'available', scheduled_at = to_timestamp($2::float8),
  finished_at = NULL, max_attempts = greatest(max_attempts, attempt + 1)
WHERE id = $1
  AND state IN ('completed', 'discarded', 'cancelled', 'retryable', 'scheduled')
  AND (unique_key IS NULL
       OR state IN ('retryable', 'scheduled')
       OR NOT EXISTS (
         SELECT 1 FROM caravan_jobs o
         WHERE o.unique_key = j.unique_key AND o.id <> j.id
           AND o.state IN ('available', 'scheduled', 'executing', 'retryable')))|sql}
  with
  | res -> res.affected = 1
  | exception Pg.Pg_error { sqlstate; _ }
    when sqlstate = Pg.sqlstate_unique_violation ->
      (* Lost a race with a concurrent insert of the same key. *)
      false

(* --- Maintenance ------------------------------------------------------------ *)

let stage t ~now =
  (query t
     ~params:[ p_time now ]
     "UPDATE caravan_jobs SET state = 'available' WHERE state IN ('scheduled', \
      'retryable') AND scheduled_at <= to_timestamp($1::float8)")
    .affected

let rescue t ~now ~nodes =
  if nodes = [] then 0
  else
    (query t
       ~params:
         [
           p_time now;
           p (rfc3339 now);
           p_json (`List (List.map (fun n -> `String n) nodes));
         ]
       {sql|UPDATE caravan_jobs SET
  state = CASE WHEN attempt < max_attempts THEN 'available' ELSE 'discarded' END,
  scheduled_at = CASE WHEN attempt < max_attempts
                      THEN to_timestamp($1::float8) ELSE scheduled_at END,
  finished_at = CASE WHEN attempt < max_attempts
                     THEN NULL ELSE to_timestamp($1::float8) END,
  errors = CASE WHEN attempt < max_attempts THEN errors
           ELSE errors || jsonb_build_array(jsonb_build_object(
             'attempt', attempt, 'at', $2::text,
             'message', 'node ' || attempted_by
                        || ' stopped heartbeating while executing this job'))
           END
WHERE state = 'executing'
  AND attempted_by IN (SELECT jsonb_array_elements_text($3::jsonb))|sql})
      .affected

let prune t ~before ~limit =
  (query t
     ~params:[ p_time before; p_int limit ]
     {sql|DELETE FROM caravan_jobs WHERE id IN (
  SELECT id FROM caravan_jobs
  WHERE state IN ('completed', 'discarded', 'cancelled')
    AND finished_at < to_timestamp($1::float8)
  LIMIT $2)|sql})
    .affected

(* --- Nodes and leadership --------------------------------------------------- *)

let node_info_to_json (n : Backend.node_info) =
  `Assoc
    [
      ("node", `String n.node);
      ( "queues",
        `List
          (List.map
             (fun (q, c) ->
               `Assoc [ ("queue", `String q); ("concurrency", `Int c) ])
             n.queues) );
      ("hostname", `String n.hostname);
      ("pid", `Int n.pid);
      ("started_at", `Float (Ptime.to_float_s n.started_at));
      ("heartbeat_at", `Float (Ptime.to_float_s n.heartbeat_at));
      ("running", `Int n.running);
    ]

let node_info_of_json j : Backend.node_info option =
  let open Yojson.Safe.Util in
  let num j =
    match j with `Float f -> f | `Int i -> float i | _ -> raise Exit
  in
  try
    Some
      {
        node = to_string (member "node" j);
        queues =
          List.map
            (fun q ->
              (to_string (member "queue" q), to_int (member "concurrency" q)))
            (to_list (member "queues" j));
        hostname = to_string (member "hostname" j);
        pid = to_int (member "pid" j);
        started_at = Option.get (Ptime.of_float_s (num (member "started_at" j)));
        heartbeat_at =
          Option.get (Ptime.of_float_s (num (member "heartbeat_at" j)));
        running = to_int (member "running" j);
      }
  with _ -> None

let heartbeat t (n : Backend.node_info) =
  ignore
    (query t
       ~params:[ p n.node; p_json (node_info_to_json n); p_time n.heartbeat_at ]
       "INSERT INTO caravan_nodes (node, info, heartbeat_at) VALUES ($1, \
        $2::jsonb, to_timestamp($3::float8)) ON CONFLICT (node) DO UPDATE SET \
        info = EXCLUDED.info, heartbeat_at = EXCLUDED.heartbeat_at")

let nodes t =
  (query t "SELECT info::text FROM caravan_nodes ORDER BY node").rows
  |> Array.to_list
  |> List.filter_map (fun r -> node_info_of_json (json (req r.(0))))

let remove_node t node =
  ignore
    (query t ~params:[ p node ] "DELETE FROM caravan_nodes WHERE node = $1")

let try_lead t ~now ~node ~ttl =
  let expires =
    Option.value ~default:now
      (Option.bind (Ptime.Span.of_float_s ttl) (Ptime.add_span now))
  in
  let res =
    query t
      ~params:[ p node; p_time expires; p_time now ]
      {sql|INSERT INTO caravan_leader (name, node, expires_at)
VALUES ('leader', $1, to_timestamp($2::float8))
ON CONFLICT (name) DO UPDATE
  SET node = EXCLUDED.node, expires_at = EXCLUDED.expires_at
  WHERE caravan_leader.node = EXCLUDED.node
     OR caravan_leader.expires_at <= to_timestamp($3::float8)
RETURNING node|sql}
  in
  Array.length res.rows = 1

let resign t ~node =
  ignore
    (query t
       ~params:[ p node ]
       "DELETE FROM caravan_leader WHERE name = 'leader' AND node = $1")

(* --- Waiting for work ------------------------------------------------------- *)

let take_pending t queues =
  Mutex.protect t.pending_mutex (fun () ->
      List.fold_left
        (fun found q ->
          if Hashtbl.mem t.pending q then (
            Hashtbl.remove t.pending q;
            true)
          else found)
        false queues)

let wait_for_jobs t ~queues ~timeout =
  if not (take_pending t queues) then
    t.with_timeout timeout (fun () ->
        Eio.Condition.loop_no_mutex t.wakeup (fun () ->
            if take_pending t queues then Some () else None))

let notify t queue =
  Mutex.protect t.pending_mutex (fun () -> Hashtbl.replace t.pending queue ());
  Eio.Condition.broadcast t.wakeup

(* Hold one connection in LISTEN mode and turn notifications into wakeups.
   Reconnects with backoff; producers fall back to polling meanwhile. *)
let rec listener t ~sleep ~conninfo delay =
  let connected = ref false in
  (try
     let conn = Pg.connect conninfo in
     Fun.protect
       ~finally:(fun () -> Pg.close conn)
       (fun () ->
         Pg.listen conn "caravan_jobs";
         connected := true;
         (* Jobs inserted while we were not listening are only found by
            polling; wake every waiter once so they look now. *)
         Eio.Condition.broadcast t.wakeup;
         let rec loop () =
           List.iter
             (fun (n : Postgresql.Notification.t) -> notify t n.extra)
             (Pg.await_notifications conn);
           loop ()
         in
         loop ())
   with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | e ->
      Log.warn (fun m ->
          m "LISTEN connection failed (%s); retrying in %.1fs"
            (Printexc.to_string e) delay));
  sleep delay;
  listener t ~sleep ~conninfo
    (if !connected then 0.5 else Float.min 30. (delay *. 2.))

(* --- Queries ------------------------------------------------------------------ *)

let get t id =
  match
    rows_of
      (query t
         ~params:[ p_id id ]
         (Printf.sprintf "SELECT %s FROM caravan_jobs WHERE id = $1" columns))
  with
  | r :: _ -> Some r
  | [] -> None

let list t (q : Backend.query) =
  let json_strings l = p_json (`List (List.map (fun s -> `String s) l)) in
  rows_of
    (query t
       ~params:
         [
           json_strings (List.map State.to_string q.states);
           json_strings q.queues;
           q.worker;
           p_opt p_id q.before_id;
           p_int q.limit;
         ]
       (Printf.sprintf
          {sql|SELECT %s FROM caravan_jobs
WHERE ($1::jsonb = '[]' OR state IN (SELECT jsonb_array_elements_text($1::jsonb)))
  AND ($2::jsonb = '[]' OR queue IN (SELECT jsonb_array_elements_text($2::jsonb)))
  AND ($3::text IS NULL OR worker = $3)
  AND ($4::bigint IS NULL OR id < $4)
ORDER BY id DESC
LIMIT $5|sql}
          columns))

let stats t : Backend.stats =
  let counts =
    (query t
       "SELECT queue, state, count(*) FROM caravan_jobs GROUP BY queue, state \
        ORDER BY queue, state")
      .rows |> Array.to_list
    |> List.map (fun r ->
        ( req r.(0),
          (match State.of_string (req r.(1)) with
          | Ok s -> s
          | Error e -> decode_error e),
          int_of_string (req r.(2)) ))
    |> List.sort compare
  in
  let paused =
    (query t "SELECT queue FROM caravan_queues WHERE paused ORDER BY queue")
      .rows |> Array.to_list
    |> List.map (fun r -> req r.(0))
  in
  { counts; paused }

let set_paused t ~queue paused =
  ignore
    (query t
       ~params:[ p queue; p (if paused then "true" else "false") ]
       "INSERT INTO caravan_queues (queue, paused) VALUES ($1, $2::boolean) ON \
        CONFLICT (queue) DO UPDATE SET paused = EXCLUDED.paused");
  if not paused then
    (* Wake producers of this queue on every node. *)
    ignore (query t ~params:[ p queue ] "SELECT pg_notify('caravan_jobs', $1)")

(* --- Construction ------------------------------------------------------------- *)

module Self = struct
  type nonrec t = t

  let name = name
  let insert = insert
  let fetch = fetch
  let complete = complete
  let retry = retry
  let discard = discard
  let snooze = snooze
  let cancel_claimed = cancel_claimed
  let release = release
  let cancel = cancel
  let requeue = requeue
  let stage = stage
  let rescue = rescue
  let prune = prune
  let heartbeat = heartbeat
  let nodes = nodes
  let remove_node = remove_node
  let try_lead = try_lead
  let resign = resign
  let wait_for_jobs = wait_for_jobs
  let get = get
  let list = list
  let stats = stats
  let set_paused = set_paused
end

let backend t = Backend.pack (module Self) t

let connect ~sw ~clock ?(pool_size = 10) ?(listen = true) conninfo =
  let t =
    {
      pool = Pg.pool ~size:pool_size conninfo;
      pending = Hashtbl.create 8;
      pending_mutex = Mutex.create ();
      wakeup = Eio.Condition.create ();
      with_timeout =
        (fun secs f ->
          match Eio.Time.with_timeout clock secs (fun () -> Ok (f ())) with
          | Ok () | Error `Timeout -> ());
    }
  in
  (* Fail fast on a bad connection string. *)
  use t (fun _ -> ());
  if listen then
    Eio.Fiber.fork_daemon ~sw (fun () ->
        listener t ~sleep:(Eio.Time.sleep clock) ~conninfo 0.5);
  Eio.Switch.on_release sw (fun () -> Pg.close_pool t.pool);
  t

let migrate t = use t Schema.migrate
let schema_version t = use t Schema.current_version
let latest_schema_version = Schema.latest

let with_transaction t f =
  use t (fun c -> Pg.with_transaction c (fun () -> f c))

let enqueue_in conn ?(now = Ptime_clock.now ()) job ?queue ?priority
    ?max_attempts ?delay ?at ?meta ?tags ?unique_key args =
  let scheduled_at =
    match (at, delay) with
    | Some at, _ -> Some at
    | None, Some d when d > 0. ->
        Option.bind (Ptime.Span.of_float_s d) (Ptime.add_span now)
    | None, _ -> None
  in
  let insert =
    Job.to_insert job ?queue ?priority ?max_attempts ?scheduled_at ?meta ?tags
      ?unique_key args
  in
  match insert_in conn ~now [ insert ] with [ r ] -> r | _ -> assert false

let truncate_all t =
  use t (fun c ->
      Pg.exec c
        "TRUNCATE caravan_jobs, caravan_nodes, caravan_queues, caravan_leader \
         RESTART IDENTITY")
