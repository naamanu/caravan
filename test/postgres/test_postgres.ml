(* PostgreSQL backend tests. Skipped unless CARAVAN_PG_URL is set, e.g.

     CARAVAN_PG_URL=postgresql://caravan@localhost:54329/caravan_test dune test

   The database is truncated repeatedly: never point this at real data. *)

open Caravan

let url = Sys.getenv_opt "CARAVAN_PG_URL"

(* --- Model-based testing: Postgres must behave exactly like Memory_backend -- *)

type op =
  | Insert of {
      queue : string;
      priority : int;
      delay : int;
      unique : string option;
      max_attempts : int;
    }
  | Insert_batch of int
  | Fetch of { queue : string; limit : int; node : string }
  | Complete of int  (** Index into the claims seen so far. *)
  | Retry of int * int
  | Discard of int
  | Snooze of int * int
  | Cancel_claimed of int
  | Release of string
  | Cancel of int
  | Requeue of int
  | Stage
  | Rescue of string list
  | Prune of int
  | Advance of int
  | Pause of string * bool
  | Lead of string

let pp_op ppf = function
  | Insert { queue; priority; delay; unique; max_attempts } ->
      Fmt.pf ppf "insert(%s p%d +%ds %a max%d)" queue priority delay
        Fmt.(option string) unique max_attempts
  | Insert_batch n -> Fmt.pf ppf "insert_batch(%d)" n
  | Fetch { queue; limit; node } -> Fmt.pf ppf "fetch(%s,%d,%s)" queue limit node
  | Complete i -> Fmt.pf ppf "complete(%d)" i
  | Retry (i, d) -> Fmt.pf ppf "retry(%d,+%d)" i d
  | Discard i -> Fmt.pf ppf "discard(%d)" i
  | Snooze (i, d) -> Fmt.pf ppf "snooze(%d,+%d)" i d
  | Cancel_claimed i -> Fmt.pf ppf "cancel_claimed(%d)" i
  | Release n -> Fmt.pf ppf "release(%s)" n
  | Cancel id -> Fmt.pf ppf "cancel(%d)" id
  | Requeue id -> Fmt.pf ppf "requeue(%d)" id
  | Stage -> Fmt.string ppf "stage"
  | Rescue ns -> Fmt.pf ppf "rescue(%a)" Fmt.(list ~sep:comma string) ns
  | Prune age -> Fmt.pf ppf "prune(%d)" age
  | Advance s -> Fmt.pf ppf "advance(%d)" s
  | Pause (q, b) -> Fmt.pf ppf "pause(%s,%b)" q b
  | Lead n -> Fmt.pf ppf "lead(%s)" n

let gen_op =
  let open QCheck.Gen in
  let queue = oneof_list [ "q1"; "q2" ] in
  let node = oneof_list [ "a"; "b"; "c" ] in
  let idx = int_range 0 20 in
  let id = int_range 1 25 in
  oneof_weighted
    [
      ( 6,
        map
          (fun (queue, priority, delay, unique, max_attempts) ->
            Insert { queue; priority; delay; unique; max_attempts })
          (tup5 queue (int_range 0 2)
             (oneof_list [ 0; 0; 5; 30 ])
             (option ~ratio:0.3 (oneof_list [ "k1"; "k2" ]))
             (int_range 1 3)) );
      (1, map (fun n -> Insert_batch n) (int_range 1 4));
      ( 5,
        map
          (fun (queue, limit, node) -> Fetch { queue; limit; node })
          (triple queue (int_range 1 3) node) );
      (3, map (fun i -> Complete i) idx);
      (2, map2 (fun i d -> Retry (i, d)) idx (int_range 1 20));
      (1, map (fun i -> Discard i) idx);
      (1, map2 (fun i d -> Snooze (i, d)) idx (int_range 1 20));
      (1, map (fun i -> Cancel_claimed i) idx);
      (1, map (fun n -> Release n) node);
      (1, map (fun i -> Cancel i) id);
      (1, map (fun i -> Requeue i) id);
      (2, return Stage);
      (1, map (fun ns -> Rescue ns) (list_size (int_range 0 2) node));
      (1, map (fun a -> Prune a) (oneof_list [ 0; 10; 100 ]));
      (2, map (fun s -> Advance s) (oneof_list [ 1; 10; 60 ]));
      (1, map2 (fun q b -> Pause (q, b)) queue bool);
      (1, map (fun n -> Lead n) node);
    ]

let arb_ops =
  QCheck.make
    ~print:(fun ops -> Fmt.str "%a" Fmt.(list ~sep:(any ";@ ") pp_op) ops)
    ~shrink:QCheck.Shrink.list
    QCheck.Gen.(list_size (int_range 1 60) gen_op)

(* Observable result of one operation. *)
type obs =
  | Ids of int64 list
  | Inserted of (bool * int64) list
  | Bool of bool
  | Int of int
  | Unit

let pp_obs ppf = function
  | Ids l -> Fmt.pf ppf "ids %a" Fmt.(Dump.list int64) l
  | Inserted l ->
      Fmt.pf ppf "inserted %a" Fmt.(Dump.list (Dump.pair bool int64)) l
  | Bool b -> Fmt.pf ppf "%b" b
  | Int i -> Fmt.pf ppf "%d" i
  | Unit -> Fmt.string ppf "()"

(* Everything about a row that a user could observe, with times as floats. *)
let snapshot (r : Row.t) =
  let t = Ptime.to_float_s in
  ( (r.id, State.to_string r.state, r.queue, r.worker, Yojson.Safe.to_string r.args),
    (r.priority, r.attempt, r.max_attempts, r.unique_key, r.attempted_by),
    ( t r.scheduled_at,
      Option.map t r.attempted_at,
      Option.map t r.finished_at,
      List.map (fun (e : Row.error) -> (e.attempt, t e.at, e.message)) r.errors ) )

module Run (B : Backend.S) = struct
  let step b ~now ~claims op =
    let claim i =
      match !claims with
      | [] -> None
      | cs -> Some (List.nth cs (i mod List.length cs))
    in
    let error i message : Row.error = { attempt = i; at = !now; message } in
    let at d = Option.get (Ptime.add_span !now (Ptime.Span.of_int_s d)) in
    let insert ~queue ~priority ~delay ~unique ~max_attempts : Row.insert =
      {
        i_queue = queue;
        i_worker = "w";
        i_args = `Assoc [ ("n", `Int delay) ];
        i_meta = `Null;
        i_tags = [ "t" ];
        i_priority = priority;
        i_max_attempts = max_attempts;
        i_scheduled_at = (if delay = 0 then None else Some (at delay));
        i_unique_key = unique;
      }
    in
    let inserted results =
      Inserted
        (List.map
           (function
             | Row.Inserted r -> (true, r.id) | Duplicate r -> (false, r.id))
           results)
    in
    match op with
    | Insert { queue; priority; delay; unique; max_attempts } ->
        inserted
          (B.insert b ~now:!now
             [ insert ~queue ~priority ~delay ~unique ~max_attempts ])
    | Insert_batch n ->
        inserted
          (B.insert b ~now:!now
             (List.init n (fun i ->
                  insert ~queue:"q1" ~priority:(i mod 2) ~delay:0
                    ~unique:(if i = 0 then Some "k1" else None)
                    ~max_attempts:2)))
    | Fetch { queue; limit; node } ->
        let rows = B.fetch b ~now:!now ~queue ~limit ~node in
        claims := !claims @ List.map Row.claim_of rows;
        Ids (List.map (fun (r : Row.t) -> r.id) rows)
    | Complete i -> (
        match claim i with
        | None -> Unit
        | Some c -> Bool (B.complete b ~now:!now c))
    | Retry (i, d) -> (
        match claim i with
        | None -> Unit
        | Some c -> Bool (B.retry b ~now:!now c ~error:(error c.claim_attempt "r") ~at:(at d)))
    | Discard i -> (
        match claim i with
        | None -> Unit
        | Some c -> Bool (B.discard b ~now:!now c ~error:(error c.claim_attempt "d")))
    | Snooze (i, d) -> (
        match claim i with
        | None -> Unit
        | Some c -> Bool (B.snooze b ~now:!now c ~at:(at d)))
    | Cancel_claimed i -> (
        match claim i with
        | None -> Unit
        | Some c ->
            Bool (B.cancel_claimed b ~now:!now c ~error:(error c.claim_attempt "c")))
    | Release node -> Int (B.release b ~node)
    | Cancel id -> Bool (B.cancel b ~now:!now (Int64.of_int id))
    | Requeue id -> Bool (B.requeue b ~now:!now (Int64.of_int id))
    | Stage -> Int (B.stage b ~now:!now)
    | Rescue nodes -> Int (B.rescue b ~now:!now ~nodes)
    | Prune age ->
        Int
          (B.prune b
             ~before:(Option.get (Ptime.sub_span !now (Ptime.Span.of_int_s age)))
             ~limit:1000)
    | Advance s ->
        now := at s;
        Unit
    | Pause (queue, paused) ->
        B.set_paused b ~queue paused;
        Unit
    | Lead node -> Bool (B.try_lead b ~now:!now ~node ~ttl:30.)

  let all b =
    List.rev (B.list b (Backend.query ~limit:1000 ()))
end

module M = Run (Memory_backend)
module P = Run (Caravan_postgres)

let model_test ~clock pg =
  QCheck.Test.make ~count:150 ~name:"postgres matches the memory model" arb_ops
    (fun ops ->
      Caravan_postgres.truncate_all pg;
      let mem = Memory_backend.create ~clock () in
      let start = Option.get (Ptime.of_float_s 1_800_000_000.) in
      let mnow = ref start and pnow = ref start in
      let mclaims = ref [] and pclaims = ref [] in
      List.iteri
        (fun i op ->
          let mo = M.step mem ~now:mnow ~claims:mclaims op in
          let po = P.step pg ~now:pnow ~claims:pclaims op in
          if mo <> po then
            QCheck.Test.fail_reportf "step %d (%a): memory %a, postgres %a" i
              pp_op op pp_obs mo pp_obs po;
          let ms = List.map snapshot (M.all mem)
          and ps = List.map snapshot (P.all pg) in
          if ms <> ps then
            QCheck.Test.fail_reportf
              "step %d (%a): job tables differ@.memory:   %a@.postgres: %a" i
              pp_op op
              Fmt.(Dump.list Row.pp) (M.all mem)
              Fmt.(Dump.list Row.pp) (P.all pg);
          if Memory_backend.stats mem <> Caravan_postgres.stats pg then
            QCheck.Test.fail_reportf "step %d (%a): stats differ" i pp_op op)
        ops;
      true)

(* --- Targeted tests ------------------------------------------------------ *)

let now () = Ptime_clock.now ()

let counter_job ~clock:_ counts =
  Job.make ~name:"count" ~codec:Codec.int
    ~perform:(fun _ n ->
      Mutex.protect (fst counts) (fun () ->
          let tbl = snd counts in
          Hashtbl.replace tbl n (1 + Option.value (Hashtbl.find_opt tbl n) ~default:0));
      Outcome.Ok)
    ()

let test_concurrent_fetch ~sw ~clock pg () =
  ignore sw;
  ignore clock;
  Caravan_postgres.truncate_all pg;
  let job = Job.make ~name:"x" ~codec:Codec.int ~perform:(fun _ _ -> Outcome.Ok) () in
  let n = 2000 in
  ignore
    (Caravan_postgres.insert pg ~now:(now ()) (List.init n (fun i -> Job.to_insert job i)));
  (* 16 fibers, each on its own pooled connection, race to claim. *)
  let claimed = ref [] in
  Eio.Fiber.all
    (List.init 16 (fun w () ->
         let rec go () =
           match
             Caravan_postgres.fetch pg ~now:(now ()) ~queue:"default" ~limit:7
               ~node:(Printf.sprintf "w%d" w)
           with
           | [] -> ()
           | rows ->
               claimed := List.map (fun (r : Row.t) -> r.id) rows @ !claimed;
               go ()
         in
         go ()));
  let unique = List.sort_uniq compare !claimed in
  Alcotest.(check int) "every job claimed" n (List.length !claimed);
  Alcotest.(check int) "no job claimed twice" n (List.length unique)

let test_listen_wakeup ~sw ~clock pg () =
  Caravan_postgres.truncate_all pg;
  let ran = Eio.Promise.create () in
  let job =
    Job.make ~name:"ping" ~codec:Codec.unit
      ~perform:(fun _ () ->
        ignore (Eio.Promise.try_resolve (snd ran) (Eio.Time.now clock));
        Outcome.Ok)
      ()
  in
  let config = { (Node.default_config ()) with node_id = "listen"; poll_interval = 30. } in
  let node =
    Node.start ~sw ~clock ~config (Caravan_postgres.backend pg) ~jobs:[ Job.pack job ]
      ~queues:[ ("default", 1) ]
  in
  (* Let the producer go idle in wait_for_jobs, then enqueue. *)
  Eio.Time.sleep clock 0.5;
  let client = Client.make (Caravan_postgres.backend pg) in
  let t0 = Eio.Time.now clock in
  ignore (Client.enqueue client job ());
  let t1 = Eio.Promise.await (fst ran) in
  Node.stop node;
  Alcotest.(check bool)
    (Printf.sprintf "picked up in %.3fs, well under the 30s poll" (t1 -. t0))
    true
    (t1 -. t0 < 1.0)

let test_transactional_enqueue ~sw:_ ~clock:_ pg () =
  Caravan_postgres.truncate_all pg;
  let job = Job.make ~name:"tx" ~codec:Codec.int ~perform:(fun _ _ -> Outcome.Ok) () in
  (try
     Caravan_postgres.with_transaction pg (fun conn ->
         ignore (Caravan_postgres.enqueue_in conn job 1);
         failwith "rollback")
   with Failure _ -> ());
  Caravan_postgres.with_transaction pg (fun conn ->
      ignore (Caravan_postgres.enqueue_in conn job 2));
  let rows = Caravan_postgres.list pg (Backend.query ()) in
  Alcotest.(check (list string)) "only the committed job exists" [ "2" ]
    (List.map (fun (r : Row.t) -> Yojson.Safe.to_string r.args) rows)

let test_migrate_idempotent ~sw:_ ~clock:_ pg () =
  Alcotest.(check (list int)) "nothing left to apply" [] (Caravan_postgres.migrate pg);
  Alcotest.(check int) "at latest version" Caravan_postgres.latest_schema_version
    (Caravan_postgres.schema_version pg)

let test_cluster_on_postgres ~sw ~clock pg () =
  Caravan_postgres.truncate_all pg;
  let counts = (Mutex.create (), Hashtbl.create 1000) in
  let job = counter_job ~clock counts in
  let n = 1000 in
  let client = Client.make (Caravan_postgres.backend pg) in
  ignore (Client.enqueue_many client (List.init n (fun i -> Job.to_insert job i)));
  let nodes =
    List.init 3 (fun i ->
        Node.start ~sw ~clock
          ~config:{ (Node.default_config ()) with node_id = Printf.sprintf "pg%d" i }
          (Caravan_postgres.backend pg) ~jobs:[ Job.pack job ]
          ~queues:[ ("default", 10) ])
  in
  let deadline = Eio.Time.now clock +. 60. in
  let rec wait () =
    let completed =
      List.assoc_opt ("default", State.Completed)
        (List.map (fun (q, s, c) -> ((q, s), c)) (Client.stats client).counts)
    in
    if completed <> Some n then
      if Eio.Time.now clock > deadline then Alcotest.fail "timed out"
      else (
        Eio.Time.sleep clock 0.1;
        wait ())
  in
  wait ();
  List.iter Node.stop nodes;
  let tbl = snd counts in
  Alcotest.(check int) "all ran" n (Hashtbl.length tbl);
  Alcotest.(check bool) "each exactly once" true
    (Hashtbl.fold (fun _ c ok -> ok && c = 1) tbl true);
  Alcotest.(check int) "nodes deregistered" 0 (List.length (Client.nodes client))

let () =
  match url with
  | None ->
      print_endline "CARAVAN_PG_URL not set; skipping PostgreSQL tests."
  | Some url ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let clock = env#clock in
      let pg = Caravan_postgres.connect ~sw ~clock ~pool_size:20 url in
      Caravan_postgres.Pg.use
        (Caravan_postgres.Pg.pool ~size:1 url)
        (fun c ->
          Caravan_postgres.Pg.exec c
            "DROP TABLE IF EXISTS caravan_jobs, caravan_nodes, caravan_queues, \
             caravan_leader, caravan_migrations CASCADE; DROP FUNCTION IF \
             EXISTS caravan_notify() CASCADE");
      Alcotest.(check (list int)) "fresh migration" [ 1 ] (Caravan_postgres.migrate pg);
      let tc name f = Alcotest.test_case name `Quick (f ~sw ~clock pg) in
      Alcotest.run ~and_exit:false "caravan-postgres"
        [
          ("model", [ QCheck_alcotest.to_alcotest (model_test ~clock pg) ]);
          ( "backend",
            [
              tc "migrations are idempotent" test_migrate_idempotent;
              tc "concurrent fetch never double-claims" test_concurrent_fetch;
              tc "transactional enqueue" test_transactional_enqueue;
            ] );
          ( "runtime",
            [
              tc "LISTEN/NOTIFY wakes idle producers" test_listen_wakeup;
              tc "three nodes, exactly once" test_cluster_on_postgres;
            ] );
        ]
