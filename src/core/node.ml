let src = Logs.Src.create "caravan.node" ~doc:"Caravan worker node"

module Log = (val Logs.src_log src)

type cron = Cron_entry : { expr : Cron.t; job : 'a Job.t; args : 'a } -> cron

let cron expr job args = Cron_entry { expr = Cron.parse_exn expr; job; args }

type config = {
  node_id : string;
  poll_interval : float;
  heartbeat_interval : float;
  node_timeout : float;
  leader_ttl : float;
  maintenance_interval : float;
  rescue_interval : float;
  prune_interval : float;
  prune_after : float option;
  prune_batch : int;
  shutdown_grace : float;
}

let default_node_id () =
  let host = try Unix.gethostname () with _ -> "localhost" in
  (* Self-seeded: the global generator starts from the same seed in every
     process, which would give containers (all pid 1) identical ids. *)
  let rng = Random.State.make_self_init () in
  Printf.sprintf "%s-%d-%06x" host (Unix.getpid ())
    (Random.State.bits rng land 0xffffff)

let default_config () =
  {
    node_id = default_node_id ();
    poll_interval = 1.0;
    heartbeat_interval = 5.0;
    node_timeout = 60.0;
    leader_ttl = 15.0;
    maintenance_interval = 1.0;
    rescue_interval = 15.0;
    prune_interval = 60.0;
    prune_after = Some (7. *. 86_400.);
    prune_batch = 10_000;
    shutdown_grace = 30.0;
  }

(* Counts running jobs and lets a producer wait for a free slot. *)
module Slots = struct
  type t = { limit : int; used : int Atomic.t; freed : Eio.Condition.t }

  let create limit =
    { limit; used = Atomic.make 0; freed = Eio.Condition.create () }

  let free t = t.limit - Atomic.get t.used

  let await_free t =
    Eio.Condition.loop_no_mutex t.freed (fun () ->
        if free t > 0 then Some () else None)

  let take t = Atomic.incr t.used

  let release t =
    Atomic.decr t.used;
    Eio.Condition.broadcast t.freed
end

type t = {
  config : config;
  backend : Backend.t;
  registry : Registry.t;
  queues : (string * int) list;
  middleware : Middleware.t list;
  telemetry : Telemetry.t;
  cron : cron list;
  executor : (Eio.Executor_pool.t * string list) option;
  now : unit -> Ptime.t;
  sleep : float -> unit;
  with_timeout : 'a. float -> (unit -> 'a) -> 'a option;
  started_at : Ptime.t;
  total : Slots.t; (* all queues, for reporting *)
  leader : bool Atomic.t;
  stop_requested : unit Eio.Promise.t * unit Eio.Promise.u;
  finished : unit Eio.Promise.t * unit Eio.Promise.u;
}

exception Shutdown_timeout

let id t = t.config.node_id
let running t = Atomic.get t.total.used
let is_leader t = Atomic.get t.leader

let add_seconds time s =
  match Ptime.Span.of_float_s s with
  | None -> time
  | Some span -> Option.value (Ptime.add_span time span) ~default:time

let describe_exn e = match e with Failure m -> m | e -> Printexc.to_string e

(* Run a backend operation; on failure report it and return [None]. *)
let guard t operation f =
  match f () with
  | v -> Some v
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e ->
      Telemetry.emit t.telemetry
        (Backend_error { node = id t; operation; error = describe_exn e });
      None

(* Retry an operation until it succeeds, backing off up to 30s. Only
   cancellation (i.e. shutdown) interrupts it. *)
let persist t operation f =
  let rec go delay =
    match guard t operation f with
    | Some v -> v
    | None ->
        t.sleep delay;
        go (Float.min 30. (delay *. 2.))
  in
  go 0.25

(* --- Executing one job -------------------------------------------------- *)

let error_of t (row : Row.t) message : Row.error =
  { attempt = row.attempt; at = t.now (); message }

let run_job t ~queue (row : Row.t) : Outcome.t =
  match Registry.resolve t.registry row with
  | Error outcome -> outcome
  | Ok resolved -> (
      let ctx = Ctx.make ~row ~node:(id t) in
      let perform () =
        match t.executor with
        | Some (pool, cpu_queues) when List.mem queue cpu_queues ->
            Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
                resolved.run ctx)
        | _ -> resolved.run ctx
      in
      let guarded () =
        match Middleware.apply t.middleware ctx perform with
        | outcome -> outcome
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            Outcome.Error
              (Printexc.to_string e ^ "\n" ^ Printexc.raw_backtrace_to_string bt)
      in
      match resolved.timeout with
      | None -> guarded ()
      | Some secs -> (
          match t.with_timeout secs guarded with
          | Some outcome -> outcome
          | None -> Outcome.Error (Printf.sprintf "timed out after %gs" secs)))

let backoff_for t (row : Row.t) =
  match Registry.resolve t.registry row with
  | Ok r -> r.backoff
  | Error _ -> Backoff.default

let record t (row : Row.t) (outcome : Outcome.t) =
  let (Backend.Backend ((module B), b)) = t.backend in
  let claim = Row.claim_of row in
  persist t "record outcome" (fun () ->
      let now = t.now () in
      match outcome with
      | Ok -> B.complete b ~now claim
      | Error message ->
          let error = error_of t row message in
          if row.attempt >= row.max_attempts then B.discard b ~now claim ~error
          else
            let delay =
              Backoff.delay (backoff_for t row) ~attempt:row.attempt
            in
            B.retry b ~now claim ~error ~at:(add_seconds now delay)
      | Snooze secs -> B.snooze b ~now claim ~at:(add_seconds now secs)
      | Cancel reason ->
          B.cancel_claimed b ~now claim ~error:(error_of t row reason)
      | Discard reason -> B.discard b ~now claim ~error:(error_of t row reason))

let execute t ~queue (row : Row.t) =
  Telemetry.emit t.telemetry (Job_started { node = id t; row });
  let started = Mtime_clock.counter () in
  let outcome = run_job t ~queue row in
  let duration = Mtime.Span.to_float_ns (Mtime_clock.count started) /. 1e9 in
  let recorded = record t row outcome in
  if not recorded then
    Log.warn (fun m ->
        m "outcome of %a ignored: the job was cancelled or rescued meanwhile"
          Row.pp row);
  Telemetry.emit t.telemetry
    (Job_finished { node = id t; row; outcome; duration; recorded })

(* --- Producers ---------------------------------------------------------- *)

let producer t ~jobs_sw (queue, concurrency) =
  let (Backend.Backend ((module B), b)) = t.backend in
  let slots = Slots.create concurrency in
  let rec loop failures =
    Slots.await_free slots;
    let limit = Slots.free slots in
    match
      guard t "fetch" (fun () ->
          B.fetch b ~now:(t.now ()) ~queue ~limit ~node:(id t))
    with
    | None ->
        t.sleep
          (Float.min 30. (t.config.poll_interval *. (2. ** float failures)));
        loop (failures + 1)
    | Some [] ->
        ignore
          (guard t "wait" (fun () ->
               B.wait_for_jobs b ~queues:[ queue ]
                 ~timeout:t.config.poll_interval));
        loop 0
    | Some rows ->
        List.iter
          (fun row ->
            Slots.take slots;
            Slots.take t.total;
            Eio.Fiber.fork ~sw:jobs_sw (fun () ->
                Fun.protect
                  ~finally:(fun () ->
                    Slots.release slots;
                    Slots.release t.total)
                  (fun () -> execute t ~queue row)))
          rows;
        loop 0
  in
  loop 0

(* --- Heartbeat and leadership ------------------------------------------- *)

let node_info t : Backend.node_info =
  {
    node = id t;
    queues = t.queues;
    hostname = (try Unix.gethostname () with _ -> "");
    pid = Unix.getpid ();
    started_at = t.started_at;
    heartbeat_at = t.now ();
    running = running t;
  }

let heartbeat t =
  let (Backend.Backend ((module B), b)) = t.backend in
  ignore (guard t "heartbeat" (fun () -> B.heartbeat b (node_info t)))

let rec heartbeat_loop t () =
  t.sleep t.config.heartbeat_interval;
  heartbeat t;
  heartbeat_loop t ()

let maintenance t ~task count =
  if count > 0 then
    Telemetry.emit t.telemetry (Maintenance { node = id t; task; count })

(* The latest firing time of [expr] in (after, now], if any. *)
let latest_slot expr ~after ~now =
  let rec go last candidate =
    match Cron.next expr ~after:candidate with
    | Some slot when not (Ptime.is_later slot ~than:now) -> go (Some slot) slot
    | _ -> last
  in
  go None after

let leader_loop t () =
  let (Backend.Backend ((module B), b)) = t.backend in
  let lead_every = Float.max 0.1 (t.config.leader_ttl /. 3.) in
  let last_lead_attempt = ref neg_infinity in
  let last_rescue = ref neg_infinity in
  let last_prune = ref neg_infinity in
  let cron_cursors = Array.make (List.length t.cron) Ptime.epoch in
  let tick () =
    let now = t.now () in
    let elapsed = Ptime.Span.to_float_s (Ptime.diff now t.started_at) in
    if elapsed -. !last_lead_attempt >= lead_every then begin
      last_lead_attempt := elapsed;
      let leader =
        Option.value ~default:false
          (guard t "leadership" (fun () ->
               B.try_lead b ~now ~node:(id t) ~ttl:t.config.leader_ttl))
      in
      let was = Atomic.exchange t.leader leader in
      if leader <> was then begin
        Telemetry.emit t.telemetry (Leadership { node = id t; leader });
        (* A new leader only fires cron slots after it took over; earlier
           slots belonged to the previous leader. *)
        if leader then Array.fill cron_cursors 0 (Array.length cron_cursors) now
      end
    end;
    if is_leader t then begin
      Option.iter
        (maintenance t ~task:"stage")
        (guard t "stage" (fun () -> B.stage b ~now));
      if elapsed -. !last_rescue >= t.config.rescue_interval then begin
        last_rescue := elapsed;
        let cutoff = add_seconds now (-.t.config.node_timeout) in
        Option.iter
          (fun nodes ->
            let dead =
              List.filter_map
                (fun (n : Backend.node_info) ->
                  if
                    n.node <> id t
                    && Ptime.is_earlier n.heartbeat_at ~than:cutoff
                  then Some n.node
                  else None)
                nodes
            in
            if dead <> [] then
              Option.iter
                (fun count ->
                  maintenance t ~task:"rescue" count;
                  List.iter
                    (fun n ->
                      Log.warn (fun m ->
                          m "node %s presumed dead; rescued its jobs" n);
                      ignore
                        (guard t "remove node" (fun () -> B.remove_node b n)))
                    dead)
                (guard t "rescue" (fun () -> B.rescue b ~now ~nodes:dead)))
          (guard t "list nodes" (fun () -> B.nodes b))
      end;
      (match t.config.prune_after with
      | Some age when elapsed -. !last_prune >= t.config.prune_interval ->
          last_prune := elapsed;
          Option.iter
            (maintenance t ~task:"prune")
            (guard t "prune" (fun () ->
                 B.prune b ~before:(add_seconds now (-.age))
                   ~limit:t.config.prune_batch))
      | _ -> ());
      List.iteri
        (fun i (Cron_entry { expr; job; args }) ->
          match latest_slot expr ~after:cron_cursors.(i) ~now with
          | None -> ()
          | Some slot ->
              let key =
                Printf.sprintf "cron:%s@%s" (Cron.to_string expr)
                  (Ptime.to_rfc3339 ~tz_offset_s:0 slot)
              in
              let insert = Job.to_insert job ~unique_key:key args in
              Option.iter
                (fun results ->
                  cron_cursors.(i) <- slot;
                  maintenance t ~task:"cron"
                    (List.length
                       (List.filter
                          (function
                            | Row.Inserted _ -> true | Duplicate _ -> false)
                          results)))
                (guard t "cron insert" (fun () -> B.insert b ~now [ insert ])))
        t.cron
    end
  in
  let rec loop () =
    tick ();
    t.sleep t.config.maintenance_interval;
    loop ()
  in
  loop ()

(* --- Lifecycle ---------------------------------------------------------- *)

let shutdown_cleanup t =
  let (Backend.Backend ((module B), b)) = t.backend in
  (* Cleanup must run even if the node's fiber is being cancelled. *)
  Eio.Cancel.protect (fun () ->
      let released =
        Option.value ~default:0
          (t.with_timeout 10. (fun () ->
               Option.value ~default:0
                 (guard t "release" (fun () -> B.release b ~node:(id t)))))
      in
      ignore
        (t.with_timeout 5. (fun () ->
             ignore (guard t "remove node" (fun () -> B.remove_node b (id t)));
             if is_leader t then
               ignore (guard t "resign" (fun () -> B.resign b ~node:(id t)))));
      Atomic.set t.leader false;
      Telemetry.emit t.telemetry (Node_stopped { node = id t; released }))

let main t () =
  Fun.protect
    ~finally:(fun () -> Eio.Promise.resolve (snd t.finished) ())
    (fun () ->
      Fun.protect ~finally:(fun () -> shutdown_cleanup t) @@ fun () ->
      Eio.Switch.run @@ fun node_sw ->
      heartbeat t;
      Telemetry.emit t.telemetry (Node_started { node = id t });
      Eio.Fiber.fork_daemon ~sw:node_sw (heartbeat_loop t);
      Eio.Fiber.fork_daemon ~sw:node_sw (leader_loop t);
      try
        Eio.Switch.run @@ fun jobs_sw ->
        Eio.Fiber.first
          (fun () -> Eio.Promise.await (fst t.stop_requested))
          (fun () ->
            Eio.Fiber.all
              (List.map (fun q () -> producer t ~jobs_sw q) t.queues));
        Log.info (fun m ->
            m "node %s stopping; waiting up to %gs for %d running jobs" (id t)
              t.config.shutdown_grace (running t));
        match
          t.with_timeout t.config.shutdown_grace (fun () ->
              Eio.Condition.loop_no_mutex t.total.freed (fun () ->
                  if running t = 0 then Some () else None))
        with
        | Some () -> ()
        | None -> Eio.Switch.fail jobs_sw Shutdown_timeout
      with Shutdown_timeout ->
        Log.warn (fun m ->
            m "node %s: shutdown grace period elapsed; cancelled running jobs"
              (id t)))

let start ~sw ~clock ?(config = default_config ()) ?(middleware = [])
    ?(telemetry = Telemetry.create ()) ?(cron = []) ?executor backend ~jobs
    ~queues =
  if queues = [] then invalid_arg "Node.start: no queues given";
  List.iter
    (fun (q, c) ->
      if c < 1 then
        invalid_arg
          (Printf.sprintf "Node.start: concurrency of %S must be >= 1" q))
    queues;
  let now () =
    match Ptime.of_float_s (Eio.Time.now clock) with
    | Some t -> t
    | None -> Ptime.epoch
  in
  let t =
    {
      config;
      backend;
      registry = Registry.create jobs;
      queues;
      middleware;
      telemetry;
      cron;
      executor;
      now;
      sleep = Eio.Time.sleep clock;
      with_timeout =
        (fun secs f ->
          match Eio.Time.with_timeout clock secs (fun () -> Ok (f ())) with
          | Ok v -> Some v
          | Error `Timeout -> None);
      started_at = now ();
      total = Slots.create max_int;
      leader = Atomic.make false;
      stop_requested = Eio.Promise.create ();
      finished = Eio.Promise.create ();
    }
  in
  Eio.Fiber.fork ~sw (main t);
  t

let stop t =
  ignore (Eio.Promise.try_resolve (snd t.stop_requested) ());
  Eio.Promise.await (fst t.finished)

let await t = Eio.Promise.await (fst t.finished)

let run_until_signal t =
  let signalled = Atomic.make false in
  let cond = Eio.Condition.create () in
  let handler =
    Sys.Signal_handle
      (fun _ ->
        Atomic.set signalled true;
        Eio.Condition.broadcast cond)
  in
  let old_int = Sys.signal Sys.sigint handler in
  let old_term = Sys.signal Sys.sigterm handler in
  Fun.protect
    ~finally:(fun () ->
      Sys.set_signal Sys.sigint old_int;
      Sys.set_signal Sys.sigterm old_term)
    (fun () ->
      Eio.Fiber.first
        (fun () ->
          Eio.Condition.loop_no_mutex cond (fun () ->
              if Atomic.get signalled then Some () else None);
          Log.info (fun m -> m "signal received; stopping node %s" (id t)))
        (fun () -> await t);
      stop t)
