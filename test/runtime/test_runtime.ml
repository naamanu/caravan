(* Runtime tests on Eio_mock: the clock is virtual and advances automatically
   whenever every fiber is blocked, so hours of retries run in milliseconds and
   every test is deterministic. *)

open Caravan

let state_t = Alcotest.testable State.pp State.equal

let config ?(node_id = "n1") () =
  {
    (Node.default_config ()) with
    node_id;
    poll_interval = 0.5;
    heartbeat_interval = 1.;
    node_timeout = 5.;
    leader_ttl = 3.;
    rescue_interval = 1.;
    shutdown_grace = 2.;
  }

(* Run [f] in a mock environment with a fresh memory backend. *)
let with_env f =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#clock in
  let mem = Memory_backend.create ~clock () in
  let now () = Option.get (Ptime.of_float_s (Eio.Time.now clock)) in
  let client = Client.make ~now (Memory_backend.backend mem) in
  Eio.Switch.run @@ fun sw -> f ~sw ~clock ~mem ~client

(* Sleep in virtual time until [p] holds, failing after [timeout] seconds. *)
let wait_until ~clock ?(timeout = 3600.) what p =
  let deadline = Eio.Time.now clock +. timeout in
  let rec go () =
    if not (p ()) then
      if Eio.Time.now clock > deadline then
        Alcotest.failf "timed out waiting for %s" what
      else (
        Eio.Time.sleep clock 0.1;
        go ())
  in
  go ()

let state_of mem id = (Option.get (Memory_backend.get mem id)).state
let id_of = function Row.Inserted r -> r.id | Duplicate r -> r.id

let job ?max_attempts ?timeout ?backoff ?(queue = "default") ?unique name
    perform =
  Job.make ~name ~codec:Codec.int ?max_attempts ?timeout ?backoff ~queue ?unique
    ~perform ()

let test_completes () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let seen = ref [] in
  let j =
    job "record" (fun _ n ->
        seen := n :: !seen;
        Outcome.Ok)
  in
  let ids = List.init 5 (fun i -> id_of (Client.enqueue client j i)) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 2) ]
  in
  wait_until ~clock "completion" (fun () ->
      List.for_all (fun id -> state_of mem id = Completed) ids);
  Node.stop node;
  Alcotest.(check (list int))
    "each ran once" [ 0; 1; 2; 3; 4 ] (List.sort compare !seen)

let test_retry_then_discard () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let attempts = ref [] in
  let j =
    job "flaky" ~max_attempts:3 ~backoff:(Backoff.Constant 10.) (fun ctx _ ->
        attempts := (Ctx.attempt ctx, Eio.Time.now clock) :: !attempts;
        failwith "boom")
  in
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "discard" (fun () -> state_of mem id = Discarded);
  Node.stop node;
  let row = Option.get (Memory_backend.get mem id) in
  Alcotest.(check int) "three attempts" 3 row.attempt;
  Alcotest.(check int) "three errors" 3 (List.length row.errors);
  Alcotest.(check bool)
    "error message kept" true
    (String.starts_with ~prefix:"Failure(\"boom\")" (List.hd row.errors).message);
  (* Each retry waits for the backoff (10s) plus up to one staging tick. *)
  match List.rev !attempts with
  | [ (1, t1); (2, t2); (3, t3) ] ->
      Alcotest.(check bool)
        "backoff respected" true
        (t2 -. t1 >= 10. && t3 -. t2 >= 10. && t3 -. t1 < 25.)
  | _ -> Alcotest.fail "expected attempts 1, 2, 3"

let test_timeout () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j =
    job "slow" ~max_attempts:1 ~timeout:2. (fun _ _ ->
        Eio.Time.sleep clock 100.;
        Outcome.Ok)
  in
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock ~timeout:50. "discard" (fun () ->
      state_of mem id = Discarded);
  Node.stop node;
  let row = Option.get (Memory_backend.get mem id) in
  Alcotest.(check string)
    "timeout recorded" "timed out after 2s" (List.hd row.errors).message

let test_snooze_and_outcomes () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j =
    job "outcomes" (fun ctx n ->
        match n with
        | 0 ->
            if
              Ctx.attempt ctx = 1
              && Ctx.errors ctx = []
              && Eio.Time.now clock < 5.
            then Outcome.Snooze 30.
            else Outcome.Ok
        | 1 -> Outcome.Cancel "not needed"
        | _ -> Outcome.Discard "bad input")
  in
  let snoozed = id_of (Client.enqueue client j 0) in
  let cancelled = id_of (Client.enqueue client j 1) in
  let discarded = id_of (Client.enqueue client j 2) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 3) ]
  in
  wait_until ~clock "all finished" (fun () ->
      List.for_all
        (fun id -> State.is_terminal (state_of mem id))
        [ snoozed; cancelled; discarded ]);
  Node.stop node;
  Alcotest.check state_t "snoozed then ok" Completed (state_of mem snoozed);
  Alcotest.(check int)
    "snooze did not use an attempt" 1
    (Option.get (Memory_backend.get mem snoozed)).attempt;
  Alcotest.check state_t "cancelled" Cancelled (state_of mem cancelled);
  Alcotest.check state_t "discarded" Discarded (state_of mem discarded)

let test_concurrency_limit () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let active = ref 0 and peak = ref 0 in
  let j =
    job "busy" (fun _ _ ->
        incr active;
        peak := max !peak !active;
        Eio.Time.sleep clock 1.;
        decr active;
        Outcome.Ok)
  in
  let ids = List.init 20 (fun i -> id_of (Client.enqueue client j i)) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 3) ]
  in
  wait_until ~clock "completion" (fun () ->
      List.for_all (fun id -> state_of mem id = Completed) ids);
  Node.stop node;
  Alcotest.(check int) "peak concurrency" 3 !peak

let test_priority_order () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let order = ref [] in
  let j =
    job "p" (fun _ n ->
        order := n :: !order;
        Outcome.Ok)
  in
  List.iter
    (fun (n, priority) -> ignore (Client.enqueue client j ~priority n))
    [ (1, 5); (2, 0); (3, 9); (4, 0); (5, 5) ];
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "all ran" (fun () -> List.length !order = 5);
  Node.stop node;
  Alcotest.(check (list int))
    "priority, then insertion" [ 2; 4; 1; 5; 3 ] (List.rev !order)

let test_scheduled () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let ran_at = ref None in
  let j =
    job "later" (fun _ _ ->
        ran_at := Some (Eio.Time.now clock);
        Outcome.Ok)
  in
  let id = id_of (Client.enqueue client j ~delay:60. 0) in
  Alcotest.check state_t "scheduled" Scheduled (state_of mem id);
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "ran" (fun () -> state_of mem id = Completed);
  Node.stop node;
  let t = Option.get !ran_at in
  Alcotest.(check bool) "not early, not much later" true (t >= 60. && t < 63.)

let test_graceful_shutdown_releases () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j =
    job "forever" (fun _ _ ->
        Eio.Time.sleep clock 1000.;
        Outcome.Ok)
  in
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "claimed" (fun () -> state_of mem id = Executing);
  let t0 = Eio.Time.now clock in
  Node.stop node;
  Alcotest.(check bool)
    "waited for the grace period" true
    (Eio.Time.now clock -. t0 >= 2.);
  let row = Option.get (Memory_backend.get mem id) in
  Alcotest.check state_t "released" Available row.state;
  Alcotest.(check int) "attempt not consumed" 0 row.attempt;
  Alcotest.(check int)
    "node deregistered" 0
    (List.length (Memory_backend.nodes mem))

let test_graceful_shutdown_waits () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j =
    job "short" (fun _ _ ->
        Eio.Time.sleep clock 1.;
        Outcome.Ok)
  in
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "claimed" (fun () -> state_of mem id = Executing);
  Node.stop node;
  Alcotest.check state_t "finished during grace" Completed (state_of mem id)

let test_rescue_dead_node () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j = job "work" (fun _ _ -> Outcome.Ok) in
  let id = id_of (Client.enqueue client j 0) in
  (* A node claims the job, heartbeats once, then vanishes. *)
  let now () = Option.get (Ptime.of_float_s (Eio.Time.now clock)) in
  ignore
    (Memory_backend.fetch mem ~now:(now ()) ~queue:"default" ~limit:1
       ~node:"ghost");
  Memory_backend.heartbeat mem
    {
      node = "ghost";
      queues = [];
      hostname = "x";
      pid = 1;
      started_at = now ();
      heartbeat_at = now ();
      running = 1;
    };
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "rescued and completed" (fun () ->
      state_of mem id = Completed);
  let rescued_at = Eio.Time.now clock in
  Node.stop node;
  let row = Option.get (Memory_backend.get mem id) in
  Alcotest.(check int) "the lost attempt counts" 2 row.attempt;
  Alcotest.(check bool) "only after the node timeout" true (rescued_at >= 5.);
  Alcotest.(check bool)
    "ghost deregistered" true
    (not
       (List.exists
          (fun (n : Backend.node_info) -> n.node = "ghost")
          (Memory_backend.nodes mem)))

let test_stale_outcome_ignored () =
  with_env @@ fun ~sw:_ ~clock ~mem ~client ->
  let j = job "x" (fun _ _ -> Outcome.Ok) in
  let id = id_of (Client.enqueue client j 0) in
  let now () = Option.get (Ptime.of_float_s (Eio.Time.now clock)) in
  let row =
    List.hd
      (Memory_backend.fetch mem ~now:(now ()) ~queue:"default" ~limit:1
         ~node:"a")
  in
  ignore (Memory_backend.rescue mem ~now:(now ()) ~nodes:[ "a" ]);
  let row2 =
    List.hd
      (Memory_backend.fetch mem ~now:(now ()) ~queue:"default" ~limit:1
         ~node:"b")
  in
  Alcotest.(check bool)
    "old claim rejected" false
    (Memory_backend.complete mem ~now:(now ()) (Row.claim_of row));
  Alcotest.(check bool)
    "new claim accepted" true
    (Memory_backend.complete mem ~now:(now ()) (Row.claim_of row2));
  Alcotest.check state_t "completed" Completed (state_of mem id)

let test_unique () =
  with_env @@ fun ~sw:_ ~clock:_ ~mem ~client ->
  let j = job "u" ~unique:Job.By_args (fun _ _ -> Outcome.Ok) in
  let a = Client.enqueue client j 1 in
  let b = Client.enqueue client j 1 in
  let c = Client.enqueue client j 2 in
  (match (a, b, c) with
  | Inserted a, Duplicate b, Inserted _ ->
      Alcotest.(check int64) "duplicate returns existing" a.id b.id
  | _ -> Alcotest.fail "expected Inserted, Duplicate, Inserted");
  (* Once the first finishes, the key is free again. *)
  ignore (Client.cancel client (id_of a));
  let second =
    match Client.enqueue client j 1 with
    | Inserted r -> r.id
    | Duplicate _ -> Alcotest.fail "key should be free after the job ended"
  in
  (* Requeuing the cancelled first job would create a second active job with
     the same key: it must be refused. *)
  Alcotest.(check bool) "requeue refused while key is held" false
    (Client.retry client (id_of a));
  ignore (Client.cancel client second);
  Alcotest.(check bool) "requeue allowed once key is free" true
    (Client.retry client (id_of a));
  ignore mem

let test_pause () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j = job "p" (fun _ _ -> Outcome.Ok) in
  Client.pause_queue client "default";
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  Eio.Time.sleep clock 10.;
  Alcotest.check state_t "not run while paused" Available (state_of mem id);
  Client.resume_queue client "default";
  wait_until ~clock "ran after resume" (fun () -> state_of mem id = Completed);
  Node.stop node

let test_unknown_worker_and_bad_args () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let known = job "known" (fun _ _ -> Outcome.Ok) in
  let unknown = job "unknown" (fun _ _ -> Outcome.Ok) in
  let u = id_of (Client.enqueue client unknown 0) in
  let bad =
    Row.inserted_row
      (List.hd
         (Client.enqueue_many client
            [ { (Job.to_insert known 0) with i_args = `String "nope" } ]))
  in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack known ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "processed" (fun () ->
      state_of mem u = Retryable && state_of mem bad.id = Discarded);
  Node.stop node

let test_cron_once_per_slot () =
  with_env @@ fun ~sw ~clock ~mem ~client:_ ->
  let runs = ref 0 in
  let j =
    Job.make ~name:"tick" ~codec:Codec.unit
      ~perform:(fun _ () ->
        incr runs;
        Ok)
      ()
  in
  let start id =
    Node.start ~sw ~clock ~config:(config ~node_id:id ())
      ~cron:[ Node.cron "* * * * *" j () ]
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 2) ]
  in
  let a = start "a" and b = start "b" in
  Eio.Time.sleep clock 600.;
  Node.stop a;
  Node.stop b;
  (* 10 minutes: the first slot after leadership is taken is minute 1. *)
  Alcotest.(check bool)
    (Printf.sprintf "ran %d times" !runs)
    true
    (!runs >= 9 && !runs <= 10);
  let leaders = List.length (List.filter Node.is_leader [ a; b ]) in
  Alcotest.(check int) "stopped nodes are not leaders" 0 leaders

let test_many_nodes_exactly_once () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let counts = Hashtbl.create 100 in
  let j =
    job "count" (fun _ n ->
        Eio.Time.sleep clock 0.01;
        Hashtbl.replace counts n
          (1 + Option.value (Hashtbl.find_opt counts n) ~default:0);
        Outcome.Ok)
  in
  let n = 500 in
  ignore (Client.enqueue_many client (List.init n (fun i -> Job.to_insert j i)));
  let nodes =
    List.init 4 (fun i ->
        Node.start ~sw ~clock
          ~config:(config ~node_id:(Printf.sprintf "n%d" i) ())
          (Memory_backend.backend mem)
          ~jobs:[ Job.pack j ]
          ~queues:[ ("default", 7) ])
  in
  wait_until ~clock "all done" (fun () ->
      Hashtbl.length counts = n
      && List.for_all
           (fun (r : Row.t) -> r.state = Completed)
           (Memory_backend.all mem));
  List.iter Node.stop nodes;
  Alcotest.(check bool)
    "every job exactly once" true
    (Hashtbl.fold (fun _ c ok -> ok && c = 1) counts true);
  let workers =
    List.sort_uniq compare
      (List.filter_map
         (fun (r : Row.t) -> r.attempted_by)
         (Memory_backend.all mem))
  in
  Alcotest.(check int) "work spread over all nodes" 4 (List.length workers)

let test_prune () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let j = job "x" (fun _ _ -> Outcome.Ok) in
  let id = id_of (Client.enqueue client j 0) in
  let config =
    { (config ()) with prune_after = Some 100.; prune_interval = 10. }
  in
  let node =
    Node.start ~sw ~clock ~config
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "done" (fun () -> state_of mem id = Completed);
  wait_until ~clock "pruned" (fun () -> Memory_backend.get mem id = None);
  Alcotest.(check bool) "kept for prune_after" true (Eio.Time.now clock >= 100.);
  Node.stop node

let test_middleware_and_telemetry () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let log = ref [] in
  let mw name : Middleware.t =
   fun _ next ->
    log := (name ^ ":before") :: !log;
    let o = next () in
    log := (name ^ ":after") :: !log;
    o
  in
  let finished = ref 0 in
  let telemetry = Telemetry.create () in
  Telemetry.attach telemetry (function
    | Job_finished { recorded = true; _ } -> incr finished
    | _ -> ());
  let j =
    job "x" (fun _ _ ->
        log := "job" :: !log;
        Outcome.Ok)
  in
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ()) ~telemetry
      ~middleware:[ mw "outer"; mw "inner" ]
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "done" (fun () -> state_of mem id = Completed);
  Node.stop node;
  Alcotest.(check (list string))
    "onion order"
    [ "outer:before"; "inner:before"; "job"; "inner:after"; "outer:after" ]
    (List.rev !log);
  Alcotest.(check int) "telemetry" 1 !finished

let test_operator_retry () =
  with_env @@ fun ~sw ~clock ~mem ~client ->
  let fail = ref true in
  let j =
    job "x" ~max_attempts:1 (fun _ _ ->
        if !fail then Outcome.Error "nope" else Outcome.Ok)
  in
  let id = id_of (Client.enqueue client j 0) in
  let node =
    Node.start ~sw ~clock ~config:(config ())
      (Memory_backend.backend mem)
      ~jobs:[ Job.pack j ]
      ~queues:[ ("default", 1) ]
  in
  wait_until ~clock "discarded" (fun () -> state_of mem id = Discarded);
  fail := false;
  Alcotest.(check bool) "retry accepted" true (Client.retry client id);
  wait_until ~clock "completed" (fun () -> state_of mem id = Completed);
  Node.stop node;
  Alcotest.(check int)
    "extra attempt granted" 2
    (Option.get (Memory_backend.get mem id)).max_attempts

let () =
  let tc name f = Alcotest.test_case name `Quick f in
  Alcotest.run "caravan-runtime"
    [
      ( "execution",
        [
          tc "jobs complete" test_completes;
          tc "retry with backoff, then discard" test_retry_then_discard;
          tc "timeout" test_timeout;
          tc "snooze, cancel, discard" test_snooze_and_outcomes;
          tc "concurrency limit" test_concurrency_limit;
          tc "priority order" test_priority_order;
          tc "scheduled jobs" test_scheduled;
          tc "unknown worker and bad args" test_unknown_worker_and_bad_args;
          tc "middleware and telemetry" test_middleware_and_telemetry;
        ] );
      ( "lifecycle",
        [
          tc "shutdown releases unfinished jobs" test_graceful_shutdown_releases;
          tc "shutdown waits for short jobs" test_graceful_shutdown_waits;
          tc "rescue from dead node" test_rescue_dead_node;
          tc "stale outcome ignored" test_stale_outcome_ignored;
        ] );
      ( "cluster",
        [
          tc "unique jobs" test_unique;
          tc "pause and resume" test_pause;
          tc "cron fires once per slot" test_cron_once_per_slot;
          tc "many nodes, exactly once" test_many_nodes_exactly_once;
          tc "prune" test_prune;
          tc "operator retry" test_operator_retry;
        ] );
    ]
