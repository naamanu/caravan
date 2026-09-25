(* Chaos test: crash worker processes with SIGKILL while they run jobs, and
   check that no job is lost.

     CARAVAN_PG_URL=postgresql://... dune exec test/chaos/chaos.exe -- [jobs] [seconds]

   The driver spawns worker processes (this same executable with "worker"),
   enqueues jobs, then repeatedly kills a random worker and starts a
   replacement. Each job appends a row to an audit table inside its own
   transaction, so the audit table records every execution that really
   happened, independently of Caravan's bookkeeping.

   Pass criteria:
   - every job ends Completed (nothing lost, nothing stuck);
   - every job executed at least once according to the audit table;
   - duplicate executions only happen for jobs whose node was killed
     (at-least-once), and are reported. *)

open Caravan

let url () =
  match Sys.getenv_opt "CARAVAN_PG_URL" with
  | Some u -> u
  | None ->
      prerr_endline "CARAVAN_PG_URL is not set";
      exit 2

let audit_job pg =
  Job.make ~name:"audit" ~codec:Codec.int ~queue:"chaos" ~max_attempts:25
    ~backoff:(Backoff.Constant 0.5)
    ~perform:(fun ctx n ->
      (* Some real work, so kills land mid-job. *)
      Eio_unix.sleep (0.1 +. Random.float 0.3);
      Caravan_postgres.with_transaction pg (fun conn ->
          ignore
            (Caravan_postgres.Pg.query conn
               ~params:[ Some (string_of_int n); Some (Ctx.node ctx) ]
               "INSERT INTO chaos_audit (n, node) VALUES ($1, $2)"));
      Outcome.Ok)
    ()

let worker () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pg =
    Caravan_postgres.connect ~sw ~clock:env#clock ~pool_size:12 (url ())
  in
  let config =
    {
      (Node.default_config ()) with
      heartbeat_interval = 0.5;
      node_timeout = 3.;
      rescue_interval = 0.5;
      leader_ttl = 2.;
      maintenance_interval = 0.2;
      poll_interval = 0.5;
    }
  in
  let node =
    Node.start ~sw ~clock:env#clock ~config
      (Caravan_postgres.backend pg)
      ~jobs:[ Job.pack (audit_job pg) ]
      ~queues:[ ("chaos", 8) ]
  in
  Node.run_until_signal node

let driver ~jobs ~seconds =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  let pg = Caravan_postgres.connect ~sw ~clock ~pool_size:4 (url ()) in
  ignore (Caravan_postgres.migrate pg);
  Caravan_postgres.truncate_all pg;
  Caravan_postgres.with_transaction pg (fun c ->
      ignore
        (Caravan_postgres.Pg.query c
           "CREATE TABLE IF NOT EXISTS chaos_audit (n int NOT NULL, node text \
            NOT NULL)");
      ignore (Caravan_postgres.Pg.query c "TRUNCATE chaos_audit"));
  let client = Client.make (Caravan_postgres.backend pg) in
  let job = audit_job pg in
  ignore
    (Client.enqueue_many client (List.init jobs (fun i -> Job.to_insert job i)));
  Fmt.pr "enqueued %d jobs@." jobs;
  let mgr = Eio.Stdenv.process_mgr env in
  let spawn () =
    Eio.Process.spawn ~sw mgr ~stdout:(Eio.Stdenv.stderr env)
      [ Sys.executable_name; "worker" ]
  in
  let workers = Array.init 4 (fun _ -> spawn ()) in
  let kills = ref 0 in
  let deadline = Eio.Time.now clock +. seconds in
  while Eio.Time.now clock < deadline do
    Eio.Time.sleep clock (0.5 +. Random.float 1.5);
    let i = Random.int (Array.length workers) in
    Eio.Process.signal workers.(i) Sys.sigkill;
    ignore (Eio.Process.await workers.(i));
    incr kills;
    workers.(i) <- spawn ()
  done;
  Fmt.pr "killed %d workers; waiting for the queue to drain@." !kills;
  let completed () =
    List.fold_left
      (fun acc (_, s, n) -> if s = State.Completed then acc + n else acc)
      0 (Client.stats client).counts
  in
  let drain_deadline = Eio.Time.now clock +. 120. in
  while completed () < jobs && Eio.Time.now clock < drain_deadline do
    Eio.Time.sleep clock 0.5
  done;
  Array.iter (fun w -> Eio.Process.signal w Sys.sigterm) workers;
  Array.iter (fun w -> ignore (Eio.Process.await w)) workers;
  let stats = Client.stats client in
  let q sql =
    Caravan_postgres.with_transaction pg (fun c ->
        match (Caravan_postgres.Pg.query c sql).rows with
        | [| [| Some v |] |] -> int_of_string v
        | _ -> failwith sql)
  in
  let executed = q "SELECT count(DISTINCT n) FROM chaos_audit" in
  let executions = q "SELECT count(*) FROM chaos_audit" in
  let rescued = q "SELECT count(*) FROM caravan_jobs WHERE attempt > 1" in
  let not_completed =
    List.filter (fun (_, s, n) -> s <> State.Completed && n > 0) stats.counts
  in
  Fmt.pr
    "completed: %d/%d, executed at least once: %d, total executions: %d (%d \
     duplicates from %d kills; %d jobs needed more than one attempt)@."
    (completed ()) jobs executed executions (executions - executed) !kills
    rescued;
  if not_completed <> [] || completed () <> jobs || executed <> jobs then (
    List.iter
      (fun (q, s, n) -> Fmt.pr "  %s: %d %a@." q n State.pp s)
      not_completed;
    Fmt.pr "CHAOS TEST FAILED@.";
    exit 1)
  else Fmt.pr "CHAOS TEST PASSED@."

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Warning);
  match Array.to_list Sys.argv |> List.tl with
  | [ "worker" ] -> worker ()
  | [] -> driver ~jobs:5000 ~seconds:20.
  | [ jobs; seconds ] ->
      driver ~jobs:(int_of_string jobs) ~seconds:(float_of_string seconds)
  | _ ->
      prerr_endline "usage: chaos.exe [jobs seconds]";
      exit 2
