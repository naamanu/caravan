(* A distributed Caravan cluster on PostgreSQL.

     export CARAVAN_DATABASE_URL=postgresql://localhost/caravan_demo
     dune exec examples/multi_node/main.exe -- migrate
     dune exec examples/multi_node/main.exe -- worker        # in several terminals
     dune exec examples/multi_node/main.exe -- enqueue 1000
     dune exec bin/main.exe -- web                           # watch it happen

   Kill a worker with kill -9 while it is busy: after the node timeout its jobs
   are rescued and finish elsewhere. *)

open Caravan

type order = { order_id : int; customer : string } [@@deriving yojson]

let send_receipt =
  Job.make ~name:"send_receipt" ~queue:"mailers" ~max_attempts:5
    ~codec:(Codec.of_yojson order_to_yojson order_of_yojson)
    ~perform:(fun ctx o ->
      Eio_unix.sleep (0.05 +. Random.float 0.3);
      (* A realistic trickle of transient failures. *)
      if Random.int 10 = 0 && not (Ctx.is_final_attempt ctx) then
        Outcome.Error
          (Printf.sprintf "SMTP 451: mailbox for %s temporarily unavailable"
             o.customer)
      else Outcome.Ok)
    ()

let render_invoice =
  Job.make ~name:"render_invoice" ~queue:"pdf" ~timeout:30.
    ~codec:(Codec.of_yojson order_to_yojson order_of_yojson)
    ~perform:(fun _ o ->
      (* CPU-bound work: runs on the executor pool, not the node's domain. *)
      let acc = ref o.order_id in
      for i = 1 to 20_000_000 do
        acc := (!acc * 31) + i
      done;
      ignore (Sys.opaque_identity !acc);
      Outcome.Ok)
    ()

let nightly_report =
  Job.make ~name:"nightly_report" ~codec:Codec.unit
    ~perform:(fun _ () ->
      Logs.app (fun m ->
          m "generating the report (fires once per minute cluster-wide)");
      Outcome.Ok)
    ()

let url () =
  match Sys.getenv_opt "CARAVAN_DATABASE_URL" with
  | Some u -> u
  | None -> "postgresql://caravan@localhost:54329/caravan_test"

let with_backend f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pg = Caravan_postgres.connect ~sw ~clock:env#clock (url ()) in
  f env sw pg

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  match Array.to_list Sys.argv |> List.tl with
  | [ "migrate" ] ->
      with_backend (fun _ _ pg ->
          Logs.app (fun m ->
              m "applied %d migrations"
                (List.length (Caravan_postgres.migrate pg))))
  | [ "enqueue"; n ] ->
      with_backend (fun _ _ pg ->
          let client = Client.make (Caravan_postgres.backend pg) in
          let n = int_of_string n in
          let inserts =
            List.concat
              (List.init n (fun i ->
                   let o =
                     {
                       order_id = i;
                       customer = Printf.sprintf "customer%d@example.com" i;
                     }
                   in
                   [ Job.to_insert send_receipt o ]
                   @
                   if i mod 10 = 0 then [ Job.to_insert render_invoice o ]
                   else []))
          in
          ignore (Client.enqueue_many client inserts);
          Logs.app (fun m -> m "enqueued %d jobs" (List.length inserts)))
  | [ "worker" ] ->
      with_backend (fun env sw pg ->
          let telemetry = Telemetry.create () in
          Telemetry.attach telemetry (Telemetry.log_handler ());
          let pool =
            Eio.Executor_pool.create ~sw ~domain_count:2
              (Eio.Stdenv.domain_mgr env)
          in
          let config =
            {
              (Node.default_config ()) with
              node_timeout = 20.;
              rescue_interval = 5.;
            }
          in
          let node =
            Node.start ~sw ~clock:env#clock ~config ~telemetry
              ~middleware:[ Middleware.logging () ]
              ~cron:[ Node.cron "* * * * *" nightly_report () ]
              ~executor:(pool, [ "pdf" ])
              (Caravan_postgres.backend pg)
              ~jobs:
                [
                  Job.pack send_receipt;
                  Job.pack render_invoice;
                  Job.pack nightly_report;
                ]
              ~queues:[ ("mailers", 20); ("pdf", 2); ("default", 5) ]
          in
          Logs.app (fun m ->
              m "worker %s running; Ctrl-C to stop gracefully" (Node.id node));
          Node.run_until_signal node)
  | _ ->
      prerr_endline "usage: main.exe (migrate | worker | enqueue N)";
      exit 2
