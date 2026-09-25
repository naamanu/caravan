(* Throughput benchmark.

     CARAVAN_PG_URL=postgresql://... dune exec --release bench/bench.exe

   Measures enqueue throughput and end-to-end processing throughput of no-op
   jobs (so it measures Caravan and the database, not job work), for the memory
   backend and for PostgreSQL with 1, 2 and 4 nodes in one process. Nodes in
   one process share a domain; separate processes on separate machines scale
   further as long as the database keeps up. *)

open Caravan

let noop =
  Job.make ~name:"noop" ~codec:Codec.int ~perform:(fun _ _ -> Outcome.Ok) ()

let time clock f =
  let t0 = Eio.Time.now clock in
  let r = f () in
  (r, Eio.Time.now clock -. t0)

let completed client =
  List.fold_left
    (fun acc (_, s, n) -> if s = State.Completed then acc + n else acc)
    0 (Client.stats client).counts

let run_nodes ~sw ~clock ~backends ~n ~concurrency client =
  let config =
    {
      (Node.default_config ()) with
      poll_interval = 0.2;
      maintenance_interval = 0.5;
    }
  in
  let (), secs =
    time clock (fun () ->
        let nodes =
          List.mapi
            (fun i b ->
              Node.start ~sw ~clock
                ~config:{ config with node_id = Printf.sprintf "bench%d" i }
                b
                ~jobs:[ Job.pack noop ]
                ~queues:[ ("default", concurrency) ])
            backends
        in
        while completed client < n do
          Eio.Time.sleep clock 0.05
        done;
        List.iter Node.stop nodes)
  in
  secs

let row label n secs =
  Fmt.pr "  %-44s %8d jobs  %6.2fs  %8.0f jobs/s@." label n secs
    (float n /. secs)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  let n = 20_000 in
  let inserts = List.init n (fun i -> Job.to_insert noop i) in
  let rec chunks l =
    match l with
    | [] -> []
    | _ ->
        List.filteri (fun i _ -> i < 1000) l
        :: chunks (List.filteri (fun i _ -> i >= 1000) l)
  in
  Fmt.pr "Memory backend:@.";
  let mem = Memory_backend.create ~clock () in
  let client = Client.make (Memory_backend.backend mem) in
  let (), secs =
    time clock (fun () ->
        List.iter
          (fun c -> ignore (Client.enqueue_many client c))
          (chunks inserts))
  in
  row "enqueue (batches of 1000)" n secs;
  row "process, 1 node x 50" n
    (run_nodes ~sw ~clock
       ~backends:[ Memory_backend.backend mem ]
       ~n ~concurrency:50 client);
  match Sys.getenv_opt "CARAVAN_PG_URL" with
  | None -> Fmt.pr "Set CARAVAN_PG_URL to benchmark PostgreSQL.@."
  | Some url ->
      Fmt.pr "PostgreSQL:@.";
      let connect () = Caravan_postgres.connect ~sw ~clock ~pool_size:20 url in
      let pg = connect () in
      ignore (Caravan_postgres.migrate pg);
      let client = Client.make (Caravan_postgres.backend pg) in
      Caravan_postgres.truncate_all pg;
      let (), secs =
        time clock (fun () ->
            List.iter
              (fun c -> ignore (Client.enqueue_many client c))
              (chunks inserts))
      in
      row "enqueue (batches of 1000)" n secs;
      Caravan_postgres.truncate_all pg;
      let m = 2000 in
      let (), secs =
        time clock (fun () ->
            for i = 1 to m do
              ignore (Client.enqueue client noop i)
            done)
      in
      row "enqueue (one at a time)" m secs;
      List.iter
        (fun nodes ->
          Caravan_postgres.truncate_all pg;
          List.iter
            (fun c -> ignore (Client.enqueue_many client c))
            (chunks inserts);
          let backends =
            List.init nodes (fun _ -> Caravan_postgres.backend (connect ()))
          in
          row
            (Printf.sprintf "process, %d node(s) x 50" nodes)
            n
            (run_nodes ~sw ~clock ~backends ~n ~concurrency:50 client))
        [ 1; 2; 4 ];
      exit 0
