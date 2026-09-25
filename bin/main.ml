open Cmdliner
open Caravan

let db =
  let env = Cmd.Env.info "CARAVAN_DATABASE_URL" in
  Arg.(
    required
    & opt (some string) None
    & info [ "d"; "db" ] ~env ~docv:"URL"
        ~doc:
          "PostgreSQL connection string, e.g. $(b,postgresql://user@host/db).")

let json_flag =
  Arg.(value & flag & info [ "json" ] ~doc:"Print JSON instead of a table.")

let setup_logs =
  let verbose =
    Arg.(
      value & flag_all & info [ "v"; "verbose" ] ~doc:"Log more (repeatable).")
  in
  Term.(
    const (fun v ->
        Fmt_tty.setup_std_outputs ();
        Logs.set_reporter (Logs_fmt.reporter ());
        Logs.set_level
          (Some
             (match List.length v with
             | 0 -> Logs.Warning
             | 1 -> Info
             | _ -> Debug)))
    $ verbose)

(* Run [f] with a connected backend; exit 1 with a readable message on error. *)
let with_pg ?(listen = false) url f =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pg =
      Caravan_postgres.connect ~sw ~clock:env#clock ~pool_size:2 ~listen url
    in
    f ~env ~sw pg;
    0
  with
  | Caravan_postgres.Pg.Pg_error { message; sqlstate; _ } ->
      Fmt.epr "caravan: database error (%s): %s@." sqlstate message;
      1
  | Failure m | Invalid_argument m ->
      Fmt.epr "caravan: %s@." m;
      1

let client pg = Client.make (Caravan_postgres.backend pg)
let print_json j = print_endline (Yojson.Safe.pretty_to_string j)

(* --- Table rendering ------------------------------------------------------ *)

let print_table headers rows =
  let widths =
    List.mapi
      (fun i h ->
        List.fold_left
          (fun w row -> max w (String.length (List.nth row i)))
          (String.length h) rows)
      headers
  in
  let line cells =
    print_endline
      (String.concat "  "
         (List.map2
            (fun w c -> c ^ String.make (w - String.length c) ' ')
            widths cells)
      |> String.trim)
  in
  line headers;
  line (List.map (fun w -> String.make w '-') widths);
  List.iter line rows

(* --- Commands ------------------------------------------------------------- *)

let migrate_cmd =
  let run () url =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        match Caravan_postgres.migrate pg with
        | [] ->
            Fmt.pr "Schema is up to date (version %d).@."
              (Caravan_postgres.schema_version pg)
        | vs ->
            Fmt.pr "Applied migrations %a; schema is now at version %d.@."
              Fmt.(list ~sep:comma int)
              vs
              (Caravan_postgres.schema_version pg))
  in
  Cmd.v
    (Cmd.info "migrate"
       ~doc:"Create or upgrade the Caravan tables. Safe to run concurrently.")
    Term.(const run $ setup_logs $ db)

let stats_cmd =
  let run () url json =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        let stats = Client.stats (client pg) in
        if json then
          print_json
            (`Assoc
               [
                 ( "counts",
                   `List
                     (List.map
                        (fun (q, s, n) ->
                          `Assoc
                            [
                              ("queue", `String q);
                              ("state", `String (State.to_string s));
                              ("count", `Int n);
                            ])
                        stats.counts) );
                 ("paused", `List (List.map (fun q -> `String q) stats.paused));
               ])
        else
          let queues =
            List.sort_uniq compare
              (List.map (fun (q, _, _) -> q) stats.counts @ stats.paused)
          in
          let count q s =
            List.fold_left
              (fun a (q', s', n) -> if q = q' && s = s' then a + n else a)
              0 stats.counts
          in
          if queues = [] then print_endline "No jobs."
          else
            print_table
              ("queue" :: List.map State.to_string State.all)
              (List.map
                 (fun q ->
                   (if List.mem q stats.paused then q ^ " (paused)" else q)
                   :: List.map (fun s -> string_of_int (count q s)) State.all)
                 queues))
  in
  Cmd.v
    (Cmd.info "stats" ~doc:"Show job counts by queue and state.")
    Term.(const run $ setup_logs $ db $ json_flag)

let time t = Ptime.to_rfc3339 ~tz_offset_s:0 t

let jobs_list_cmd =
  let states =
    Arg.(
      value & opt_all string []
      & info [ "s"; "state" ] ~docv:"STATE" ~doc:"Filter by state (repeatable).")
  and queues =
    Arg.(
      value & opt_all string []
      & info [ "q"; "queue" ] ~docv:"QUEUE" ~doc:"Filter by queue (repeatable).")
  and worker =
    Arg.(
      value
      & opt (some string) None
      & info [ "w"; "worker" ] ~docv:"NAME" ~doc:"Filter by job name.")
  and limit =
    Arg.(
      value & opt int 20 & info [ "n"; "limit" ] ~doc:"Maximum number of jobs.")
  in
  let run () url states queues worker limit json =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        let states =
          List.map
            (fun s ->
              match State.of_string s with Ok s -> s | Error e -> failwith e)
            states
        in
        let rows =
          Client.list (client pg)
            (Backend.query ~states ~queues ?worker ~limit ())
        in
        if json then print_json (`List (List.map Row.to_json rows))
        else
          print_table
            [
              "id";
              "state";
              "worker";
              "queue";
              "attempt";
              "scheduled";
              "last error";
            ]
            (List.map
               (fun (r : Row.t) ->
                 [
                   Int64.to_string r.id;
                   State.to_string r.state;
                   r.worker;
                   r.queue;
                   Printf.sprintf "%d/%d" r.attempt r.max_attempts;
                   time r.scheduled_at;
                   (match List.rev r.errors with
                   | e :: _ ->
                       let m = List.hd (String.split_on_char '\n' e.message) in
                       if String.length m > 50 then String.sub m 0 47 ^ "..."
                       else m
                   | [] -> "");
                 ])
               rows))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List jobs, newest first.")
    Term.(
      const run $ setup_logs $ db $ states $ queues $ worker $ limit $ json_flag)

let job_id =
  Arg.(required & pos 0 (some int64) None & info [] ~docv:"ID" ~doc:"Job id.")

let jobs_get_cmd =
  let run () url id =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        match Client.get (client pg) id with
        | Some r -> print_json (Row.to_json r)
        | None -> failwith (Printf.sprintf "no job with id %Ld" id))
  in
  Cmd.v
    (Cmd.info "get" ~doc:"Show one job as JSON.")
    Term.(const run $ setup_logs $ db $ job_id)

let job_action name doc f past =
  let run () url id =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        if f (client pg) id then Fmt.pr "Job %Ld %s.@." id past
        else
          failwith
            (Printf.sprintf "job %Ld could not be %s in its current state" id
               past))
  in
  Cmd.v (Cmd.info name ~doc) Term.(const run $ setup_logs $ db $ job_id)

let jobs_cmd =
  Cmd.group
    (Cmd.info "jobs" ~doc:"Inspect and manage jobs.")
    [
      jobs_list_cmd;
      jobs_get_cmd;
      job_action "retry" "Run a finished, failed or waiting job again now."
        Client.retry "requeued";
      job_action "cancel" "Cancel an active job." Client.cancel "cancelled";
    ]

let queue_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUEUE")

let queues_cmd =
  let action name doc f past =
    let run () url q =
      with_pg url (fun ~env:_ ~sw:_ pg ->
          f (client pg) q;
          Fmt.pr "Queue %s %s.@." q past)
    in
    Cmd.v (Cmd.info name ~doc) Term.(const run $ setup_logs $ db $ queue_arg)
  in
  Cmd.group
    (Cmd.info "queues" ~doc:"Pause and resume queues.")
    [
      action "pause" "Stop nodes from claiming jobs from a queue."
        Client.pause_queue "paused";
      action "resume" "Resume a paused queue." Client.resume_queue "resumed";
    ]

let nodes_cmd =
  let run () url json =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        let nodes = Client.nodes (client pg) in
        if json then
          print_json
            (`List
               (List.map
                  (fun (n : Backend.node_info) ->
                    `Assoc
                      [
                        ("node", `String n.node);
                        ("hostname", `String n.hostname);
                        ("pid", `Int n.pid);
                        ("running", `Int n.running);
                        ("heartbeat_at", `String (time n.heartbeat_at));
                      ])
                  nodes))
        else if nodes = [] then print_endline "No nodes are running."
        else
          print_table
            [ "node"; "host"; "queues"; "running"; "heartbeat" ]
            (List.map
               (fun (n : Backend.node_info) ->
                 [
                   n.node;
                   Printf.sprintf "%s:%d" n.hostname n.pid;
                   String.concat ","
                     (List.map
                        (fun (q, c) -> Printf.sprintf "%s×%d" q c)
                        n.queues);
                   string_of_int n.running;
                   time n.heartbeat_at;
                 ])
               nodes))
  in
  Cmd.v
    (Cmd.info "nodes" ~doc:"List live worker nodes.")
    Term.(const run $ setup_logs $ db $ json_flag)

let duration =
  let parse s =
    let n = String.length s in
    if n < 2 then Error (`Msg "expected a duration like 30m, 12h or 7d")
    else
      let unit = s.[n - 1]
      and v = float_of_string_opt (String.sub s 0 (n - 1)) in
      match (v, unit) with
      | Some v, 's' -> Ok v
      | Some v, 'm' -> Ok (v *. 60.)
      | Some v, 'h' -> Ok (v *. 3600.)
      | Some v, 'd' -> Ok (v *. 86400.)
      | _ -> Error (`Msg "expected a duration like 30m, 12h or 7d")
  in
  Arg.conv' ~docv:"DURATION"
    ( (fun s -> Result.map_error (fun (`Msg m) -> m) (parse s)),
      fun ppf v -> Fmt.pf ppf "%gs" v )

let prune_cmd =
  let older =
    Arg.(
      value
      & opt duration (7. *. 86400.)
      & info [ "older-than" ] ~doc:"Age of finished jobs to delete.")
  in
  let run () url older =
    with_pg url (fun ~env:_ ~sw:_ pg ->
        let before =
          Option.get
            (Ptime.sub_span (Ptime_clock.now ())
               (Option.get (Ptime.Span.of_float_s older)))
        in
        let rec go total =
          let n = Caravan_postgres.prune pg ~before ~limit:10_000 in
          if n = 0 then total else go (total + n)
        in
        Fmt.pr "Deleted %d finished jobs.@." (go 0))
  in
  Cmd.v
    (Cmd.info "prune" ~doc:"Delete finished jobs older than a given age.")
    Term.(const run $ setup_logs $ db $ older)

let web_cmd =
  let port =
    Arg.(value & opt int 4000 & info [ "p"; "port" ] ~doc:"Port to listen on.")
  and host =
    Arg.(
      value & opt string "127.0.0.1"
      & info [ "host" ] ~doc:"Address to bind (use 0.0.0.0 for all).")
  and user =
    Arg.(
      value
      & opt (some string) None
      & info [ "user" ]
          ~env:(Cmd.Env.info "CARAVAN_WEB_USER")
          ~doc:"Basic-auth user.")
  and password =
    Arg.(
      value
      & opt (some string) None
      & info [ "password" ]
          ~env:(Cmd.Env.info "CARAVAN_WEB_PASSWORD")
          ~doc:"Basic-auth password (prefer the environment variable).")
  and prefix =
    Arg.(
      value & opt string "/"
      & info [ "prefix" ] ~doc:"URL path to mount the dashboard at.")
  and read_only =
    Arg.(
      value & flag
      & info [ "read-only" ] ~doc:"Disable retry, cancel and pause.")
  in
  let run () url port host user password prefix read_only =
    with_pg url (fun ~env ~sw pg ->
        let auth =
          match (user, password) with
          | Some u, Some p -> Some (u, p)
          | None, None -> None
          | _ -> failwith "set both --user and --password, or neither"
        in
        let host =
          match Unix.inet_addr_of_string host with
          | addr -> Eio_unix.Net.Ipaddr.of_unix addr
          | exception Failure _ -> failwith ("not an IP address: " ^ host)
        in
        let web = Caravan_web.make ~prefix ?auth ~read_only (client pg) in
        Fmt.pr "Caravan dashboard on http://%a:%d%s@." Eio.Net.Ipaddr.pp host
          port prefix;
        Caravan_web.serve ~sw ~net:env#net ~host ~port web)
  in
  Cmd.v
    (Cmd.info "web" ~doc:"Serve the dashboard, JSON API and Prometheus metrics.")
    Term.(
      const run $ setup_logs $ db $ port $ host $ user $ password $ prefix
      $ read_only)

let () =
  let doc = "operate Caravan job queues" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Inspect and manage the jobs, queues and nodes of a Caravan cluster \
         backed by PostgreSQL.";
      `P "The database URL is read from $(b,--db) or $(b,CARAVAN_DATABASE_URL).";
    ]
  in
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "caravan" ~version:"%%VERSION%%" ~doc ~man)
          [
            migrate_cmd;
            stats_cmd;
            jobs_cmd;
            queues_cmd;
            nodes_cmd;
            prune_cmd;
            web_cmd;
          ]))
