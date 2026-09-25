open Caravan

let run f =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#clock in
  let mem = Memory_backend.create ~clock () in
  let now () = Option.get (Ptime.of_float_s (Eio.Time.now clock)) in
  let client = Client.make ~now (Memory_backend.backend mem) in
  f ~client ~now

let job =
  Job.make ~name:"greet" ~codec:Codec.string ~perform:(fun _ _ -> Outcome.Ok) ()

let req ?(meth = `GET) ?(headers = []) ?(query = []) path : Caravan_web.request
    =
  {
    meth;
    path;
    query;
    header = (fun k -> List.assoc_opt (String.lowercase_ascii k) headers);
  }

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

let check_status name expected (r : Caravan_web.response) =
  Alcotest.(check int) name expected r.status

let basic u p = "Basic " ^ Base64.encode_string (u ^ ":" ^ p)

let test_pages () =
  run @@ fun ~client ~now ->
  let web = Caravan_web.make ~now client in
  ignore (Client.enqueue client job "<script>alert(1)</script>");
  let r = Caravan_web.handle web (req "/") in
  check_status "overview" 200 r;
  Alcotest.(check bool) "lists the queue" true (contains ~sub:"default" r.body);
  let r = Caravan_web.handle web (req "/jobs") in
  check_status "jobs" 200 r;
  Alcotest.(check bool)
    "args are escaped" false
    (contains ~sub:"<script>" r.body);
  Alcotest.(check bool)
    "escaped form present" true
    (contains ~sub:"&lt;script&gt;" r.body);
  check_status "job page" 200 (Caravan_web.handle web (req "/jobs/1"));
  check_status "missing job" 404 (Caravan_web.handle web (req "/jobs/99"));
  check_status "bad id" 404 (Caravan_web.handle web (req "/jobs/abc"));
  check_status "unknown" 404 (Caravan_web.handle web (req "/nope"))

let test_actions () =
  run @@ fun ~client ~now ->
  let web = Caravan_web.make ~now client in
  let id =
    match Client.enqueue client job "x" with
    | Inserted r -> r.id
    | Duplicate r -> r.id
  in
  let r =
    Caravan_web.handle web
      (req ~meth:`POST (Printf.sprintf "/jobs/%Ld/cancel" id))
  in
  check_status "redirect" 303 r;
  Alcotest.(check bool)
    "cancelled" true
    ((Option.get (Client.get client id)).state = Cancelled);
  ignore (Caravan_web.handle web (req ~meth:`POST "/queues/default/pause"));
  Alcotest.(check (list string))
    "paused" [ "default" ] (Client.stats client).paused;
  let r =
    Caravan_web.handle web (req ~meth:`POST "/api/queues/default/resume")
  in
  check_status "api resume" 200 r;
  Alcotest.(check (list string)) "resumed" [] (Client.stats client).paused

let test_security () =
  run @@ fun ~client ~now ->
  let web = Caravan_web.make ~now ~auth:("admin", "pw") client in
  check_status "no credentials" 401 (Caravan_web.handle web (req "/"));
  check_status "wrong password" 401
    (Caravan_web.handle web
       (req ~headers:[ ("authorization", basic "admin" "nope") ] "/"));
  check_status "right password" 200
    (Caravan_web.handle web
       (req ~headers:[ ("authorization", basic "admin" "pw") ] "/"));
  check_status "healthz is open" 200 (Caravan_web.handle web (req "/healthz"));
  let auth = ("authorization", basic "admin" "pw") in
  check_status "cross-origin post" 403
    (Caravan_web.handle web
       (req ~meth:`POST
          ~headers:
            [
              auth; ("origin", "https://evil.example"); ("host", "caravan.local");
            ]
          "/queues/default/pause"));
  Alcotest.(check (list string)) "not paused" [] (Client.stats client).paused;
  let ro = Caravan_web.make ~now ~read_only:true client in
  check_status "read-only refuses" 403
    (Caravan_web.handle ro (req ~meth:`POST "/api/queues/default/pause"))

let test_api_and_prefix () =
  run @@ fun ~client ~now ->
  let web = Caravan_web.make ~now ~prefix:"/admin/jobs" client in
  ignore (Client.enqueue client job "a");
  ignore (Client.enqueue client job "b");
  let r =
    Caravan_web.handle web
      (req ~query:[ ("limit", [ "1" ]) ] "/admin/jobs/api/jobs")
  in
  check_status "api under prefix" 200 r;
  (match Yojson.Safe.from_string r.body with
  | `List [ j ] ->
      Alcotest.(check string)
        "newest first" "b"
        Yojson.Safe.Util.(member "args" j |> to_string)
  | _ -> Alcotest.fail "expected one job");
  check_status "outside prefix" 404 (Caravan_web.handle web (req "/api/jobs"));
  let r = Caravan_web.handle web (req "/admin/jobs/") in
  Alcotest.(check bool)
    "base href" true
    (contains ~sub:{|<base href="/admin/jobs/">|} r.body)

let test_metrics () =
  run @@ fun ~client ~now ->
  let metrics = Caravan_web.Metrics.create () in
  let telemetry = Telemetry.create () in
  Caravan_web.Metrics.attach metrics telemetry;
  let row =
    match Client.enqueue client job "m" with Inserted r | Duplicate r -> r
  in
  Telemetry.emit telemetry
    (Job_finished
       { node = "n"; row; outcome = Ok; duration = 0.2; recorded = true });
  let web = Caravan_web.make ~now ~metrics client in
  let body = (Caravan_web.handle web (req "/metrics")).body in
  List.iter
    (fun sub -> Alcotest.(check bool) sub true (contains ~sub body))
    [
      {|caravan_jobs{queue="default",state="available"} 1|};
      {|caravan_job_executions_total{queue="default",worker="greet",outcome="ok"} 1|};
      {|caravan_job_duration_seconds_bucket{queue="default",worker="greet",le="0.25"} 1|};
      {|caravan_job_duration_seconds_bucket{queue="default",worker="greet",le="0.1"} 0|};
      {|caravan_job_duration_seconds_count{queue="default",worker="greet"} 1|};
    ]

let () =
  let tc n f = Alcotest.test_case n `Quick f in
  Alcotest.run "caravan-web"
    [
      ( "web",
        [
          tc "pages render and escape" test_pages;
          tc "actions" test_actions;
          tc "auth, csrf, read-only" test_security;
          tc "api and prefix" test_api_and_prefix;
          tc "prometheus metrics" test_metrics;
        ] );
    ]
