open Caravan
module Metrics = Metrics

let src = Logs.Src.create "caravan.web" ~doc:"Caravan web dashboard"

module Log = (val Logs.src_log src)

type t = {
  client : Client.t;
  prefix : string;  (** Always starts and ends with '/'. *)
  auth : (string * string) option;
  read_only : bool;
  metrics : Metrics.t option;
  now : unit -> Ptime.t;
}

type request = {
  meth : [ `GET | `POST | `HEAD | `Other ];
  path : string;  (** Absolute, without the query string. *)
  query : (string * string list) list;
  header : string -> string option;
}

type response = {
  status : int;
  content_type : string;
  headers : (string * string) list;
  body : string;
}

let normalize_prefix p =
  let p = if String.starts_with ~prefix:"/" p then p else "/" ^ p in
  if String.ends_with ~suffix:"/" p then p else p ^ "/"

let make ?(prefix = "/") ?auth ?(read_only = false) ?metrics
    ?(now = Ptime_clock.now) client =
  { client; prefix = normalize_prefix prefix; auth; read_only; metrics; now }

let respond ?(headers = []) ?(content_type = "text/html; charset=utf-8") status
    body =
  { status; content_type; headers; body }

let json ?(status = 200) j =
  respond ~content_type:"application/json" status (Yojson.Safe.to_string j)

let json_error status message =
  json ~status (`Assoc [ ("error", `String message) ])

let redirect location = respond ~headers:[ ("location", location) ] 303 ""

(* --- Security ---------------------------------------------------------------- *)

let constant_time_equal a b =
  String.length a = String.length b
  &&
  let diff = ref 0 in
  String.iteri
    (fun i c -> diff := !diff lor (Char.code c lxor Char.code b.[i]))
    a;
  !diff = 0

let authorized t req =
  match t.auth with
  | None -> true
  | Some (user, pass) -> (
      match req.header "authorization" with
      | Some v
        when String.length v > 6
             && String.lowercase_ascii (String.sub v 0 6) = "basic " -> (
          match
            Base64.decode (String.trim (String.sub v 6 (String.length v - 6)))
          with
          | Ok creds -> (
              match String.index_opt creds ':' with
              | Some i ->
                  let u = String.sub creds 0 i
                  and p =
                    String.sub creds (i + 1) (String.length creds - i - 1)
                  in
                  (* Evaluate both comparisons to avoid leaking which failed. *)
                  let ok_u = constant_time_equal u user
                  and ok_p = constant_time_equal p pass in
                  ok_u && ok_p
              | None -> false)
          | Error _ -> false)
      | _ -> false)

(* Reject cross-site form posts: browsers send Origin on cross-origin POSTs. *)
let same_origin req =
  match (req.header "origin", req.header "host") with
  | None, _ -> true
  | Some origin, Some host ->
      let strip s =
        match String.index_opt s ':' with
        | Some i when String.length s > i + 2 && String.sub s i 3 = "://" ->
            String.sub s (i + 3) (String.length s - i - 3)
        | _ -> s
      in
      strip origin = host
  | Some _, None -> false

(* --- Routes -------------------------------------------------------------------- *)

let param req k =
  match List.assoc_opt k req.query with Some (v :: _) -> Some v | _ -> None

let params req k =
  match List.assoc_opt k req.query with Some vs -> vs | None -> []

let query_of_request req =
  let states =
    List.filter_map
      (fun s -> Result.to_option (State.of_string s))
      (params req "state")
  in
  Backend.query ~states ~queues:(params req "queue")
    ?worker:(param req "worker")
    ?before_id:(Option.bind (param req "before") Int64.of_string_opt)
    ?limit:(Option.bind (param req "limit") int_of_string_opt)
    ()

let stats_json (s : Backend.stats) =
  `Assoc
    [
      ( "counts",
        `List
          (List.map
             (fun (q, st, n) ->
               `Assoc
                 [
                   ("queue", `String q);
                   ("state", `String (State.to_string st));
                   ("count", `Int n);
                 ])
             s.counts) );
      ("paused", `List (List.map (fun q -> `String q) s.paused));
    ]

let node_json (n : Backend.node_info) =
  let time t = `String (Ptime.to_rfc3339 ~tz_offset_s:0 t) in
  `Assoc
    [
      ("node", `String n.node);
      ("hostname", `String n.hostname);
      ("pid", `Int n.pid);
      ( "queues",
        `List
          (List.map
             (fun (q, c) ->
               `Assoc [ ("queue", `String q); ("concurrency", `Int c) ])
             n.queues) );
      ("running", `Int n.running);
      ("started_at", time n.started_at);
      ("heartbeat_at", time n.heartbeat_at);
    ]

let segments path =
  String.split_on_char '/' path
  |> List.filter (( <> ) "")
  |> List.map Uri.pct_decode

let route t req =
  let base = t.prefix in
  let rel =
    if String.starts_with ~prefix:t.prefix req.path then
      String.sub req.path (String.length t.prefix)
        (String.length req.path - String.length t.prefix)
    else if req.path ^ "/" = t.prefix then ""
    else "\000"
  in
  let html body = respond 200 body in
  let mutating f =
    if t.read_only then respond 403 "read-only dashboard" else f ()
  in
  let with_job id_s f =
    match Int64.of_string_opt id_s with None -> None | Some id -> Some (f id)
  in
  let now = t.now () in
  let meth = match req.meth with `HEAD -> `GET | m -> m in
  match (meth, segments rel) with
  | _ when rel = "\000" -> None
  | `GET, [] ->
      Some
        (html
           (Views.overview ~base ~read_only:t.read_only ~now
              ~stats:(Client.stats t.client) ~nodes:(Client.nodes t.client)))
  | `GET, [ "jobs" ] ->
      let query = query_of_request req in
      Some (html (Views.jobs ~base ~now ~query (Client.list t.client query)))
  | `GET, [ "jobs"; id ] ->
      with_job id (fun id ->
          match Client.get t.client id with
          | Some r -> html (Views.job ~base ~read_only:t.read_only ~now r)
          | None ->
              respond 404
                (Views.not_found ~base
                   (Printf.sprintf "No job with id %Ld." id)))
  | `POST, [ "jobs"; id; (("retry" | "cancel") as action) ] ->
      with_job id (fun id ->
          mutating (fun () ->
              ignore
                (if action = "retry" then Client.retry t.client id
                 else Client.cancel t.client id);
              redirect (Printf.sprintf "%sjobs/%Ld" base id)))
  | `POST, [ "queues"; q; (("pause" | "resume") as action) ] ->
      Some
        (mutating (fun () ->
             if action = "pause" then Client.pause_queue t.client q
             else Client.resume_queue t.client q;
             redirect base))
  | `GET, [ "metrics" ] ->
      Some
        (respond ~content_type:"text/plain; version=0.0.4" 200
           (Metrics.render ?collector:t.metrics ~stats:(Client.stats t.client)
              ~nodes:(Client.nodes t.client) ()))
  | `GET, [ "healthz" ] -> (
      match Client.stats t.client with
      | _ -> Some (respond ~content_type:"text/plain" 200 "ok")
      | exception e ->
          Some (respond ~content_type:"text/plain" 503 (Printexc.to_string e)))
  | `GET, [ "api"; "stats" ] -> Some (json (stats_json (Client.stats t.client)))
  | `GET, [ "api"; "nodes" ] ->
      Some (json (`List (List.map node_json (Client.nodes t.client))))
  | `GET, [ "api"; "jobs" ] ->
      Some
        (json
           (`List
              (List.map Row.to_json
                 (Client.list t.client (query_of_request req)))))
  | `GET, [ "api"; "jobs"; id ] ->
      with_job id (fun id ->
          match Client.get t.client id with
          | Some r -> json (Row.to_json r)
          | None -> json_error 404 "no such job")
  | `POST, [ "api"; "jobs"; id; (("retry" | "cancel") as action) ] ->
      with_job id (fun id ->
          if t.read_only then json_error 403 "read-only"
          else
            let changed =
              if action = "retry" then Client.retry t.client id
              else Client.cancel t.client id
            in
            json (`Assoc [ ("changed", `Bool changed) ]))
  | `POST, [ "api"; "queues"; q; (("pause" | "resume") as action) ] ->
      if t.read_only then Some (json_error 403 "read-only")
      else (
        if action = "pause" then Client.pause_queue t.client q
        else Client.resume_queue t.client q;
        Some
          (json
             (`Assoc
                [ ("queue", `String q); ("paused", `Bool (action = "pause")) ])))
  | _ -> None

let handle t req =
  let is_api =
    let rel = segments req.path in
    List.mem "api" rel
  in
  let not_found () =
    if is_api then json_error 404 "not found"
    else
      respond 404
        (Views.not_found ~base:t.prefix "Nothing lives at this address.")
  in
  if
    (not (String.ends_with ~suffix:"/healthz" req.path))
    && not (authorized t req)
  then
    respond
      ~headers:
        [ ("www-authenticate", {|Basic realm="Caravan", charset="UTF-8"|}) ]
      ~content_type:"text/plain" 401 "authentication required"
  else if req.meth = `POST && not (same_origin req) then
    respond ~content_type:"text/plain" 403 "cross-origin request refused"
  else
    match route t req with
    | Some r -> r
    | None -> not_found ()
    | exception (Eio.Cancel.Cancelled _ as e) -> raise e
    | exception e ->
        Log.err (fun m ->
            m "error handling %s: %s" req.path (Printexc.to_string e));
        if is_api then json_error 500 "internal error"
        else respond ~content_type:"text/plain" 500 "internal error"

(* --- HTTP server ----------------------------------------------------------------- *)

let security_headers =
  [
    ("x-content-type-options", "nosniff");
    ("x-frame-options", "DENY");
    ("referrer-policy", "same-origin");
    ( "content-security-policy",
      "default-src 'none'; style-src 'unsafe-inline'; form-action 'self'; \
       base-uri 'self'" );
  ]

let callback t _conn (req : Http.Request.t) (_body : Cohttp_eio.Server.body) =
  let uri = Uri.of_string (Http.Request.resource req) in
  let headers = Http.Request.headers req in
  let request =
    {
      meth =
        (match Http.Request.meth req with
        | `GET -> `GET
        | `POST -> `POST
        | `HEAD -> `HEAD
        | _ -> `Other);
      path = Uri.path uri;
      query = Uri.query uri;
      header = Http.Header.get headers;
    }
  in
  let r = handle t request in
  let headers =
    Http.Header.of_list
      (("content-type", r.content_type)
       :: ("cache-control", "no-store")
       :: security_headers
      @ r.headers)
  in
  Cohttp_eio.Server.respond_string ~headers
    ~status:(Http.Status.of_int r.status)
    ~body:r.body ()

let server t = Cohttp_eio.Server.make ~callback:(callback t) ()

let serve ~sw ~net ?(host = Eio.Net.Ipaddr.V4.loopback) ?(port = 4000) ?stop t =
  let socket =
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net (`Tcp (host, port))
  in
  Log.info (fun m -> m "dashboard listening on port %d%s" port t.prefix);
  Cohttp_eio.Server.run ?stop socket (server t) ~on_error:(fun e ->
      Log.warn (fun m -> m "HTTP connection error: %s" (Printexc.to_string e)))
