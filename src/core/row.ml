type id = int64
type error = { attempt : int; at : Ptime.t; message : string }

type t = {
  id : id;
  state : State.t;
  queue : string;
  worker : string;
  args : Yojson.Safe.t;
  meta : Yojson.Safe.t;
  tags : string list;
  priority : int;
  attempt : int;
  max_attempts : int;
  errors : error list;
  inserted_at : Ptime.t;
  scheduled_at : Ptime.t;
  attempted_at : Ptime.t option;
  attempted_by : string option;
  finished_at : Ptime.t option;
  unique_key : string option;
}

type insert = {
  i_queue : string;
  i_worker : string;
  i_args : Yojson.Safe.t;
  i_meta : Yojson.Safe.t;
  i_tags : string list;
  i_priority : int;
  i_max_attempts : int;
  i_scheduled_at : Ptime.t option;
  i_unique_key : string option;
}

type insert_result = Inserted of t | Duplicate of t
type claim = { claim_id : id; claim_node : string; claim_attempt : int }

let claim_of (r : t) =
  match r.attempted_by with
  | Some node when r.attempt > 0 ->
      { claim_id = r.id; claim_node = node; claim_attempt = r.attempt }
  | _ -> invalid_arg "Row.claim_of: job has not been claimed"

let inserted_row = function Inserted r | Duplicate r -> r
let min_priority = 0
let max_priority = 9

let validate_insert i =
  if String.trim i.i_queue = "" then Error "queue name must not be empty"
  else if String.trim i.i_worker = "" then Error "worker name must not be empty"
  else if i.i_priority < min_priority || i.i_priority > max_priority then
    Error
      (Printf.sprintf "priority %d out of range %d-%d" i.i_priority min_priority
         max_priority)
  else if i.i_max_attempts < 1 then Error "max_attempts must be at least 1"
  else Ok i

let time_json t = `String (Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 t)
let opt f = function None -> `Null | Some x -> f x

let error_to_json (e : error) =
  `Assoc
    [
      ("attempt", `Int e.attempt);
      ("at", time_json e.at);
      ("message", `String e.message);
    ]

let error_of_json = function
  | `Assoc fields -> (
      match
        ( List.assoc_opt "attempt" fields,
          List.assoc_opt "at" fields,
          List.assoc_opt "message" fields )
      with
      | Some (`Int attempt), Some (`String at), Some (`String message) -> (
          match Ptime.of_rfc3339 at with
          | Ok (at, _, _) -> Ok { attempt; at; message }
          | Error _ -> Error ("invalid timestamp in job error: " ^ at))
      | _ -> Error "malformed job error")
  | _ -> Error "job error must be an object"

let to_json r =
  `Assoc
    [
      ("id", `Intlit (Int64.to_string r.id));
      ("state", `String (State.to_string r.state));
      ("queue", `String r.queue);
      ("worker", `String r.worker);
      ("args", r.args);
      ("meta", r.meta);
      ("tags", `List (List.map (fun t -> `String t) r.tags));
      ("priority", `Int r.priority);
      ("attempt", `Int r.attempt);
      ("max_attempts", `Int r.max_attempts);
      ("errors", `List (List.map error_to_json r.errors));
      ("inserted_at", time_json r.inserted_at);
      ("scheduled_at", time_json r.scheduled_at);
      ("attempted_at", opt time_json r.attempted_at);
      ("attempted_by", opt (fun s -> `String s) r.attempted_by);
      ("finished_at", opt time_json r.finished_at);
      ("unique_key", opt (fun s -> `String s) r.unique_key);
    ]

let pp ppf r =
  Fmt.pf ppf "#%Ld %s[%s] %a attempt %d/%d" r.id r.worker r.queue State.pp
    r.state r.attempt r.max_attempts
