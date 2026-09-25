type unique = By_args | By_key of string

type 'a t = {
  name : string;
  codec : 'a Codec.t;
  perform : Ctx.t -> 'a -> Outcome.t;
  queue : string;
  max_attempts : int;
  priority : int;
  timeout : float option;
  backoff : Backoff.t;
  unique : unique option;
  tags : string list;
}

let check_priority where p =
  if p < Row.min_priority || p > Row.max_priority then
    invalid_arg (Printf.sprintf "%s: priority %d out of range 0-9" where p)

let make ~name ~codec ~perform ?(queue = "default") ?(max_attempts = 20)
    ?(priority = 0) ?timeout ?(backoff = Backoff.default) ?unique ?(tags = [])
    () =
  if String.trim name = "" then invalid_arg "Job.make: empty name";
  if String.trim queue = "" then invalid_arg "Job.make: empty queue";
  if max_attempts < 1 then invalid_arg "Job.make: max_attempts must be >= 1";
  check_priority "Job.make" priority;
  (match timeout with
  | Some t when not (t > 0.) -> invalid_arg "Job.make: timeout must be > 0"
  | _ -> ());
  {
    name;
    codec;
    perform;
    queue;
    max_attempts;
    priority;
    timeout;
    backoff;
    unique;
    tags;
  }

let name t = t.name
let queue t = t.queue
let max_attempts t = t.max_attempts
let priority t = t.priority
let timeout t = t.timeout
let backoff t = t.backoff
let codec t = t.codec
let perform t = t.perform

let rec canonicalize : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc fields ->
      `Assoc
        (fields
        |> List.map (fun (k, v) -> (k, canonicalize v))
        |> List.stable_sort (fun (a, _) (b, _) -> String.compare a b))
  | `List xs -> `List (List.map canonicalize xs)
  | j -> j

let canonical_json j = Yojson.Safe.to_string (canonicalize j)

let to_insert t ?queue ?priority ?max_attempts ?scheduled_at ?(meta = `Null)
    ?tags ?unique_key args : Row.insert =
  let args_json = t.codec.encode args in
  let priority = Option.value priority ~default:t.priority in
  check_priority "Job.to_insert" priority;
  let unique_key =
    match (unique_key, t.unique) with
    | Some k, _ -> Some (t.name ^ ":key:" ^ k)
    | None, Some By_args ->
        Some
          (t.name ^ ":args:"
          ^ Digest.to_hex (Digest.string (canonical_json args_json)))
    | None, Some (By_key k) -> Some (t.name ^ ":key:" ^ k)
    | None, None -> None
  in
  {
    i_queue = Option.value queue ~default:t.queue;
    i_worker = t.name;
    i_args = args_json;
    i_meta = meta;
    i_tags = Option.value tags ~default:t.tags;
    i_priority = priority;
    i_max_attempts = Option.value max_attempts ~default:t.max_attempts;
    i_scheduled_at = scheduled_at;
    i_unique_key = unique_key;
  }

type packed = Pack : 'a t -> packed

let pack t = Pack t
let packed_name (Pack t) = t.name
