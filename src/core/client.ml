type t = { backend : Backend.t; now : unit -> Ptime.t }

let make ?(now = Ptime_clock.now) backend = { backend; now }
let backend t = t.backend

let add_seconds time s =
  match Ptime.Span.of_float_s s with
  | None -> time
  | Some span -> Option.value (Ptime.add_span time span) ~default:time

let enqueue_many t inserts =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.insert b ~now:(t.now ()) inserts

let enqueue t job ?queue ?priority ?max_attempts ?delay ?at ?meta ?tags
    ?unique_key args =
  let now = t.now () in
  let scheduled_at =
    match (at, delay) with
    | Some at, _ -> Some at
    | None, Some d when d > 0. -> Some (add_seconds now d)
    | None, _ -> None
  in
  let insert =
    Job.to_insert job ?queue ?priority ?max_attempts ?scheduled_at ?meta ?tags
      ?unique_key args
  in
  let (Backend.Backend ((module B), b)) = t.backend in
  match B.insert b ~now [ insert ] with
  | [ r ] -> r
  | _ -> failwith "Client.enqueue: backend returned the wrong number of results"

let get t id =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.get b id

let list t q =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.list b q

let stats t =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.stats b

let nodes t =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.nodes b

let cancel t id =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.cancel b ~now:(t.now ()) id

let retry t id =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.requeue b ~now:(t.now ()) id

let pause_queue t queue =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.set_paused b ~queue true

let resume_queue t queue =
  let (Backend.Backend ((module B), b)) = t.backend in
  B.set_paused b ~queue false
