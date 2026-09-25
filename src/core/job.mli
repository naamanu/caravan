(** Job definitions.

    A ['a Job.t] describes a kind of work: its name, how its arguments of type
    ['a] are encoded, which queue it runs on, and the function that performs it.
    Definitions are values; define them once at the top level and share them
    between the code that enqueues and the nodes that execute.

    {[
    type email = { to_ : string; subject : string } [@@deriving yojson]

    let send_email =
      Caravan.Job.make ~name:"send_email" ~queue:"mailers" ~max_attempts:5
        ~codec:(Caravan.Codec.of_yojson email_to_yojson email_of_yojson)
        ~perform:(fun _ctx e ->
          Mailer.send e;
          Caravan.Outcome.Ok)
        ()
    ]}

    Delivery is at-least-once: a job can run more than once (for example if a
    node dies after finishing the work but before recording it), so [perform]
    must be idempotent. *)

type unique =
  | By_args
      (** At most one active job with this name and these exact arguments. *)
  | By_key of string
      (** At most one active job with this name and this key. The key is given
          at definition time; use [?unique_key] on {!Client.enqueue} for
          per-enqueue keys. *)

type 'a t

val make :
  name:string ->
  codec:'a Codec.t ->
  perform:(Ctx.t -> 'a -> Outcome.t) ->
  ?queue:string ->
  ?max_attempts:int ->
  ?priority:int ->
  ?timeout:float ->
  ?backoff:Backoff.t ->
  ?unique:unique ->
  ?tags:string list ->
  unit ->
  'a t
(** Defaults: queue ["default"], [max_attempts] 20, priority 0 (highest), no
    timeout, {!Backoff.default}, not unique.

    [timeout] is in seconds; an attempt that exceeds it is cancelled and counts
    as a failure.

    Raises [Invalid_argument] on an empty name, [max_attempts < 1] or a priority
    outside 0-9. *)

val name : _ t -> string
val queue : _ t -> string
val max_attempts : _ t -> int
val priority : _ t -> int
val timeout : _ t -> float option
val backoff : _ t -> Backoff.t
val codec : 'a t -> 'a Codec.t
val perform : 'a t -> Ctx.t -> 'a -> Outcome.t

val to_insert :
  'a t ->
  ?queue:string ->
  ?priority:int ->
  ?max_attempts:int ->
  ?scheduled_at:Ptime.t ->
  ?meta:Yojson.Safe.t ->
  ?tags:string list ->
  ?unique_key:string ->
  'a ->
  Row.insert
(** Build the insert for one enqueue. Optional arguments override the
    definition's defaults for this job only. *)

type packed = Pack : 'a t -> packed

val pack : 'a t -> packed
val packed_name : packed -> string

val canonical_json : Yojson.Safe.t -> string
(** JSON serialisation with object keys sorted recursively, so equal values
    produce equal strings. Used to derive [By_args] unique keys. *)
