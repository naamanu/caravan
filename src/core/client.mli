(** Enqueue and manage jobs.

    A client is a thin, stateless handle on a backend. Any process can create
    one; it does not need to run a {!Node}. *)

type t

val make : ?now:(unit -> Ptime.t) -> Backend.t -> t
(** [now] defaults to the system wall clock. *)

val backend : t -> Backend.t

val enqueue :
  t ->
  'a Job.t ->
  ?queue:string ->
  ?priority:int ->
  ?max_attempts:int ->
  ?delay:float ->
  ?at:Ptime.t ->
  ?meta:Yojson.Safe.t ->
  ?tags:string list ->
  ?unique_key:string ->
  'a ->
  Row.insert_result
(** Insert one job. It runs as soon as a worker is free, or [delay] seconds from
    now, or at [at] (which wins if both are given). *)

val enqueue_many : t -> Row.insert list -> Row.insert_result list
(** Insert many jobs atomically (build them with {!Job.to_insert}). *)

val get : t -> Row.id -> Row.t option
val list : t -> Backend.query -> Row.t list
val stats : t -> Backend.stats
val nodes : t -> Backend.node_info list
val cancel : t -> Row.id -> bool

val retry : t -> Row.id -> bool
(** Run a finished, failed, scheduled or cancelled job again now. *)

val pause_queue : t -> string -> unit
(** Nodes stop claiming jobs from a paused queue; running jobs finish. *)

val resume_queue : t -> string -> unit
