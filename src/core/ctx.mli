(** The execution context passed to a job's [perform] function. *)

type t

val make : row:Row.t -> node:string -> t
val row : t -> Row.t
val id : t -> Row.id

val attempt : t -> int
(** 1 on the first run, 2 on the first retry, ... *)

val max_attempts : t -> int

val is_final_attempt : t -> bool
(** Whether a failure now would discard the job rather than retry it. *)

val queue : t -> string
val worker : t -> string
val meta : t -> Yojson.Safe.t

val node : t -> string
(** The id of the node executing the job. *)

val errors : t -> Row.error list
(** Errors from previous attempts, oldest first. *)
