(** Wrap job execution with cross-cutting behaviour (logging, tracing, metrics,
    error reporting, ...).

    A middleware receives the context and a thunk that runs the rest of the
    chain (and ultimately the job), and returns the outcome. It may inspect or
    replace the outcome, and must call [next] at most once. *)

type t = Ctx.t -> (unit -> Outcome.t) -> Outcome.t

val apply : t list -> Ctx.t -> (unit -> Outcome.t) -> Outcome.t
(** The first middleware in the list is the outermost. *)

val logging : ?src:Logs.src -> unit -> t
(** Logs each attempt's start and outcome with its duration. *)
