(** An in-process backend.

    Jobs live in memory and are lost when the process exits, so this backend is
    for tests, development and single-process tools. It implements the exact
    semantics of {!Backend.S} and serves as the reference model that other
    backends are tested against. It is safe to use from several domains. *)

include Backend.S

val create : clock:_ Eio.Time.clock -> unit -> t
(** [clock] is only used to bound {!wait_for_jobs}. *)

val backend : t -> Backend.t

val all : t -> Row.t list
(** Every stored job, ordered by id. *)
