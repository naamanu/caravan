(** Maps job names to definitions so a node can execute stored rows. *)

type t

val create : Job.packed list -> t
(** @raise Invalid_argument if two definitions share a name. *)

val names : t -> string list

type resolved = {
  run : Ctx.t -> Outcome.t;  (** Decode the arguments and perform the job. *)
  timeout : float option;
  backoff : Backoff.t;
}

val resolve : t -> Row.t -> (resolved, Outcome.t) result
(** [Error outcome] if the row cannot be run here: an unknown worker yields
    [Outcome.Error] (so the job is retried, perhaps on a node running newer
    code), and arguments that fail to decode yield [Outcome.Discard] (retrying
    cannot fix them). *)
