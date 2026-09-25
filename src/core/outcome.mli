(** What a job's [perform] function reports back to the runtime. *)

type t =
  | Ok  (** The job succeeded. *)
  | Error of string
      (** The attempt failed. It is retried with backoff until [max_attempts] is
          reached, then discarded. Raising an exception is equivalent. *)
  | Snooze of float
      (** Run again after this many seconds. Does not consume an attempt. *)
  | Cancel of string  (** Stop for good; the job ends [Cancelled]. *)
  | Discard of string
      (** Stop for good; the job ends [Discarded] regardless of attempts left.
      *)

val pp : t Fmt.t

val label : t -> string
(** A short, stable label ("ok", "error", ...) for metrics. *)
