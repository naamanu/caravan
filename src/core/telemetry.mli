(** Runtime events, for metrics, tracing and alerting.

    Handlers run synchronously in the fiber that emits the event, so they must
    be fast and must not block. Exceptions raised by handlers are logged and
    otherwise ignored. *)

type event =
  | Node_started of { node : string }
  | Node_stopped of { node : string; released : int }
  | Job_started of { node : string; row : Row.t }
  | Job_finished of {
      node : string;
      row : Row.t;  (** The row as claimed, before the outcome was recorded. *)
      outcome : Outcome.t;
      duration : float;  (** Seconds. *)
      recorded : bool;
          (** [false] if the outcome was ignored because the job had been
              cancelled or rescued in the meantime. *)
    }
  | Leadership of { node : string; leader : bool }
  | Maintenance of { node : string; task : string; count : int }
      (** [task] is one of ["stage"], ["rescue"], ["prune"], ["cron"]. *)
  | Backend_error of { node : string; operation : string; error : string }

type t

val create : unit -> t
val attach : t -> (event -> unit) -> unit
val emit : t -> event -> unit

val log_handler : ?src:Logs.src -> unit -> event -> unit
(** A handler that logs node-level events (not per-job ones). *)
