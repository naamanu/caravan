(** The job lifecycle.

    {v
                 Requeue (operator)
        +----------------------------------------------+
        v                                              |
    Scheduled --Stage--> Available --Claim--> Executing --Complete--> Completed
        ^                  ^   ^                |  |  |
        |                  |   +----Release-----+  |  +--Discard--> Discarded
        +------Snooze------|-----------------------+  |
                           |                          +--Retry--> Retryable
                           +----------Stage--------------------------+
    v}

    Any non-terminal state can be [Cancel]led. Every backend must apply exactly
    these transitions; [transition] is the single source of truth and the
    model-based tests check backends against it. *)

type t =
  | Available  (** Ready to be claimed by a worker. *)
  | Scheduled  (** Waiting for its [scheduled_at] time. *)
  | Executing  (** Claimed by a node and currently running. *)
  | Retryable  (** Failed; will become [Available] at its retry time. *)
  | Completed  (** Finished successfully. Terminal. *)
  | Discarded  (** Failed permanently or was discarded by the job. Terminal. *)
  | Cancelled  (** Cancelled by an operator or by the job. Terminal. *)

type event =
  | Stage  (** A [Scheduled] or [Retryable] job whose time has come. *)
  | Claim  (** A worker takes the job. *)
  | Complete
  | Retry  (** The attempt failed and attempts remain. *)
  | Discard
      (** The attempt failed and no attempts remain, or the job gave up. *)
  | Snooze
      (** The job asked to run again later without consuming an attempt. *)
  | Cancel
  | Release
      (** A node gives the job back: graceful shutdown or a rescued orphan. *)
  | Requeue  (** An operator asks for the job to run again now. *)

val all : t list
val all_events : event list
val is_terminal : t -> bool

val is_active : t -> bool
(** Active jobs are those that participate in uniqueness constraints: every
    non-terminal state. *)

val transition : t -> event -> (t, [ `Invalid_transition of t * event ]) result
val to_string : t -> string
val of_string : string -> (t, string) result
val event_to_string : event -> string
val pp : t Fmt.t
val pp_event : event Fmt.t
val equal : t -> t -> bool
