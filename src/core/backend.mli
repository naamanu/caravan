(** The storage interface.

    A backend stores job rows and implements every {!State} transition
    atomically. Caravan ships an in-memory backend ({!Memory_backend}) and a
    PostgreSQL backend ([caravan-postgres]); anything that implements {!S} can
    be plugged in.

    Every operation that depends on the time takes it as [~now], supplied by the
    caller's clock. This keeps backends deterministic under test; in production
    run NTP on your nodes (small skew only delays scheduled jobs).

    Operations that record an attempt's outcome take a {!Row.claim} and return
    [false], changing nothing, if the job is no longer [Executing] under that
    claim (for instance because it was rescued from a node presumed dead). *)

type node_info = {
  node : string;
  queues : (string * int) list;  (** Queue name and concurrency. *)
  hostname : string;
  pid : int;
  started_at : Ptime.t;
  heartbeat_at : Ptime.t;
  running : int;  (** Jobs executing on this node at the last heartbeat. *)
}

type query = {
  states : State.t list;  (** Empty means any state. *)
  queues : string list;  (** Empty means any queue. *)
  worker : string option;
  before_id : Row.id option;  (** For paging: only ids strictly below this. *)
  limit : int;
}
(** Listings are ordered newest (highest id) first. *)

val query :
  ?states:State.t list ->
  ?queues:string list ->
  ?worker:string ->
  ?before_id:Row.id ->
  ?limit:int ->
  unit ->
  query
(** [limit] defaults to 50 and is clamped to 1-1000. *)

type stats = {
  counts : (string * State.t * int) list;
      (** Job counts by queue and state. Pairs with a zero count may be omitted.
      *)
  paused : string list;
}

module type S = sig
  type t

  val name : string
  (** A short identifier such as ["memory"] or ["postgres"]. *)

  val insert : t -> now:Ptime.t -> Row.insert list -> Row.insert_result list
  (** Insert jobs, all or nothing, returning results in input order. A job whose
      [scheduled_at] is in the future starts [Scheduled]; otherwise [Available].
      A job whose unique key is held by an active job is not inserted and yields
      [Duplicate existing].
      @raise Invalid_argument if an insert fails {!Row.validate_insert}. *)

  val fetch :
    t -> now:Ptime.t -> queue:string -> limit:int -> node:string -> Row.t list
  (** Atomically claim up to [limit] [Available] jobs from [queue] in priority,
      then [scheduled_at], then id order: each becomes [Executing] with its
      [attempt] incremented and [attempted_by = node]. Two concurrent fetches
      never claim the same job. Returns [[]] if the queue is paused. *)

  val complete : t -> now:Ptime.t -> Row.claim -> bool

  val retry :
    t -> now:Ptime.t -> Row.claim -> error:Row.error -> at:Ptime.t -> bool
  (** [Executing -> Retryable], appending [error], runnable again at [at]. *)

  val discard : t -> now:Ptime.t -> Row.claim -> error:Row.error -> bool

  val snooze : t -> now:Ptime.t -> Row.claim -> at:Ptime.t -> bool
  (** [Executing -> Scheduled] at [at]; the attempt is not counted. *)

  val cancel_claimed : t -> now:Ptime.t -> Row.claim -> error:Row.error -> bool
  (** The job itself asked to be cancelled. *)

  val release : t -> node:string -> int
  (** Return every job [Executing] on [node] to [Available] without counting the
      attempt. Used on graceful shutdown. Returns the number released. *)

  val cancel : t -> now:Ptime.t -> Row.id -> bool
  (** Operator cancel of any active job. A job that is currently executing will
      finish its attempt, but its outcome is then ignored. *)

  val requeue : t -> now:Ptime.t -> Row.id -> bool
  (** Operator retry: make a terminal, scheduled or retryable job [Available]
      now. If it had exhausted its attempts, [max_attempts] is raised by one so
      it gets another try. Returns [false], changing nothing, if the job is
      unique and another active job now holds its key. *)

  val stage : t -> now:Ptime.t -> int
  (** Make due [Scheduled] and [Retryable] jobs [Available]. *)

  val rescue : t -> now:Ptime.t -> nodes:string list -> int
  (** Recover jobs left [Executing] by dead [nodes]. The interrupted attempt
      counts: jobs with attempts left become [Available], the rest [Discarded]
      with an error explaining why. *)

  val prune : t -> before:Ptime.t -> limit:int -> int
  (** Delete up to [limit] terminal jobs that finished before [before]. *)

  val heartbeat : t -> node_info -> unit
  val nodes : t -> node_info list
  val remove_node : t -> string -> unit

  val try_lead : t -> now:Ptime.t -> node:string -> ttl:float -> bool
  (** Acquire or renew the cluster-wide leadership lease for [ttl] seconds.
      Returns whether [node] is the leader. *)

  val resign : t -> node:string -> unit

  val wait_for_jobs : t -> queues:string list -> timeout:float -> unit
  (** Block the calling fiber until jobs may have become available on one of
      [queues], or [timeout] seconds pass. Spurious wakeups are allowed. *)

  val get : t -> Row.id -> Row.t option
  val list : t -> query -> Row.t list
  val stats : t -> stats
  val set_paused : t -> queue:string -> bool -> unit
end

type t =
  | Backend : (module S with type t = 'a) * 'a -> t
      (** A backend packed with its state. *)

val pack : (module S with type t = 'a) -> 'a -> t
val name : t -> string
