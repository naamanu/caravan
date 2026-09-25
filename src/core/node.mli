(** A worker node: the process-level runtime that executes jobs.

    Run one node per process, on as many machines as you like, all pointing at
    the same backend. Each node runs:

    - one {b producer} per queue, which claims jobs up to that queue's
      concurrency and runs each in its own fiber, with the job's timeout and the
      middleware chain;
    - a {b heartbeat} that advertises the node as alive;
    - a {b leader} loop. Exactly one node at a time holds a lease and performs
      cluster maintenance: staging scheduled and retryable jobs, rescuing jobs
      from nodes that stopped heartbeating, pruning old finished jobs and
      inserting cron jobs. Every maintenance task is idempotent, so a brief
      overlap between leaders is harmless.

    Stopping is graceful: producers stop claiming, running jobs get
    [shutdown_grace] seconds to finish, then are cancelled and handed back to
    the queue without consuming an attempt. *)

type t

type cron
(** A periodic job. *)

val cron : string -> 'a Job.t -> 'a -> cron
(** [cron "*/5 * * * *" job args] enqueues [job args] at each firing time of the
    expression (see {!Cron}). Each firing is enqueued exactly once across the
    cluster, even across leader changes.
    @raise Invalid_argument if the expression does not parse. *)

type config = {
  node_id : string;
  poll_interval : float;
      (** Upper bound on how long an idle producer waits before polling. *)
  heartbeat_interval : float;
  node_timeout : float;
      (** A node whose last heartbeat is older than this is presumed dead and
          its executing jobs are rescued. Must comfortably exceed
          [heartbeat_interval]. *)
  leader_ttl : float;  (** Length of the leadership lease. *)
  maintenance_interval : float;  (** How often the leader stages jobs. *)
  rescue_interval : float;
  prune_interval : float;
  prune_after : float option;
      (** Delete finished jobs this many seconds after they finish. [None] keeps
          them forever. *)
  prune_batch : int;
  shutdown_grace : float;
}

val default_config : unit -> config
(** Polls every 1s, heartbeats every 5s, rescues after 60s of silence, 15s
    leader lease, prunes finished jobs after 7 days, 30s shutdown grace.
    [node_id] is [hostname-pid-random]. *)

val start :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?config:config ->
  ?middleware:Middleware.t list ->
  ?telemetry:Telemetry.t ->
  ?cron:cron list ->
  ?executor:Eio.Executor_pool.t * string list ->
  Backend.t ->
  jobs:Job.packed list ->
  queues:(string * int) list ->
  t
(** Start a node in [sw] and return immediately.

    [queues] lists the queues this node serves with their concurrency (the
    maximum number of that queue's jobs running at once on this node).

    [executor] is a pool of domains plus the names of queues whose jobs are
    CPU-bound: those jobs' [perform] runs on the pool so they cannot starve the
    node's own fibers (heartbeats, other queues) on the main domain.

    @raise Invalid_argument
      for an empty queue list, a concurrency below 1, or duplicate job names. *)

val stop : t -> unit
(** Stop gracefully and wait until the node has shut down. Idempotent. *)

val await : t -> unit
(** Wait until the node has shut down (after {!stop}). *)

val run_until_signal : t -> unit
(** Block until SIGINT or SIGTERM, then {!stop}. Requires a real Eio backend
    (e.g. [Eio_main]). *)

val id : t -> string

val running : t -> int
(** Jobs currently executing on this node. *)

val is_leader : t -> bool
