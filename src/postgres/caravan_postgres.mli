(** PostgreSQL backend for Caravan.

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let pg =
        Caravan_postgres.connect ~sw ~clock:env#clock
          "postgresql://localhost/myapp"
      in
      ignore (Caravan_postgres.migrate pg);
      let backend = Caravan_postgres.backend pg in
      ...
    ]}

    Workers claim jobs with [SELECT ... FOR UPDATE SKIP LOCKED], so any number
    of nodes can share one database without contending on the same rows. Idle
    producers are woken by [LISTEN]/[NOTIFY] as soon as a job becomes available,
    and fall back to polling if notifications are unavailable (for example
    behind a transaction-mode connection pooler such as PgBouncer).

    Queries use libpq's asynchronous API through Eio, so they never block other
    fibers. *)

module Pg = Pg
include Caravan.Backend.S

val connect :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?pool_size:int ->
  ?listen:bool ->
  string ->
  t
(** [connect ~sw ~clock conninfo] creates a backend with a pool of up to
    [pool_size] (default 10) connections, plus one dedicated [LISTEN] connection
    unless [listen] is [false].

    A node uses one connection per concurrent backend operation; size the pool
    for the number of queues plus some headroom, not for job concurrency.
    [clock] bounds waits for new jobs.

    @raise Pg.Pg_error if the database cannot be reached. *)

val backend : t -> Caravan.Backend.t

val migrate : t -> int list
(** Apply pending schema migrations and return the versions applied. Safe to run
    concurrently from several processes. *)

val schema_version : t -> int
val latest_schema_version : int

(** {1 Transactional enqueue}

    Insert jobs in the same transaction as your own writes, so a job exists if
    and only if the transaction that created it commits. *)

val with_transaction : t -> (Pg.conn -> 'a) -> 'a
(** Run [f] in a transaction on a pooled connection. Use {!Pg.query} for your
    own statements and {!enqueue_in} for jobs. *)

val enqueue_in :
  Pg.conn ->
  ?now:Ptime.t ->
  'a Caravan.Job.t ->
  ?queue:string ->
  ?priority:int ->
  ?max_attempts:int ->
  ?delay:float ->
  ?at:Ptime.t ->
  ?meta:Yojson.Safe.t ->
  ?tags:string list ->
  ?unique_key:string ->
  'a ->
  Caravan.Row.insert_result
(** Like {!Caravan.Client.enqueue}, on a connection you control. *)

(** {1 Testing} *)

val truncate_all : t -> unit
(** Delete every job, node, pause flag and lease, and reset job ids.
    Destructive; meant for test suites. *)
