(** A minimal non-blocking PostgreSQL client on libpq and Eio.

    Queries never block the domain: the calling fiber waits on the socket with
    {!Eio_unix.await_readable} while other fibers run. Parameters and results
    use the text protocol. *)

exception Pg_error of { sqlstate : string; message : string; query : string }
(** A query failed on the server, or the connection failed ([sqlstate] is
    ["08000"] for connection errors). *)

type conn

val connect : string -> conn
(** [connect conninfo] opens a connection. [conninfo] is anything libpq accepts:
    ["postgresql://user:pass@host:5432/db"] or ["host=... dbname=..."]. The
    session time zone is set to UTC.
    @raise Pg_error if the connection cannot be established. *)

val close : conn -> unit
val is_usable : conn -> bool
(** [false] once a query on this connection was interrupted or the connection
    failed; such connections must be discarded. *)

type result = {
  rows : string option array array;  (** [None] is SQL NULL. *)
  affected : int;  (** For INSERT/UPDATE/DELETE, the number of rows. *)
}

val query : conn -> ?params:string option list -> string -> result
(** Run one statement.
    @raise Pg_error on failure. *)

val exec : conn -> string -> unit
(** Run one or more statements without parameters (e.g. a migration). *)

val with_transaction : conn -> (unit -> 'a) -> 'a
(** Run [f] inside BEGIN/COMMIT, rolling back if it raises. *)

val listen : conn -> string -> unit

val await_notifications : conn -> Postgresql.Notification.t list
(** Block until at least one notification has arrived on a connection that
    ran {!listen}, and return all that are pending. *)

type pool

val pool : ?size:int -> string -> pool
(** A lazily-filled pool of at most [size] (default 10) connections. *)

val use : pool -> (conn -> 'a) -> 'a
(** Borrow a connection. Unusable connections are replaced transparently. *)

val close_pool : pool -> unit
(** Close every connection the pool opened. Call only when no fiber is using
    the pool any more. *)

val sqlstate_unique_violation : string
