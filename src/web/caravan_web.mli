(** A web dashboard, JSON API and Prometheus endpoint for Caravan.

    {[
    let web = Caravan_web.make ~auth:("admin", secret) client in
    Eio.Fiber.fork ~sw (fun () ->
        Caravan_web.serve ~sw ~net:env#net ~port:4000 web)
    ]}

    Routes, relative to [prefix]:
    - [GET /] overview of queues and nodes; [GET /jobs], [GET /jobs/:id]
    - [POST /jobs/:id/retry|cancel], [POST /queues/:q/pause|resume]
    - [GET /api/stats], [GET /api/nodes],
      [GET /api/jobs?state=&queue=&worker=&before=&limit=], [GET /api/jobs/:id],
      [POST /api/jobs/:id/retry|cancel], [POST /api/queues/:q/pause|resume]
    - [GET /metrics] in the Prometheus text format
    - [GET /healthz], which never requires authentication

    When [auth] is set, every other route requires HTTP basic authentication;
    serve it over TLS (e.g. behind a reverse proxy). Cross-origin form posts are
    refused. *)

module Metrics : sig
  type t
  (** Counters and histograms built from {!Caravan.Telemetry} events. *)

  val create : unit -> t

  val attach : t -> Caravan.Telemetry.t -> unit
  (** Record job executions and backend errors of the nodes using this
      telemetry. *)

  val render :
    ?collector:t ->
    stats:Caravan.Backend.stats ->
    nodes:Caravan.Backend.node_info list ->
    unit ->
    string
end

type t

val make :
  ?prefix:string ->
  ?auth:string * string ->
  ?read_only:bool ->
  ?metrics:Metrics.t ->
  ?now:(unit -> Ptime.t) ->
  Caravan.Client.t ->
  t
(** [prefix] (default ["/"]) is the path the dashboard is mounted at.
    [read_only] hides and refuses every mutating action. [metrics] adds this
    process's execution counters to [/metrics]; queue depth gauges are always
    included. *)

(** {1 Serving} *)

val serve :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?host:Eio.Net.Ipaddr.v4v6 ->
  ?port:int ->
  ?stop:'a Eio.Promise.t ->
  t ->
  'a
(** Listen on [host] (default loopback) and [port] (default 4000) until [stop]
    resolves. *)

val server : t -> Cohttp_eio.Server.t
(** For embedding in an existing cohttp-eio server. *)

(** {1 Testing} *)

type request = {
  meth : [ `GET | `POST | `HEAD | `Other ];
  path : string;
  query : (string * string list) list;
  header : string -> string option;
}

type response = {
  status : int;
  content_type : string;
  headers : (string * string) list;
  body : string;
}

val handle : t -> request -> response
(** The whole application as a function, without any networking. *)
