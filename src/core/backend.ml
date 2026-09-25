type node_info = {
  node : string;
  queues : (string * int) list;
  hostname : string;
  pid : int;
  started_at : Ptime.t;
  heartbeat_at : Ptime.t;
  running : int;
}

type query = {
  states : State.t list;
  queues : string list;
  worker : string option;
  before_id : Row.id option;
  limit : int;
}

let query ?(states = []) ?(queues = []) ?worker ?before_id ?(limit = 50) () =
  { states; queues; worker; before_id; limit = Int.max 1 (Int.min 1000 limit) }

type stats = { counts : (string * State.t * int) list; paused : string list }

module type S = sig
  type t

  val name : string
  val insert : t -> now:Ptime.t -> Row.insert list -> Row.insert_result list

  val fetch :
    t -> now:Ptime.t -> queue:string -> limit:int -> node:string -> Row.t list

  val complete : t -> now:Ptime.t -> Row.claim -> bool

  val retry :
    t -> now:Ptime.t -> Row.claim -> error:Row.error -> at:Ptime.t -> bool

  val discard : t -> now:Ptime.t -> Row.claim -> error:Row.error -> bool
  val snooze : t -> now:Ptime.t -> Row.claim -> at:Ptime.t -> bool
  val cancel_claimed : t -> now:Ptime.t -> Row.claim -> error:Row.error -> bool
  val release : t -> node:string -> int
  val cancel : t -> now:Ptime.t -> Row.id -> bool
  val requeue : t -> now:Ptime.t -> Row.id -> bool
  val stage : t -> now:Ptime.t -> int
  val rescue : t -> now:Ptime.t -> nodes:string list -> int
  val prune : t -> before:Ptime.t -> limit:int -> int
  val heartbeat : t -> node_info -> unit
  val nodes : t -> node_info list
  val remove_node : t -> string -> unit
  val try_lead : t -> now:Ptime.t -> node:string -> ttl:float -> bool
  val resign : t -> node:string -> unit
  val wait_for_jobs : t -> queues:string list -> timeout:float -> unit
  val get : t -> Row.id -> Row.t option
  val list : t -> query -> Row.t list
  val stats : t -> stats
  val set_paused : t -> queue:string -> bool -> unit
end

type t = Backend : (module S with type t = 'a) * 'a -> t

let pack m b = Backend (m, b)
let name (Backend ((module B), _)) = B.name
