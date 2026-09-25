(** A persisted job: the record every backend stores. *)

type id = int64

type error = { attempt : int; at : Ptime.t; message : string }
(** One failed attempt, recorded in the job's error history. *)

type t = {
  id : id;
  state : State.t;
  queue : string;
  worker : string;  (** The {!Job.name} used to dispatch this job. *)
  args : Yojson.Safe.t;
  meta : Yojson.Safe.t;
      (** Free-form metadata; never interpreted by Caravan. *)
  tags : string list;
  priority : int;  (** 0 (highest) to 9 (lowest). *)
  attempt : int;  (** Attempts started so far, including the current one. *)
  max_attempts : int;
  errors : error list;  (** Oldest first. *)
  inserted_at : Ptime.t;
  scheduled_at : Ptime.t;  (** When the job becomes (or became) runnable. *)
  attempted_at : Ptime.t option;
  attempted_by : string option;  (** Node id that claimed the latest attempt. *)
  finished_at : Ptime.t option;
      (** Set when the job enters a terminal state. *)
  unique_key : string option;
}

type insert = {
  i_queue : string;
  i_worker : string;
  i_args : Yojson.Safe.t;
  i_meta : Yojson.Safe.t;
  i_tags : string list;
  i_priority : int;
  i_max_attempts : int;
  i_scheduled_at : Ptime.t option;  (** [None] means "now". *)
  i_unique_key : string option;
}
(** A job to be inserted. Build these with {!Job.to_insert} rather than by hand.
*)

type insert_result =
  | Inserted of t
  | Duplicate of t
      (** Another active job already holds the same unique key; this is it. *)

type claim = { claim_id : id; claim_node : string; claim_attempt : int }
(** Proof that a node holds a particular attempt of a job. Backends apply
    outcome transitions only if the job is still [Executing] under the same
    claim, so a node that was presumed dead cannot clobber the job after it has
    been rescued and re-run elsewhere. *)

val claim_of : t -> claim
(** @raise Invalid_argument if the row has never been attempted. *)

val inserted_row : insert_result -> t
val min_priority : int
val max_priority : int
val validate_insert : insert -> (insert, string) result
val to_json : t -> Yojson.Safe.t
val error_to_json : error -> Yojson.Safe.t
val error_of_json : Yojson.Safe.t -> (error, string) result
val pp : t Fmt.t
