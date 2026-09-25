(** Retry delay policies.

    [delay policy ~attempt] is the number of seconds to wait before retrying a
    job whose attempt number [attempt] (1-based) just failed. *)

type t =
  | Exponential of { base : float; factor : float; max : float; jitter : float }
      (** [base *. factor ** (attempt - 1)], capped at [max], then scaled by a
          random factor in [[1 - jitter, 1 + jitter]]. *)
  | Linear of { step : float; max : float }
      (** [step *. attempt], capped at [max]. *)
  | Constant of float
  | Custom of (int -> float)

val default : t
(** Exponential: 15s, 30s, 60s, ... capped at 24h, with 10% jitter. *)

val exponential :
  ?base:float -> ?factor:float -> ?max:float -> ?jitter:float -> unit -> t

val delay : ?rand:(unit -> float) -> t -> attempt:int -> float
(** [rand] returns a float in [\[0, 1)] and defaults to [Random.float 1.]. The
    result is always within [[0, 1 year]], even for a misbehaving [Custom]
    policy. *)
