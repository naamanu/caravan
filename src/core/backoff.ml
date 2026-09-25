type t =
  | Exponential of { base : float; factor : float; max : float; jitter : float }
  | Linear of { step : float; max : float }
  | Constant of float
  | Custom of (int -> float)

let exponential ?(base = 15.) ?(factor = 2.) ?(max = 86_400.) ?(jitter = 0.1) ()
    =
  Exponential { base; factor; max; jitter = Float.min 1. (Float.max 0. jitter) }

let default = exponential ()

(* Delays are clamped to [0, one year]: a NaN, negative or infinite delay from
   a custom policy must never produce an unrunnable timestamp. *)
let max_delay = 365. *. 86_400.
let sanitize d = if Float.is_nan d || d < 0. then 0. else Float.min d max_delay

let delay ?(rand = fun () -> Random.float 1.) policy ~attempt =
  let attempt = Int.max 1 attempt in
  match policy with
  | Exponential { base; factor; max; jitter } ->
      (* [raw] overflows to infinity for huge attempt counts; cap it first. *)
      let raw = base *. (factor ** float_of_int (attempt - 1)) in
      let capped = if Float.is_finite raw then Float.min raw max else max in
      let scale = 1. -. jitter +. (2. *. jitter *. rand ()) in
      sanitize (capped *. scale)
  | Linear { step; max } ->
      sanitize (Float.min (step *. float_of_int attempt) max)
  | Constant d -> sanitize d
  | Custom f -> sanitize (f attempt)
