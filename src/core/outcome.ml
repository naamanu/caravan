type t =
  | Ok
  | Error of string
  | Snooze of float
  | Cancel of string
  | Discard of string

let label = function
  | Ok -> "ok"
  | Error _ -> "error"
  | Snooze _ -> "snooze"
  | Cancel _ -> "cancel"
  | Discard _ -> "discard"

let pp ppf = function
  | Ok -> Fmt.string ppf "ok"
  | Error e -> Fmt.pf ppf "error: %s" e
  | Snooze s -> Fmt.pf ppf "snooze %.3fs" s
  | Cancel r -> Fmt.pf ppf "cancel: %s" r
  | Discard r -> Fmt.pf ppf "discard: %s" r
