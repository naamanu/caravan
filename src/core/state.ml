type t =
  | Available
  | Scheduled
  | Executing
  | Retryable
  | Completed
  | Discarded
  | Cancelled

type event =
  | Stage
  | Claim
  | Complete
  | Retry
  | Discard
  | Snooze
  | Cancel
  | Release
  | Requeue

let all =
  [
    Available; Scheduled; Executing; Retryable; Completed; Discarded; Cancelled;
  ]

let all_events =
  [ Stage; Claim; Complete; Retry; Discard; Snooze; Cancel; Release; Requeue ]

let is_terminal = function
  | Completed | Discarded | Cancelled -> true
  | Available | Scheduled | Executing | Retryable -> false

let is_active s = not (is_terminal s)

let transition state event =
  match (state, event) with
  | (Scheduled | Retryable), Stage -> Ok Available
  | Available, Claim -> Ok Executing
  | Executing, Complete -> Ok Completed
  | Executing, Retry -> Ok Retryable
  | Executing, Discard -> Ok Discarded
  | Executing, Snooze -> Ok Scheduled
  | Executing, Release -> Ok Available
  | s, Cancel when is_active s -> Ok Cancelled
  | (Completed | Discarded | Cancelled | Retryable | Scheduled), Requeue ->
      Ok Available
  | s, e -> Error (`Invalid_transition (s, e))

let to_string = function
  | Available -> "available"
  | Scheduled -> "scheduled"
  | Executing -> "executing"
  | Retryable -> "retryable"
  | Completed -> "completed"
  | Discarded -> "discarded"
  | Cancelled -> "cancelled"

let of_string = function
  | "available" -> Ok Available
  | "scheduled" -> Ok Scheduled
  | "executing" -> Ok Executing
  | "retryable" -> Ok Retryable
  | "completed" -> Ok Completed
  | "discarded" -> Ok Discarded
  | "cancelled" -> Ok Cancelled
  | s -> Error ("unknown job state: " ^ s)

let event_to_string = function
  | Stage -> "stage"
  | Claim -> "claim"
  | Complete -> "complete"
  | Retry -> "retry"
  | Discard -> "discard"
  | Snooze -> "snooze"
  | Cancel -> "cancel"
  | Release -> "release"
  | Requeue -> "requeue"

let pp = Fmt.of_to_string to_string
let pp_event = Fmt.of_to_string event_to_string
let equal = ( = )
