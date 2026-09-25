(** Cron expressions.

    Standard five-field syntax, [minute hour day-of-month month day-of-week],
    evaluated in UTC. Each field accepts [*], numbers, ranges [a-b], steps [*/n]
    and [a-b/n], and comma-separated lists. Months accept [jan]..[dec] and days
    of week accept [sun]..[sat]; day-of-week [7] is Sunday. The macros
    [@yearly], [@annually], [@monthly], [@weekly], [@daily], [@midnight] and
    [@hourly] are supported.

    As in Vixie cron, when both day-of-month and day-of-week are restricted
    (neither is [*]), a day matches if {e either} field matches. *)

type t

val parse : string -> (t, string) result
val parse_exn : string -> t

val to_string : t -> string
(** The original expression. *)

val matches : t -> Ptime.t -> bool
(** Whether the minute containing this instant is a firing time. *)

val next : t -> after:Ptime.t -> Ptime.t option
(** The first firing time strictly after [after], truncated to the minute.
    [None] if there is none within the next five years (e.g. [0 0 30 2 *]). *)
