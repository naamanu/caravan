type t = Ctx.t -> (unit -> Outcome.t) -> Outcome.t

let apply middlewares ctx run =
  let chain = List.fold_right (fun mw next () -> mw ctx next) middlewares run in
  chain ()

let default_src = Logs.Src.create "caravan.job" ~doc:"Caravan job execution"

let logging ?(src = default_src) () : t =
 fun ctx next ->
  let module Log = (val Logs.src_log src) in
  let started = Mtime_clock.counter () in
  Log.debug (fun m ->
      m "start %s#%Ld attempt %d/%d" (Ctx.worker ctx) (Ctx.id ctx)
        (Ctx.attempt ctx) (Ctx.max_attempts ctx));
  let outcome = next () in
  let ms = Mtime.Span.to_float_ns (Mtime_clock.count started) /. 1e6 in
  (match outcome with
  | Outcome.Ok ->
      Log.info (fun m ->
          m "%s#%Ld ok in %.1fms" (Ctx.worker ctx) (Ctx.id ctx) ms)
  | o ->
      Log.warn (fun m ->
          m "%s#%Ld attempt %d %a in %.1fms" (Ctx.worker ctx) (Ctx.id ctx)
            (Ctx.attempt ctx) Outcome.pp o ms));
  outcome
