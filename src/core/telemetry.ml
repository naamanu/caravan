type event =
  | Node_started of { node : string }
  | Node_stopped of { node : string; released : int }
  | Job_started of { node : string; row : Row.t }
  | Job_finished of {
      node : string;
      row : Row.t;
      outcome : Outcome.t;
      duration : float;
      recorded : bool;
    }
  | Leadership of { node : string; leader : bool }
  | Maintenance of { node : string; task : string; count : int }
  | Backend_error of { node : string; operation : string; error : string }

type t = (event -> unit) list Atomic.t

let src = Logs.Src.create "caravan.telemetry" ~doc:"Caravan telemetry"

module Log = (val Logs.src_log src)

let create () = Atomic.make []

let rec attach t h =
  let old = Atomic.get t in
  if not (Atomic.compare_and_set t old (old @ [ h ])) then attach t h

let emit t ev =
  List.iter
    (fun h ->
      try h ev with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | e ->
          Log.err (fun m ->
              m "telemetry handler raised: %s" (Printexc.to_string e)))
    (Atomic.get t)

let log_handler ?(src = Logs.Src.create "caravan.node") () =
  let module L = (val Logs.src_log src) in
  function
  | Node_started { node } -> L.info (fun m -> m "node %s started" node)
  | Node_stopped { node; released } ->
      L.info (fun m -> m "node %s stopped (%d jobs released)" node released)
  | Leadership { node; leader } ->
      L.info (fun m ->
          m "node %s %s leadership" node (if leader then "acquired" else "lost"))
  | Maintenance { node = _; task; count } when count > 0 ->
      L.debug (fun m -> m "%s: %d jobs" task count)
  | Backend_error { node = _; operation; error } ->
      L.err (fun m -> m "backend error during %s: %s" operation error)
  | Maintenance _ | Job_started _ | Job_finished _ -> ()
