type t = {
  mutex : Mutex.t;
  jobs : (Row.id, Row.t) Hashtbl.t;
  mutable next_id : Row.id;
  nodes : (string, Backend.node_info) Hashtbl.t;
  paused : (string, unit) Hashtbl.t;
  mutable leader : (string * Ptime.t) option;  (** node, lease expiry *)
  pending : (string, unit) Hashtbl.t;
      (** Queues that received runnable jobs since a waiter last looked. *)
  wakeup : Eio.Condition.t;
  with_timeout : float -> (unit -> unit) -> unit;
}

let name = "memory"

let create ~clock () =
  {
    mutex = Mutex.create ();
    jobs = Hashtbl.create 1024;
    next_id = 1L;
    nodes = Hashtbl.create 8;
    paused = Hashtbl.create 8;
    leader = None;
    pending = Hashtbl.create 8;
    wakeup = Eio.Condition.create ();
    with_timeout =
      (fun secs f ->
        match Eio.Time.with_timeout clock secs (fun () -> Ok (f ())) with
        | Ok () | Error `Timeout -> ());
  }

let locked t f = Mutex.protect t.mutex f

(* Must be called with the lock held. *)
let signal_queue t queue = Hashtbl.replace t.pending queue ()
let notify t = Eio.Condition.broadcast t.wakeup

let add_seconds time s =
  match Ptime.add_span time (Ptime.Span.of_float_s s |> Option.get) with
  | Some t -> t
  | None -> time

let is_active_unique t key =
  Hashtbl.to_seq_values t.jobs
  |> Seq.find (fun (r : Row.t) ->
      r.unique_key = Some key && State.is_active r.state)

let insert t ~now inserts =
  let inserts =
    List.map
      (fun i ->
        match Row.validate_insert i with
        | Ok i -> i
        | Error e -> invalid_arg ("Memory_backend.insert: " ^ e))
      inserts
  in
  let results =
    locked t (fun () ->
        List.map
          (fun (i : Row.insert) ->
            let existing =
              Option.bind i.i_unique_key (fun k -> is_active_unique t k)
            in
            match existing with
            | Some r ->
                (* Mirror PostgreSQL, where a conflicting insert still consumes
                   a sequence value: ids are increasing but may have gaps. *)
                t.next_id <- Int64.succ t.next_id;
                Row.Duplicate r
            | None ->
                let id = t.next_id in
                t.next_id <- Int64.succ id;
                let scheduled_at = Option.value i.i_scheduled_at ~default:now in
                let state : State.t =
                  if Ptime.is_later scheduled_at ~than:now then Scheduled
                  else Available
                in
                let row : Row.t =
                  {
                    id;
                    state;
                    queue = i.i_queue;
                    worker = i.i_worker;
                    args = i.i_args;
                    meta = i.i_meta;
                    tags = i.i_tags;
                    priority = i.i_priority;
                    attempt = 0;
                    max_attempts = i.i_max_attempts;
                    errors = [];
                    inserted_at = now;
                    scheduled_at;
                    attempted_at = None;
                    attempted_by = None;
                    finished_at = None;
                    unique_key = i.i_unique_key;
                  }
                in
                Hashtbl.replace t.jobs id row;
                if state = Available then signal_queue t row.queue;
                Row.Inserted row)
          inserts)
  in
  notify t;
  results

let compare_fetch_order (a : Row.t) (b : Row.t) =
  match Int.compare a.priority b.priority with
  | 0 -> (
      match Ptime.compare a.scheduled_at b.scheduled_at with
      | 0 -> Int64.compare a.id b.id
      | c -> c)
  | c -> c

let fetch t ~now ~queue ~limit ~node =
  if limit <= 0 then []
  else
    locked t (fun () ->
        if Hashtbl.mem t.paused queue then []
        else
          let candidates =
            Hashtbl.to_seq_values t.jobs
            |> Seq.filter (fun (r : Row.t) ->
                r.state = Available && r.queue = queue)
            |> List.of_seq
            |> List.sort compare_fetch_order
            |> List.filteri (fun i _ -> i < limit)
          in
          List.map
            (fun (r : Row.t) ->
              let r =
                {
                  r with
                  state = Executing;
                  attempt = r.attempt + 1;
                  attempted_at = Some now;
                  attempted_by = Some node;
                }
              in
              Hashtbl.replace t.jobs r.id r;
              r)
            candidates)

(* Apply [f] to the job iff it is still executing under [claim]. *)
let with_claim t (claim : Row.claim) f =
  locked t (fun () ->
      match Hashtbl.find_opt t.jobs claim.claim_id with
      | Some r
        when r.state = Executing
             && r.attempted_by = Some claim.claim_node
             && r.attempt = claim.claim_attempt ->
          Hashtbl.replace t.jobs r.id (f r);
          true
      | _ -> false)

let complete t ~now claim =
  with_claim t claim (fun r ->
      { r with state = Completed; finished_at = Some now })

let retry t ~now:_ claim ~error ~at =
  with_claim t claim (fun r ->
      {
        r with
        state = Retryable;
        scheduled_at = at;
        errors = r.errors @ [ error ];
      })

let discard t ~now claim ~error =
  with_claim t claim (fun r ->
      {
        r with
        state = Discarded;
        finished_at = Some now;
        errors = r.errors @ [ error ];
      })

let snooze t ~now:_ claim ~at =
  with_claim t claim (fun r ->
      { r with state = Scheduled; scheduled_at = at; attempt = r.attempt - 1 })

let cancel_claimed t ~now claim ~error =
  with_claim t claim (fun r ->
      {
        r with
        state = Cancelled;
        finished_at = Some now;
        errors = r.errors @ [ error ];
      })

let update_where t pred f =
  let changed =
    locked t (fun () ->
        let targets =
          Hashtbl.to_seq_values t.jobs |> Seq.filter pred |> List.of_seq
        in
        List.iter
          (fun (r : Row.t) ->
            let r' = f r in
            Hashtbl.replace t.jobs r.id r';
            if r'.state = Available then signal_queue t r'.queue)
          targets;
        List.length targets)
  in
  if changed > 0 then notify t;
  changed

let release t ~node =
  update_where t
    (fun r -> r.state = Executing && r.attempted_by = Some node)
    (fun r -> { r with state = Available; attempt = r.attempt - 1 })

let cancel t ~now id =
  update_where t
    (fun r -> r.id = id && State.is_active r.state)
    (fun r -> { r with state = Cancelled; finished_at = Some now })
  > 0

let requeue t ~now id =
  update_where t
    (fun r ->
      r.id = id
      && (match r.state with
         | Completed | Discarded | Cancelled | Retryable | Scheduled -> true
         | Available | Executing -> false)
      &&
      (* Never create a second active job with the same unique key. *)
      match r.unique_key with
      | None -> true
      | Some k -> (
          State.is_active r.state
          ||
          match is_active_unique t k with
          | None -> true
          | Some other -> other.id = r.id))
    (fun r ->
      {
        r with
        state = Available;
        scheduled_at = now;
        finished_at = None;
        max_attempts = Int.max r.max_attempts (r.attempt + 1);
      })
  > 0

let stage t ~now =
  update_where t
    (fun r ->
      (r.state = Scheduled || r.state = Retryable)
      && not (Ptime.is_later r.scheduled_at ~than:now))
    (fun r -> { r with state = Available })

let rescue t ~now ~nodes =
  update_where t
    (fun r ->
      r.state = Executing
      && match r.attempted_by with Some n -> List.mem n nodes | None -> false)
    (fun r ->
      let node = Option.value r.attempted_by ~default:"?" in
      if r.attempt < r.max_attempts then
        { r with state = Available; scheduled_at = now }
      else
        {
          r with
          state = Discarded;
          finished_at = Some now;
          errors =
            r.errors
            @ [
                {
                  attempt = r.attempt;
                  at = now;
                  message =
                    Printf.sprintf
                      "node %s stopped heartbeating while executing this job"
                      node;
                };
              ];
        })

let prune t ~before ~limit =
  locked t (fun () ->
      let victims =
        Hashtbl.to_seq_values t.jobs
        |> Seq.filter (fun (r : Row.t) ->
            State.is_terminal r.state
            &&
            match r.finished_at with
            | Some f -> Ptime.is_earlier f ~than:before
            | None -> false)
        |> Seq.take limit |> List.of_seq
      in
      List.iter (fun (r : Row.t) -> Hashtbl.remove t.jobs r.id) victims;
      List.length victims)

let heartbeat t (info : Backend.node_info) =
  locked t (fun () -> Hashtbl.replace t.nodes info.node info)

let nodes t =
  locked t (fun () ->
      Hashtbl.to_seq_values t.nodes
      |> List.of_seq
      |> List.sort (fun (a : Backend.node_info) b ->
          String.compare a.node b.node))

let remove_node t node = locked t (fun () -> Hashtbl.remove t.nodes node)

let try_lead t ~now ~node ~ttl =
  locked t (fun () ->
      let can_take =
        match t.leader with
        | None -> true
        | Some (holder, expires) ->
            holder = node || not (Ptime.is_later expires ~than:now)
      in
      if can_take then t.leader <- Some (node, add_seconds now ttl);
      can_take)

let resign t ~node =
  locked t (fun () ->
      match t.leader with
      | Some (holder, _) when holder = node -> t.leader <- None
      | _ -> ())

let take_pending t queues =
  locked t (fun () ->
      List.fold_left
        (fun found q ->
          if Hashtbl.mem t.pending q then (
            Hashtbl.remove t.pending q;
            true)
          else found)
        false queues)

let wait_for_jobs t ~queues ~timeout =
  if not (take_pending t queues) then
    t.with_timeout timeout (fun () ->
        Eio.Condition.loop_no_mutex t.wakeup (fun () ->
            if take_pending t queues then Some () else None))

let get t id = locked t (fun () -> Hashtbl.find_opt t.jobs id)

let list t (q : Backend.query) =
  locked t (fun () ->
      Hashtbl.to_seq_values t.jobs
      |> Seq.filter (fun (r : Row.t) ->
          (q.states = [] || List.mem r.state q.states)
          && (q.queues = [] || List.mem r.queue q.queues)
          && (match q.worker with None -> true | Some w -> r.worker = w)
          && match q.before_id with None -> true | Some b -> r.id < b)
      |> List.of_seq
      |> List.sort (fun (a : Row.t) b -> Int64.compare b.id a.id)
      |> List.filteri (fun i _ -> i < q.limit))

let stats t : Backend.stats =
  locked t (fun () ->
      let counts = Hashtbl.create 16 in
      Hashtbl.iter
        (fun _ (r : Row.t) ->
          let k = (r.queue, r.state) in
          Hashtbl.replace counts k
            (1 + Option.value (Hashtbl.find_opt counts k) ~default:0))
        t.jobs;
      {
        Backend.counts =
          Hashtbl.to_seq counts
          |> Seq.map (fun ((q, s), n) -> (q, s, n))
          |> List.of_seq |> List.sort compare;
        paused =
          Hashtbl.to_seq_keys t.paused |> List.of_seq |> List.sort compare;
      })

let set_paused t ~queue paused =
  locked t (fun () ->
      if paused then Hashtbl.replace t.paused queue ()
      else (
        Hashtbl.remove t.paused queue;
        signal_queue t queue));
  if not paused then notify t

let all t =
  locked t (fun () ->
      Hashtbl.to_seq_values t.jobs
      |> List.of_seq
      |> List.sort (fun (a : Row.t) b -> Int64.compare a.id b.id))

module Self = struct
  type nonrec t = t

  let name = name
  let insert = insert
  let fetch = fetch
  let complete = complete
  let retry = retry
  let discard = discard
  let snooze = snooze
  let cancel_claimed = cancel_claimed
  let release = release
  let cancel = cancel
  let requeue = requeue
  let stage = stage
  let rescue = rescue
  let prune = prune
  let heartbeat = heartbeat
  let nodes = nodes
  let remove_node = remove_node
  let try_lead = try_lead
  let resign = resign
  let wait_for_jobs = wait_for_jobs
  let get = get
  let list = list
  let stats = stats
  let set_paused = set_paused
end

let backend t = Backend.pack (module Self) t
