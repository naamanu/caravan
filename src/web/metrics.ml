open Caravan

let buckets =
  [|
    0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5; 5.; 10.; 30.; 60.; 300.;
  |]

type histogram = {
  counts : int array;
  mutable sum : float;
  mutable total : int;
}

type t = {
  mutex : Mutex.t;
  executions : (string * string * string, int) Hashtbl.t;
      (** queue, worker, outcome -> count *)
  durations : (string * string, histogram) Hashtbl.t;  (** queue, worker *)
  backend_errors : (string, int) Hashtbl.t;  (** operation -> count *)
}

let create () =
  {
    mutex = Mutex.create ();
    executions = Hashtbl.create 64;
    durations = Hashtbl.create 64;
    backend_errors = Hashtbl.create 8;
  }

let incr tbl k =
  Hashtbl.replace tbl k (1 + Option.value (Hashtbl.find_opt tbl k) ~default:0)

let observe t (row : Row.t) outcome duration =
  Mutex.protect t.mutex (fun () ->
      incr t.executions (row.queue, row.worker, Outcome.label outcome);
      let h =
        match Hashtbl.find_opt t.durations (row.queue, row.worker) with
        | Some h -> h
        | None ->
            let h =
              {
                counts = Array.make (Array.length buckets) 0;
                sum = 0.;
                total = 0;
              }
            in
            Hashtbl.replace t.durations (row.queue, row.worker) h;
            h
      in
      Array.iteri
        (fun i b -> if duration <= b then h.counts.(i) <- h.counts.(i) + 1)
        buckets;
      h.sum <- h.sum +. duration;
      h.total <- h.total + 1)

let attach t telemetry =
  Telemetry.attach telemetry (function
    | Telemetry.Job_finished { row; outcome; duration; _ } ->
        observe t row outcome duration
    | Backend_error { operation; _ } ->
        Mutex.protect t.mutex (fun () -> incr t.backend_errors operation)
    | _ -> ())

(* --- Prometheus text exposition format ----------------------------------- *)

let escape v =
  let b = Buffer.create (String.length v) in
  String.iter
    (function
      | '\\' -> Buffer.add_string b "\\\\"
      | '"' -> Buffer.add_string b "\\\""
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    v;
  Buffer.contents b

let labels l =
  if l = [] then ""
  else
    "{"
    ^ String.concat ","
        (List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k (escape v)) l)
    ^ "}"

let float_str f =
  if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
  else Printf.sprintf "%g" f

let render ?collector ~(stats : Backend.stats) ~(nodes : Backend.node_info list)
    () =
  let b = Buffer.create 4096 in
  let header name kind help =
    Printf.bprintf b "# HELP %s %s\n# TYPE %s %s\n" name help name kind
  in
  let sample name l v = Printf.bprintf b "%s%s %s\n" name (labels l) v in
  header "caravan_jobs" "gauge" "Jobs currently stored, by queue and state.";
  List.iter
    (fun (q, s, n) ->
      sample "caravan_jobs"
        [ ("queue", q); ("state", State.to_string s) ]
        (string_of_int n))
    stats.counts;
  header "caravan_queue_paused" "gauge" "1 if the queue is paused.";
  let queues =
    List.sort_uniq String.compare
      (List.map (fun (q, _, _) -> q) stats.counts @ stats.paused)
  in
  List.iter
    (fun q ->
      sample "caravan_queue_paused"
        [ ("queue", q) ]
        (if List.mem q stats.paused then "1" else "0"))
    queues;
  header "caravan_nodes" "gauge" "Nodes that have heartbeated recently.";
  sample "caravan_nodes" [] (string_of_int (List.length nodes));
  header "caravan_node_running_jobs" "gauge" "Jobs executing on each node.";
  List.iter
    (fun (n : Backend.node_info) ->
      sample "caravan_node_running_jobs"
        [ ("node", n.node) ]
        (string_of_int n.running))
    nodes;
  (match collector with
  | None -> ()
  | Some t ->
      Mutex.protect t.mutex (fun () ->
          header "caravan_job_executions_total" "counter"
            "Job attempts executed by this process, by outcome.";
          Hashtbl.to_seq t.executions
          |> List.of_seq |> List.sort compare
          |> List.iter (fun ((q, w, o), n) ->
              sample "caravan_job_executions_total"
                [ ("queue", q); ("worker", w); ("outcome", o) ]
                (string_of_int n));
          header "caravan_job_duration_seconds" "histogram"
            "Job attempt duration in seconds.";
          Hashtbl.to_seq t.durations |> List.of_seq |> List.sort compare
          |> List.iter (fun ((q, w), h) ->
              let l = [ ("queue", q); ("worker", w) ] in
              Array.iteri
                (fun i bound ->
                  sample "caravan_job_duration_seconds_bucket"
                    (l @ [ ("le", float_str bound) ])
                    (string_of_int h.counts.(i)))
                buckets;
              sample "caravan_job_duration_seconds_bucket"
                (l @ [ ("le", "+Inf") ])
                (string_of_int h.total);
              sample "caravan_job_duration_seconds_sum" l
                (Printf.sprintf "%.6f" h.sum);
              sample "caravan_job_duration_seconds_count" l
                (string_of_int h.total));
          header "caravan_backend_errors_total" "counter"
            "Failed backend operations in this process.";
          Hashtbl.to_seq t.backend_errors
          |> List.of_seq |> List.sort compare
          |> List.iter (fun (op, n) ->
              sample "caravan_backend_errors_total"
                [ ("operation", op) ]
                (string_of_int n))));
  Buffer.contents b
