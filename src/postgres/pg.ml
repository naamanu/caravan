module P = Postgresql

exception Pg_error of { sqlstate : string; message : string; query : string }

let () =
  Printexc.register_printer (function
    | Pg_error { sqlstate; message; query = _ } ->
        Some (Printf.sprintf "PostgreSQL error %s: %s" sqlstate message)
    | _ -> None)

let sqlstate_unique_violation = "23505"
let connection_error = "08000"

type conn = { pg : P.connection; mutable usable : bool }

let fail ?(query = "") sqlstate message =
  raise (Pg_error { sqlstate; message = String.trim message; query })

let fd c = c.pg#socket_descr

(* Wrap libpq errors, and mark the connection unusable on any failure or
   cancellation that could leave it mid-protocol. *)
let guarded c ~query f =
  if not c.usable then fail ~query connection_error "connection is not usable";
  match f () with
  | v -> v
  | exception (Pg_error _ as e) -> raise e
  | exception P.Error e ->
      c.usable <- false;
      fail ~query connection_error (P.string_of_error e)
  | exception e ->
      c.usable <- false;
      raise e

let connect_raw conninfo =
  let pg =
    try new P.connection ~conninfo ~startonly:true ()
    with P.Error e -> fail connection_error (P.string_of_error e)
  in
  let c = { pg; usable = true } in
  let rec poll = function
    | P.Polling_ok -> ()
    | Polling_failed ->
        let msg = pg#error_message in
        (try pg#finish with _ -> ());
        fail connection_error ("could not connect: " ^ msg)
    | Polling_reading ->
        Eio_unix.await_readable (fd c);
        poll pg#connect_poll
    | Polling_writing ->
        Eio_unix.await_writable (fd c);
        poll pg#connect_poll
  in
  (match poll P.Polling_writing with
  | () -> ()
  | exception e ->
      (try pg#finish with _ -> ());
      raise e);
  pg#set_nonblocking true;
  pg#set_notice_processing `Quiet;
  c

let close c =
  c.usable <- false;
  try c.pg#finish with _ -> ()

let is_usable c = c.usable && c.pg#status = P.Ok

type result = { rows : string option array array; affected : int }

let rec flush c =
  match c.pg#flush with
  | P.Successful -> ()
  | Data_left_to_send ->
      Eio_unix.await_writable (fd c);
      flush c

let rec await_result c =
  c.pg#consume_input;
  if c.pg#is_busy then (
    Eio_unix.await_readable (fd c);
    await_result c)
  else c.pg#get_result

let convert ~query (r : P.result) =
  match r#status with
  | P.Command_ok | Tuples_ok ->
      let n = r#ntuples and m = r#nfields in
      let rows =
        Array.init n (fun i ->
            Array.init m (fun j ->
                if r#getisnull i j then None else Some (r#getvalue i j)))
      in
      let affected =
        match int_of_string_opt r#cmd_tuples with Some n -> n | None -> 0
      in
      { rows; affected }
  | _ ->
      let sqlstate =
        try r#error_field P.Error_field.SQLSTATE with _ -> "XX000"
      in
      fail ~query sqlstate r#error

(* Drain every result of the current command; report the last one, or the
   first error. *)
let collect c ~query =
  let rec go last err =
    match await_result c with
    | None -> (
        match (err, last) with
        | Some e, _ -> raise e
        | None, Some r -> r
        | None, None -> { rows = [||]; affected = 0 })
    | Some r -> (
        match convert ~query r with
        | v -> go (Some v) err
        | exception (Pg_error _ as e) ->
            go last (if err = None then Some e else err))
  in
  go None None

let query c ?(params = []) sql =
  guarded c ~query:sql (fun () ->
      let params =
        Array.of_list
          (List.map (function None -> P.null | Some s -> s) params)
      in
      c.pg#send_query ~params sql;
      flush c;
      collect c ~query:sql)

let exec c sql =
  guarded c ~query:sql (fun () ->
      c.pg#send_query sql;
      flush c;
      ignore (collect c ~query:sql))

let connect conninfo =
  let c = connect_raw conninfo in
  (* Timestamps are exchanged as epoch seconds, but keep server-side rendering
     (logs, ad-hoc queries) unambiguous too. *)
  (try exec c "SET TIME ZONE 'UTC'"
   with e ->
     close c;
     raise e);
  c

let with_transaction c f =
  ignore (query c "BEGIN");
  match f () with
  | v ->
      ignore (query c "COMMIT");
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (if is_usable c then
         try ignore (Eio.Cancel.protect (fun () -> query c "ROLLBACK"))
         with _ -> c.usable <- false);
      Printexc.raise_with_backtrace e bt

let listen c channel =
  ignore (query c (Printf.sprintf "LISTEN %s" channel))

let rec drain c acc =
  match c.pg#notifies with Some n -> drain c (n :: acc) | None -> List.rev acc

let rec await_notifications c =
  let pending =
    guarded c ~query:"LISTEN" (fun () ->
        c.pg#consume_input;
        drain c [])
  in
  if pending <> [] then pending
  else (
    Eio_unix.await_readable (fd c);
    await_notifications c)

type pool = { pool : conn Eio.Pool.t; conns : conn list ref; mutex : Mutex.t }

let pool ?(size = 10) conninfo =
  let conns = ref [] and mutex = Mutex.create () in
  let track c = Mutex.protect mutex (fun () -> conns := c :: !conns) in
  let untrack c =
    Mutex.protect mutex (fun () -> conns := List.filter (fun c' -> c' != c) !conns)
  in
  {
    conns;
    mutex;
    pool =
      Eio.Pool.create size
        ~validate:is_usable
        ~dispose:(fun c ->
          untrack c;
          close c)
        (fun () ->
          let c = connect conninfo in
          track c;
          c);
  }

let use p f = Eio.Pool.use p.pool f

let close_pool p =
  let conns = Mutex.protect p.mutex (fun () -> let l = !(p.conns) in p.conns := []; l) in
  List.iter close conns
