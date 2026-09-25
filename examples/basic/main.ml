(* A single-process tour of Caravan on the in-memory backend.

   dune exec examples/basic/main.exe *)

type email = { to_ : string; subject : string } [@@deriving yojson]

let send_email =
  Caravan.Job.make ~name:"send_email" ~queue:"mailers" ~max_attempts:3
    ~backoff:(Caravan.Backoff.Constant 0.5)
    ~codec:(Caravan.Codec.of_yojson email_to_yojson email_of_yojson)
    ~perform:(fun ctx e ->
      (* Fail the first attempt for one address to show retries. *)
      if e.to_ = "flaky@example.com" && Caravan.Ctx.attempt ctx = 1 then
        Caravan.Outcome.Error "SMTP server said: try again later"
      else (
        Fmt.pr "  sent %S to %s (attempt %d)@." e.subject e.to_
          (Caravan.Ctx.attempt ctx);
        Caravan.Outcome.Ok))
    ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#clock in
  let backend = Caravan.Memory_backend.(backend (create ~clock ())) in
  let client = Caravan.Client.make backend in
  List.iter
    (fun to_ ->
      ignore
        (Caravan.Client.enqueue client send_email { to_; subject = "Welcome!" }))
    [ "ada@example.com"; "flaky@example.com"; "grace@example.com" ];
  ignore
    (Caravan.Client.enqueue client send_email ~delay:2.
       { to_ = "later@example.com"; subject = "Two seconds later" });
  let config = { (Caravan.Node.default_config ()) with poll_interval = 0.2 } in
  let node =
    Caravan.Node.start ~sw ~clock ~config backend
      ~jobs:[ Caravan.Job.pack send_email ]
      ~queues:[ ("mailers", 5) ]
  in
  let rec wait () =
    let s = Caravan.Client.stats client in
    let completed =
      List.fold_left
        (fun n (_, st, c) -> if st = Caravan.State.Completed then n + c else n)
        0 s.counts
    in
    if completed < 4 then (
      Eio.Time.sleep clock 0.1;
      wait ())
  in
  wait ();
  Caravan.Node.stop node;
  Fmt.pr "All 4 emails delivered.@."
