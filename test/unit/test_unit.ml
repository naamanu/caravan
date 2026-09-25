open Caravan

let ptime ?(sec = 0) (y, mo, d) (h, mi) =
  Option.get (Ptime.of_date_time ((y, mo, d), ((h, mi, sec), 0)))

let ptime_t = Alcotest.testable (Ptime.pp_rfc3339 ()) Ptime.equal
let state_t = Alcotest.testable State.pp State.equal

(* --- State ---------------------------------------------------------------- *)

let state_tests =
  let ok s e expected () =
    Alcotest.(check (result state_t reject))
      (Fmt.str "%a --%a-->" State.pp s State.pp_event e)
      (Ok expected) (State.transition s e)
  in
  let invalid s e () =
    Alcotest.(check bool)
      (Fmt.str "%a --%a--> invalid" State.pp s State.pp_event e)
      true
      (Result.is_error (State.transition s e))
  in
  [
    Alcotest.test_case "happy path" `Quick (fun () ->
        ok Available Claim Executing ();
        ok Executing Complete Completed ();
        ok Scheduled Stage Available ();
        ok Retryable Stage Available ();
        ok Executing Retry Retryable ();
        ok Executing Snooze Scheduled ();
        ok Executing Release Available ());
    Alcotest.test_case "terminal states only requeue" `Quick (fun () ->
        List.iter
          (fun s ->
            List.iter
              (fun e ->
                if e = State.Requeue then ok s e Available ()
                else invalid s e ())
              State.all_events)
          [ State.Completed; Discarded; Cancelled ]);
    Alcotest.test_case "cannot claim twice" `Quick (invalid Executing Claim);
    Alcotest.test_case "string round trip" `Quick (fun () ->
        List.iter
          (fun s ->
            Alcotest.(check (result state_t string))
              "round trip" (Ok s)
              (State.of_string (State.to_string s)))
          State.all);
  ]

let state_props =
  let gen_state =
    QCheck.make ~print:State.to_string (QCheck.Gen.oneof_list State.all)
  in
  let gen_event =
    QCheck.make ~print:State.event_to_string
      (QCheck.Gen.oneof_list State.all_events)
  in
  [
    QCheck.Test.make ~name:"cancel is valid exactly for active states" gen_state
      (fun s -> Result.is_ok (State.transition s Cancel) = State.is_active s);
    QCheck.Test.make ~name:"only Claim enters Executing"
      (QCheck.pair gen_state gen_event) (fun (s, e) ->
        match State.transition s e with
        | Ok Executing -> e = Claim && s = Available
        | _ -> true);
    QCheck.Test.make ~name:"leaving a terminal state requires Requeue"
      (QCheck.pair gen_state gen_event) (fun (s, e) ->
        match State.transition s e with
        | Ok _ when State.is_terminal s -> e = Requeue
        | _ -> true);
  ]

(* --- Backoff -------------------------------------------------------------- *)

let backoff_tests =
  [
    Alcotest.test_case "exponential without jitter" `Quick (fun () ->
        let p =
          Backoff.exponential ~base:10. ~factor:2. ~max:100. ~jitter:0. ()
        in
        let d a = Backoff.delay p ~attempt:a in
        Alcotest.(check (list (float 1e-9)))
          "delays"
          [ 10.; 20.; 40.; 80.; 100.; 100. ]
          (List.map d [ 1; 2; 3; 4; 5; 50 ]));
    Alcotest.test_case "huge attempt does not overflow" `Quick (fun () ->
        let d =
          Backoff.delay Backoff.default ~attempt:100_000 ~rand:(fun () -> 0.5)
        in
        Alcotest.(check (float 1e-6)) "capped" 86_400. d);
    Alcotest.test_case "custom policies are sanitised" `Quick (fun () ->
        let d f = Backoff.delay (Custom f) ~attempt:1 in
        Alcotest.(check (float 0.)) "nan" 0. (d (fun _ -> Float.nan));
        Alcotest.(check (float 0.)) "negative" 0. (d (fun _ -> -5.));
        Alcotest.(check bool)
          "infinite is capped" true
          (Float.is_finite (d (fun _ -> Float.infinity))));
  ]

let backoff_props =
  [
    QCheck.Test.make ~name:"jittered delay stays within bounds"
      QCheck.(
        triple (int_range 1 200) (float_range 0. 1.) (float_bound_exclusive 1.))
      (fun (attempt, jitter, r) ->
        let p = Backoff.exponential ~base:15. ~max:3600. ~jitter () in
        let d = Backoff.delay p ~attempt ~rand:(fun () -> r) in
        let nominal = Float.min 3600. (15. *. (2. ** float (attempt - 1))) in
        d >= (nominal *. (1. -. jitter)) -. 1e-6
        && d <= (nominal *. (1. +. jitter)) +. 1e-6);
  ]

(* --- Cron ----------------------------------------------------------------- *)

let next_of expr after = Cron.next (Cron.parse_exn expr) ~after

let cron_tests =
  let check_next expr after expected () =
    Alcotest.(check (option ptime_t)) expr expected (next_of expr after)
  in
  let base = ptime (2026, 9, 25) (10, 7) in
  [
    Alcotest.test_case "every minute" `Quick
      (check_next "* * * * *" base (Some (ptime (2026, 9, 25) (10, 8))));
    Alcotest.test_case "seconds are truncated" `Quick
      (check_next "* * * * *"
         (ptime ~sec:59 (2026, 9, 25) (10, 7))
         (Some (ptime (2026, 9, 25) (10, 8))));
    Alcotest.test_case "every 15 minutes" `Quick
      (check_next "*/15 * * * *" base (Some (ptime (2026, 9, 25) (10, 15))));
    Alcotest.test_case "hourly macro rolls over day" `Quick
      (check_next "@hourly"
         (ptime (2026, 12, 31) (23, 30))
         (Some (ptime (2027, 1, 1) (0, 0))));
    Alcotest.test_case "weekday names" `Quick
      (* 2026-09-25 is a Friday. *)
      (check_next "30 9 * * mon" base (Some (ptime (2026, 9, 28) (9, 30))));
    Alcotest.test_case "sunday as 7" `Quick
      (check_next "0 0 * * 7" base (Some (ptime (2026, 9, 27) (0, 0))));
    Alcotest.test_case "leap day" `Quick
      (check_next "0 12 29 2 *" base (Some (ptime (2028, 2, 29) (12, 0))));
    Alcotest.test_case "impossible date" `Quick
      (check_next "0 0 30 2 *" base None);
    Alcotest.test_case "dom or dow when both restricted" `Quick
      (* Either field matching suffices: Monday the 28th precedes October 1st. *)
      (check_next "0 0 1 * mon" base (Some (ptime (2026, 9, 28) (0, 0))));
    Alcotest.test_case "ranges, lists and steps" `Quick
      (check_next "5,10-12/2 8-9 * jan-mar *" base
         (Some (ptime (2027, 1, 1) (8, 5))));
    Alcotest.test_case "parse errors" `Quick (fun () ->
        List.iter
          (fun e ->
            Alcotest.(check bool) e true (Result.is_error (Cron.parse e)))
          [
            "";
            "* * * *";
            "60 * * * *";
            "* 24 * * *";
            "* * 0 * *";
            "* * * 13 *";
            "* * * * 8";
            "*/0 * * * *";
            "5-1 * * * *";
            "@often";
            "a * * * *";
            "1,,2 * * * *";
            "-1 * * * *";
          ]);
  ]

let cron_props =
  let field lo hi =
    QCheck.Gen.(
      oneof
        [
          return "*";
          map string_of_int (int_range lo hi);
          map (fun s -> "*/" ^ string_of_int s) (int_range 1 (hi - lo + 1));
          map2
            (fun a b ->
              let a, b = (min a b, max a b) in
              Printf.sprintf "%d-%d" a b)
            (int_range lo hi) (int_range lo hi);
        ])
  in
  let gen_expr =
    QCheck.Gen.(
      map
        (fun (mi, h, dom, mo, dow) -> String.concat " " [ mi; h; dom; mo; dow ])
        (tup5 (field 0 59) (field 0 23) (field 1 28) (field 1 12) (field 0 6)))
  in
  let gen_time =
    QCheck.Gen.(
      map
        (fun s -> Option.get (Ptime.of_float_s (float_of_int s)))
        (int_range 1_600_000_000 1_900_000_000))
  in
  let arb =
    QCheck.make
      ~print:(fun (e, t) -> e ^ " after " ^ Ptime.to_rfc3339 t)
      QCheck.Gen.(pair gen_expr gen_time)
  in
  [
    QCheck.Test.make ~count:300 ~name:"next is the first matching minute" arb
      (fun (expr, after) ->
        let c = Cron.parse_exn expr in
        match Cron.next c ~after with
        | None ->
            (* Legitimate for rare combinations, e.g. "* * */7 1 */7" (a January
               Sunday on the 1st/8th/...) next fires in 2034. *)
            true
        | Some n ->
            let minute = Ptime.Span.of_int_s 60 in
            Ptime.is_later n ~than:after
            && Cron.matches c n
            &&
            (* no earlier matching minute, checked for the first day *)
            let rec clean t k =
              k = 0
              || (not (Ptime.is_earlier t ~than:n))
              || (not (Cron.matches c t))
                 && clean (Option.get (Ptime.add_span t minute)) (k - 1)
            in
            let first =
              let d, ((h, m, _), _) = Ptime.to_date_time after in
              Option.get
                (Ptime.add_span
                   (Option.get (Ptime.of_date_time (d, ((h, m, 0), 0))))
                   minute)
            in
            clean first 1440);
  ]

(* --- Job ------------------------------------------------------------------ *)

let job_tests =
  let echo =
    Job.make ~name:"echo" ~codec:Codec.json ~unique:By_args
      ~perform:(fun _ _ -> Outcome.Ok)
      ()
  in
  [
    Alcotest.test_case "by_args key ignores field order" `Quick (fun () ->
        let k a = (Job.to_insert echo a).i_unique_key in
        Alcotest.(check (option string))
          "same key"
          (k
             (`Assoc
                [
                  ("a", `Int 1);
                  ("b", `List [ `Assoc [ ("y", `Null); ("x", `Null) ] ]);
                ]))
          (k
             (`Assoc
                [
                  ("b", `List [ `Assoc [ ("x", `Null); ("y", `Null) ] ]);
                  ("a", `Int 1);
                ]));
        Alcotest.(check bool)
          "different args differ" true
          (k (`Int 1) <> k (`Int 2)));
    Alcotest.test_case "invalid definitions are rejected" `Quick (fun () ->
        let bad f =
          Alcotest.(check bool)
            "raises" true
            (match f () with
            | _ -> false
            | exception Invalid_argument _ -> true)
        in
        bad (fun () ->
            Job.make ~name:"" ~codec:Codec.unit ~perform:(fun _ _ -> Ok) ());
        bad (fun () ->
            Job.make ~name:"x" ~max_attempts:0 ~codec:Codec.unit
              ~perform:(fun _ _ -> Ok)
              ());
        bad (fun () ->
            Job.make ~name:"x" ~priority:10 ~codec:Codec.unit
              ~perform:(fun _ _ -> Ok)
              ()));
    Alcotest.test_case "registry rejects duplicates" `Quick (fun () ->
        Alcotest.check_raises "dup"
          (Invalid_argument "Registry.create: duplicate job name echo")
          (fun () -> ignore (Registry.create [ Job.pack echo; Job.pack echo ])));
  ]

let () =
  let props name ts = (name, List.map QCheck_alcotest.to_alcotest ts) in
  Alcotest.run "caravan-unit"
    [
      ("state", state_tests);
      props "state-props" state_props;
      ("backoff", backoff_tests);
      props "backoff-props" backoff_props;
      ("cron", cron_tests);
      props "cron-props" cron_props;
      ("job", job_tests);
    ]
