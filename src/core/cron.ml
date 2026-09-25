type t = {
  source : string;
  minute : bool array; (* 0..59 *)
  hour : bool array; (* 0..23 *)
  dom : bool array; (* 1..31, index 0 unused *)
  month : bool array; (* 1..12, index 0 unused *)
  dow : bool array; (* 0..6, Sunday = 0 *)
  dom_star : bool;
  dow_star : bool;
}

let ( let* ) = Result.bind

let month_names =
  [
    ("jan", 1);
    ("feb", 2);
    ("mar", 3);
    ("apr", 4);
    ("may", 5);
    ("jun", 6);
    ("jul", 7);
    ("aug", 8);
    ("sep", 9);
    ("oct", 10);
    ("nov", 11);
    ("dec", 12);
  ]

let dow_names =
  [
    ("sun", 0);
    ("mon", 1);
    ("tue", 2);
    ("wed", 3);
    ("thu", 4);
    ("fri", 5);
    ("sat", 6);
  ]

let parse_value ~names ~field s =
  match List.assoc_opt (String.lowercase_ascii s) names with
  | Some v -> Ok v
  | None -> (
      match int_of_string_opt s with
      | Some v when String.length s > 0 && s.[0] <> '-' && s.[0] <> '+' -> Ok v
      | _ -> Error (Printf.sprintf "invalid value %S in %s field" s field))

(* Parse one field into a set of values within [lo, hi]. *)
let parse_field ~field ~lo ~hi ?(names = []) s =
  let set = Array.make (hi + 1) false in
  let check v =
    if v < lo || v > hi then
      Error (Printf.sprintf "%s value %d out of range %d-%d" field v lo hi)
    else Ok v
  in
  let parse_part part =
    let range, step =
      match String.index_opt part '/' with
      | None -> (part, Ok 1)
      | Some i -> (
          let r = String.sub part 0 i in
          let st = String.sub part (i + 1) (String.length part - i - 1) in
          ( r,
            match int_of_string_opt st with
            | Some n when n > 0 -> Ok n
            | _ -> Error (Printf.sprintf "invalid step %S in %s field" st field)
          ))
    in
    let* step = step in
    let* a, b =
      if range = "*" then Ok (lo, hi)
      else
        match String.index_opt range '-' with
        | None ->
            let* v = parse_value ~names ~field range in
            let* v = check v in
            (* "5/15" means "from 5 to the end, every 15". *)
            if String.contains part '/' then Ok (v, hi) else Ok (v, v)
        | Some i ->
            let* a = parse_value ~names ~field (String.sub range 0 i) in
            let* b =
              parse_value ~names ~field
                (String.sub range (i + 1) (String.length range - i - 1))
            in
            let* a = check a in
            let* b = check b in
            if a > b then
              Error (Printf.sprintf "empty range %d-%d in %s field" a b field)
            else Ok (a, b)
    in
    let v = ref a in
    while !v <= b do
      set.(!v) <- true;
      v := !v + step
    done;
    Ok ()
  in
  if s = "" then Error (Printf.sprintf "empty %s field" field)
  else
    let* () =
      List.fold_left
        (fun acc part ->
          let* () = acc in
          if part = "" then
            Error (Printf.sprintf "empty list item in %s field" field)
          else parse_part part)
        (Ok ())
        (String.split_on_char ',' s)
    in
    Ok set

let expand_macro s =
  match String.lowercase_ascii s with
  | "@yearly" | "@annually" -> Ok "0 0 1 1 *"
  | "@monthly" -> Ok "0 0 1 * *"
  | "@weekly" -> Ok "0 0 * * 0"
  | "@daily" | "@midnight" -> Ok "0 0 * * *"
  | "@hourly" -> Ok "0 * * * *"
  | s when String.length s > 0 && s.[0] = '@' ->
      Error ("unknown cron macro " ^ s)
  | _ -> Ok s

let parse source =
  let trimmed = String.trim source in
  let* expr = expand_macro trimmed in
  let fields =
    String.split_on_char ' ' expr
    |> List.concat_map (String.split_on_char '\t')
    |> List.filter (fun f -> f <> "")
  in
  match fields with
  | [ mi; h; dm; mo; dw ] ->
      let* minute = parse_field ~field:"minute" ~lo:0 ~hi:59 mi in
      let* hour = parse_field ~field:"hour" ~lo:0 ~hi:23 h in
      let* dom = parse_field ~field:"day-of-month" ~lo:1 ~hi:31 dm in
      let* month =
        parse_field ~field:"month" ~lo:1 ~hi:12 ~names:month_names mo
      in
      let* dow7 =
        parse_field ~field:"day-of-week" ~lo:0 ~hi:7 ~names:dow_names dw
      in
      let dow = Array.init 7 (fun i -> dow7.(i) || (i = 0 && dow7.(7))) in
      Ok
        {
          source = trimmed;
          minute;
          hour;
          dom;
          month;
          dow;
          (* Vixie cron treats a field as unrestricted if it starts with '*'. *)
          dom_star = dm.[0] = '*';
          dow_star = dw.[0] = '*';
        }
  | _ ->
      Error
        (Printf.sprintf "expected 5 fields in cron expression %S, got %d" source
           (List.length fields))

let parse_exn s =
  match parse s with Ok t -> t | Error e -> invalid_arg ("Cron.parse: " ^ e)

let to_string t = t.source

let day_matches t ~day ~weekday =
  let dom_ok = t.dom.(day) and dow_ok = t.dow.(weekday) in
  if t.dom_star || t.dow_star then dom_ok && dow_ok else dom_ok || dow_ok

let weekday_index date =
  match Ptime.weekday (Option.get (Ptime.of_date date)) with
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6

let matches t time =
  let (y, mo, d), ((h, mi, _), _) = Ptime.to_date_time time in
  t.minute.(mi) && t.hour.(h) && t.month.(mo)
  && day_matches t ~day:d ~weekday:(weekday_index (y, mo, d))

let days_in_month y m =
  match m with
  | 2 -> if (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

let next t ~after =
  (* Walk forward field by field, jumping over whole months/days/hours that
     cannot match. Each step strictly increases the candidate, and the search
     is bounded to five years so impossible expressions terminate. *)
  let (y0, _, _), _ = Ptime.to_date_time after in
  let limit_year = y0 + 5 in
  let rec search y mo d h mi =
    if y > limit_year then None
    else if mo > 12 then search (y + 1) 1 1 0 0
    else if not t.month.(mo) then search y (mo + 1) 1 0 0
    else if d > days_in_month y mo then search y (mo + 1) 1 0 0
    else if not (day_matches t ~day:d ~weekday:(weekday_index (y, mo, d))) then
      search y mo (d + 1) 0 0
    else if h > 23 then search y mo (d + 1) 0 0
    else if not t.hour.(h) then search y mo d (h + 1) 0
    else if mi > 59 then search y mo d (h + 1) 0
    else if not t.minute.(mi) then search y mo d h (mi + 1)
    else Ptime.of_date_time ((y, mo, d), ((h, mi, 0), 0))
  in
  (* Start at the minute after [after], truncated. *)
  let start =
    let (y, mo, d), ((h, mi, _), _) = Ptime.to_date_time after in
    let floored =
      Option.get (Ptime.of_date_time ((y, mo, d), ((h, mi, 0), 0)))
    in
    Option.get (Ptime.add_span floored (Ptime.Span.of_int_s 60))
  in
  let (y, mo, d), ((h, mi, _), _) = Ptime.to_date_time start in
  search y mo d h mi
