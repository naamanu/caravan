open Caravan

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let url_part s = Uri.pct_encode ~component:`Path s
let query_part s = Uri.pct_encode ~component:`Query_value s

let css =
  {css|
:root {
  --bg: #f7f6f2; --panel: #ffffff; --ink: #1d1c1a; --muted: #6b6860;
  --line: #e4e1d8; --accent: #b4541a; --accent-ink: #ffffff;
  --available: #2f6fb0; --scheduled: #7a5fb0; --executing: #b4851a;
  --retryable: #c0621d; --completed: #3b8a4d; --discarded: #b0333a;
  --cancelled: #6b6860;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #161513; --panel: #1f1e1b; --ink: #ece9e1; --muted: #9c988d;
    --line: #34322d; --accent: #e07a3a; --accent-ink: #161513;
    --available: #6aa6e0; --scheduled: #a58fdc; --executing: #e0b24a;
    --retryable: #eb8c4a; --completed: #6cc27f; --discarded: #e6666d;
    --cancelled: #9c988d;
  }
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--bg); color: var(--ink);
  font: 14px/1.5 ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif; }
header { display: flex; align-items: baseline; gap: 24px; padding: 18px 24px;
  border-bottom: 1px solid var(--line); background: var(--panel); flex-wrap: wrap; }
header h1 { margin: 0; font-size: 18px; letter-spacing: .02em; }
header h1 span { color: var(--accent); }
header nav a { color: var(--muted); text-decoration: none; margin-right: 16px; }
header nav a:hover, header nav a.on { color: var(--ink); }
main { padding: 24px; max-width: 1200px; margin: 0 auto; }
h2 { font-size: 13px; text-transform: uppercase; letter-spacing: .08em;
  color: var(--muted); margin: 28px 0 10px; font-weight: 600; }
.panel { background: var(--panel); border: 1px solid var(--line); border-radius: 8px;
  overflow-x: auto; }
table { width: 100%; border-collapse: collapse; font-variant-numeric: tabular-nums; }
th, td { text-align: left; padding: 9px 12px; border-bottom: 1px solid var(--line);
  white-space: nowrap; }
th { font-weight: 600; color: var(--muted); font-size: 12px; }
tr:last-child td { border-bottom: none; }
td.num, th.num { text-align: right; }
td.wrap { white-space: normal; word-break: break-word; }
a { color: var(--accent); }
.state { display: inline-block; padding: 1px 8px; border-radius: 99px; font-size: 12px;
  font-weight: 600; color: var(--panel); }
.s-available { background: var(--available); } .s-scheduled { background: var(--scheduled); }
.s-executing { background: var(--executing); } .s-retryable { background: var(--retryable); }
.s-completed { background: var(--completed); } .s-discarded { background: var(--discarded); }
.s-cancelled { background: var(--cancelled); }
.zero { color: var(--line); }
.muted { color: var(--muted); }
.tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(130px, 1fr)); gap: 12px; }
.tile { background: var(--panel); border: 1px solid var(--line); border-radius: 8px; padding: 12px 14px; }
.tile b { display: block; font-size: 24px; font-variant-numeric: tabular-nums; }
.tile span { color: var(--muted); font-size: 12px; }
form.inline { display: inline; }
button { font: inherit; font-size: 12px; padding: 3px 10px; border-radius: 6px; cursor: pointer;
  border: 1px solid var(--line); background: var(--panel); color: var(--ink); }
button.primary { background: var(--accent); color: var(--accent-ink); border-color: var(--accent); }
pre { background: var(--bg); border: 1px solid var(--line); border-radius: 6px; padding: 10px;
  overflow-x: auto; font-size: 12px; margin: 6px 0 0; white-space: pre-wrap; }
.filters a { margin-right: 10px; }
.pager { margin-top: 12px; }
dl { display: grid; grid-template-columns: max-content 1fr; gap: 6px 18px; margin: 0; padding: 14px; }
dt { color: var(--muted); }
dd { margin: 0; }
|css}

let page ~base ~title ~nav ~refresh body =
  Printf.sprintf
    {html|<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<base href="%s">
%s<title>%s · Caravan</title><style>%s</style></head>
<body><header><h1><span>◆</span> Caravan</h1><nav>%s</nav></header>
<main>%s</main></body></html>|html}
    (esc base)
    (if refresh then {|<meta http-equiv="refresh" content="5">|} else "")
    (esc title) css
    (String.concat ""
       (List.map
          (fun (label, href, on) ->
            Printf.sprintf {|<a href="%s"%s>%s</a>|} href
              (if on then {| class="on"|} else "")
              label)
          [ ("Overview", "./", nav = `Overview); ("Jobs", "jobs", nav = `Jobs) ]))
    body

let state_badge s =
  let s = State.to_string s in
  Printf.sprintf {|<span class="state s-%s">%s</span>|} s s

let time t =
  Ptime.to_rfc3339 ~tz_offset_s:0 t
  |> String.map (function 'T' -> ' ' | c -> c)

let ago ~now t =
  let s = Ptime.Span.to_float_s (Ptime.diff now t) in
  let future = s < 0. in
  let a = Float.abs s in
  let v =
    if a < 60. then Printf.sprintf "%.0fs" a
    else if a < 3600. then Printf.sprintf "%.0fm" (a /. 60.)
    else if a < 86400. then Printf.sprintf "%.1fh" (a /. 3600.)
    else Printf.sprintf "%.1fd" (a /. 86400.)
  in
  if future then "in " ^ v else v ^ " ago"

let button ~read_only ~action ?(primary = false) label =
  if read_only then ""
  else
    Printf.sprintf
      {|<form class="inline" method="post" action="%s"><button%s>%s</button></form>|}
      action
      (if primary then {| class="primary"|} else "")
      label

let overview ~base ~read_only ~now ~(stats : Backend.stats)
    ~(nodes : Backend.node_info list) =
  let states = State.all in
  let count q s =
    List.fold_left
      (fun acc (q', s', n) -> if q = q' && s = s' then acc + n else acc)
      0 stats.counts
  in
  let queues =
    List.sort_uniq String.compare
      (List.map (fun (q, _, _) -> q) stats.counts
      @ stats.paused
      @ List.concat_map
          (fun (n : Backend.node_info) -> List.map fst n.queues)
          nodes)
  in
  let total s = List.fold_left (fun acc q -> acc + count q s) 0 queues in
  let tiles =
    String.concat ""
      (List.map
         (fun s ->
           Printf.sprintf {|<div class="tile"><b>%d</b><span>%s</span></div>|}
             (total s) (State.to_string s))
         states)
  in
  let cell q s =
    let n = count q s in
    if n = 0 then {|<td class="num zero">0</td>|}
    else
      Printf.sprintf
        {|<td class="num"><a href="jobs?queue=%s&amp;state=%s">%d</a></td>|}
        (query_part q) (State.to_string s) n
  in
  let queue_rows =
    if queues = [] then
      {|<tr><td colspan="10" class="muted">No queues yet. Enqueue a job or start a node.</td></tr>|}
    else
      String.concat ""
        (List.map
           (fun q ->
             let paused = List.mem q stats.paused in
             Printf.sprintf "<tr><td><b>%s</b>%s</td>%s<td>%s</td></tr>" (esc q)
               (if paused then {| <span class="muted">(paused)</span>|} else "")
               (String.concat "" (List.map (cell q) states))
               (if paused then
                  button ~read_only ~primary:true
                    ~action:(Printf.sprintf "queues/%s/resume" (url_part q))
                    "Resume"
                else
                  button ~read_only
                    ~action:(Printf.sprintf "queues/%s/pause" (url_part q))
                    "Pause"))
           queues)
  in
  let node_rows =
    if nodes = [] then
      {|<tr><td colspan="6" class="muted">No nodes are running.</td></tr>|}
    else
      String.concat ""
        (List.map
           (fun (n : Backend.node_info) ->
             Printf.sprintf
               {|<tr><td><b>%s</b></td><td>%s:%d</td><td class="wrap">%s</td><td class="num">%d</td><td>%s</td><td>%s</td></tr>|}
               (esc n.node) (esc n.hostname) n.pid
               (esc
                  (String.concat ", "
                     (List.map
                        (fun (q, c) -> Printf.sprintf "%s×%d" q c)
                        n.queues)))
               n.running (ago ~now n.started_at) (ago ~now n.heartbeat_at))
           nodes)
  in
  page ~base ~title:"Overview" ~nav:`Overview ~refresh:true
    (Printf.sprintf
       {|<div class="tiles">%s</div>
<h2>Queues</h2><div class="panel"><table><tr><th>Queue</th>%s<th></th></tr>%s</table></div>
<h2>Nodes</h2><div class="panel"><table><tr><th>Node</th><th>Host</th><th>Queues</th><th class="num">Running</th><th>Started</th><th>Heartbeat</th></tr>%s</table></div>|}
       tiles
       (String.concat ""
          (List.map
             (fun s ->
               Printf.sprintf {|<th class="num">%s</th>|} (State.to_string s))
             states))
       queue_rows node_rows)

let jobs ~base ~now ~(query : Backend.query) (rows : Row.t list) =
  let filter_link label state =
    let current = match query.states with [ s ] -> Some s | _ -> None in
    let qs =
      (match state with
        | Some s -> [ ("state", State.to_string s) ]
        | None -> [])
      @ List.map (fun q -> ("queue", q)) query.queues
    in
    Printf.sprintf {|<a href="jobs%s"%s>%s</a>|}
      (if qs = [] then ""
       else
         "?"
         ^ String.concat "&amp;"
             (List.map (fun (k, v) -> k ^ "=" ^ query_part v) qs))
      (if current = state then {| style="color:var(--ink);font-weight:600"|}
       else "")
      label
  in
  let filters =
    String.concat ""
      (filter_link "all" None
      :: List.map (fun s -> filter_link (State.to_string s) (Some s)) State.all
      )
  in
  let row (r : Row.t) =
    Printf.sprintf
      {|<tr><td><a href="jobs/%Ld">#%Ld</a></td><td>%s</td><td><b>%s</b></td><td>%s</td><td class="num">%d/%d</td><td class="num">%d</td><td>%s</td><td class="wrap muted">%s</td></tr>|}
      r.id r.id (state_badge r.state) (esc r.worker) (esc r.queue) r.attempt
      r.max_attempts r.priority
      (ago ~now
         (match r.state with
         | Completed | Discarded | Cancelled ->
             Option.value r.finished_at ~default:r.inserted_at
         | Executing -> Option.value r.attempted_at ~default:r.inserted_at
         | _ -> r.scheduled_at))
      (esc
         (let s = Yojson.Safe.to_string r.args in
          if String.length s > 80 then String.sub s 0 77 ^ "..." else s))
  in
  let pager =
    match List.rev rows with
    | last :: _ when List.length rows >= query.limit ->
        let qs =
          List.map (fun s -> "state=" ^ State.to_string s) query.states
          @ List.map (fun q -> "queue=" ^ query_part q) query.queues
          @ [ Printf.sprintf "before=%Ld" last.id ]
        in
        Printf.sprintf
          {|<div class="pager"><a href="jobs?%s">Older →</a></div>|}
          (String.concat "&amp;" qs)
    | _ -> ""
  in
  page ~base ~title:"Jobs" ~nav:`Jobs ~refresh:false
    (Printf.sprintf
       {|<div class="filters">%s</div><h2>%d jobs shown</h2>
<div class="panel"><table><tr><th>Id</th><th>State</th><th>Worker</th><th>Queue</th><th class="num">Attempt</th><th class="num">Priority</th><th>When</th><th>Args</th></tr>%s</table></div>%s|}
       filters (List.length rows)
       (if rows = [] then
          {|<tr><td colspan="8" class="muted">No matching jobs.</td></tr>|}
        else String.concat "" (List.map row rows))
       pager)

let job ~base ~read_only ~now (r : Row.t) =
  let opt f = function
    | None -> {|<span class="muted">—</span>|}
    | Some v -> f v
  in
  let t v =
    Printf.sprintf "%s <span class=\"muted\">(%s)</span>" (time v) (ago ~now v)
  in
  let errors =
    if r.errors = [] then {|<p class="muted">No errors recorded.</p>|}
    else
      String.concat ""
        (List.rev_map
           (fun (e : Row.error) ->
             Printf.sprintf
               {|<div style="padding:12px 14px;border-bottom:1px solid var(--line)"><b>Attempt %d</b> <span class="muted">%s</span><pre>%s</pre></div>|}
               e.attempt (time e.at) (esc e.message))
           r.errors)
  in
  let actions =
    (if State.is_active r.state then
       button ~read_only
         ~action:(Printf.sprintf "jobs/%Ld/cancel" r.id)
         "Cancel"
     else "")
    ^
    match r.state with
    | Available | Executing -> ""
    | _ ->
        " "
        ^ button ~read_only ~primary:true
            ~action:(Printf.sprintf "jobs/%Ld/retry" r.id)
            "Retry now"
  in
  page ~base
    ~title:(Printf.sprintf "Job %Ld" r.id)
    ~nav:`Jobs ~refresh:false
    (Printf.sprintf
       {|<h2>Job #%Ld · %s</h2><div style="margin-bottom:12px">%s %s</div>
<div class="panel"><dl>
<dt>Worker</dt><dd>%s</dd><dt>Queue</dt><dd>%s</dd><dt>Priority</dt><dd>%d</dd>
<dt>Attempt</dt><dd>%d of %d</dd><dt>Inserted</dt><dd>%s</dd><dt>Scheduled</dt><dd>%s</dd>
<dt>Attempted</dt><dd>%s</dd><dt>Attempted by</dt><dd>%s</dd><dt>Finished</dt><dd>%s</dd>
<dt>Unique key</dt><dd>%s</dd><dt>Tags</dt><dd>%s</dd>
<dt>Args</dt><dd><pre>%s</pre></dd><dt>Meta</dt><dd><pre>%s</pre></dd>
</dl></div><h2>Errors</h2><div class="panel">%s</div>|}
       r.id (esc r.worker) (state_badge r.state) actions (esc r.worker)
       (esc r.queue) r.priority r.attempt r.max_attempts (t r.inserted_at)
       (t r.scheduled_at) (opt t r.attempted_at) (opt esc r.attempted_by)
       (opt t r.finished_at) (opt esc r.unique_key)
       (if r.tags = [] then {|<span class="muted">—</span>|}
        else esc (String.concat ", " r.tags))
       (esc (Yojson.Safe.pretty_to_string r.args))
       (esc (Yojson.Safe.pretty_to_string r.meta))
       errors)

let not_found ~base what =
  page ~base ~title:"Not found" ~nav:`Overview ~refresh:false
    (Printf.sprintf
       {|<h2>Not found</h2><p>%s</p><p><a href="./">Back to the overview</a></p>|}
       (esc what))
