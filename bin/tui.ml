open Notty
open Notty.Infix

type focus =
  | Stages
  | Jobs

type ui = {
  spec_name : string;
  mutable focus : focus;
  mutable sel_stage : int;
  mutable sel_job : int;
  mutable sel_id : string option;  (* keeps the selection on the same job as the list moves *)
  mutable scroll : int;
  mutable confirm_quit : bool;
  mutable message : string;
}

let create spec_name = { spec_name; focus = Stages; sel_stage = 0; sel_job = 0; sel_id = None; scroll = 0; confirm_quit = false; message = "" }

let duration secs =
  let s = int_of_float secs in
  if s < 60 then Printf.sprintf "%ds" s
  else if s < 3600 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60)
  else Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)

let bold = A.st A.bold
let dim = A.fg A.(gray 12)
let sel = A.st A.reverse
let red = A.fg A.lightred
let green = A.fg A.lightgreen
let yellow = A.fg A.lightyellow
let cyan = A.fg A.lightcyan

(* exactly [w] columns *)
let text ?(attr = A.empty) w s = I.hsnap ~align:`Left (max 0 w) (I.string attr s)
let rtext ?(attr = A.empty) w s = I.hsnap ~align:`Right (max 0 w) (I.string attr s)

let where (s : Engine.stage) =
  match s.spec.kind with
  | Spec.Copy { src = sh, _; dst = dh, _; move; _ } ->
      let h = function None -> "local" | Some h -> List.hd (String.split_on_char '.' h) in
      Printf.sprintf "%s%s>%s" (h sh) (if move then "=" else "-") (h dh)
  | Spec.Shell { host = None; _ } -> "local"
  | Spec.Shell { host = Some h; _ } -> List.hd (String.split_on_char '.' h)

(* jobs shown for a stage: running, then failed, then recently finished *)
let stage_jobs (s : Engine.stage) = s.running @ s.awaiting @ s.failed @ List.rev (List.of_seq (Queue.to_seq s.recent))

let columns = [ ("stage", 10, false); ("where", 14, false); ("run", 6, true); ("wait", 6, true); ("ahead", 7, true); ("done", 6, true); ("skip", 5, true); ("fail", 4, true); ("avg", 7, true); ("/hour", 6, true) ]

let row ?(attr = A.empty) cells =
  List.map2 (fun (_, w, right) c -> if right then rtext ~attr w c else text ~attr w c) columns cells
  |> List.mapi (fun k i -> if k = 0 then i else I.string attr " " <|> i)
  |> I.hcat

let stage_row (t : Engine.t) ui i (s : Engine.stage) =
  let running = List.length s.running in
  let waiting = Queue.length s.input + Queue.length s.ready + List.length s.checking + s.retrying in
  let ahead =
    match s.spec.ahead with
    | None -> ""
    | Some n ->
        let next = if i + 1 < Array.length t.stages then Engine.backlog t.stages.(i + 1) else 0 in
        Printf.sprintf "%d/%d" (running + next) n
  in
  let cells =
    [
      s.spec.name;
      where s;
      (if s.paused then Printf.sprintf "P %d/%d" running s.jobs else Printf.sprintf "%d/%d" running s.jobs);
      string_of_int waiting;
      ahead;
      string_of_int s.done_;
      string_of_int s.skipped;
      string_of_int (List.length s.failed);
      (match Engine.mean_duration s with Some d -> duration d | None -> "-");
      Printf.sprintf "%.0f" (Engine.rate_per_hour t s);
    ]
  in
  let attr =
    if i = ui.sel_stage then if ui.focus = Stages then sel else bold
    else if s.failed <> [] then red
    else if s.paused then yellow
    else A.empty
  in
  row ~attr cells

let job_line (t : Engine.t) (j : Engine.job) w selected focused =
  let now = Unix.gettimeofday () in
  let glyph, attr, time =
    match j.finished with
    | None -> ("▶", cyan, duration (now -. j.started))
    | Some (at, 0) -> ("✓", green, duration (at -. j.started))
    | Some (at, rc) -> (
        match Engine.retry_at t j.stage j.id with
        | Some retry when List.memq j t.stages.(j.stage).awaiting -> ("↻", yellow, Printf.sprintf "exit %d, retry in %s" rc (duration (Float.max 0. (retry -. now))))
        | _ -> ("✗", red, Printf.sprintf "exit %d %s" rc (duration (at -. j.started))))
  in
  let attempt = match j.kind with Engine.Run n when n > 1 -> Printf.sprintf " #%d" n | _ -> "" in
  let label = j.id ^ attempt in
  let tw = String.length time + 1 in
  let hl = if not selected then A.empty else if focused then sel else A.st A.underline in
  I.string A.(attr ++ hl) (glyph ^ " ") <|> text ~attr:hl (w - 2 - tw) label <|> rtext ~attr:hl tw time

let log_pane (j : Engine.job option) ui w h =
  match j with
  | None -> I.void w h
  | Some j ->
      let lines = Array.of_seq (Queue.to_seq j.log) in
      let n = Array.length lines in
      let h' = max 0 (h - 1) in
      ui.scroll <- max 0 (min ui.scroll (n - h'));
      let last = n - ui.scroll in
      let first = max 0 (last - h') in
      let body = I.vcat (List.init (last - first) (fun k -> text w lines.(first + k))) in
      let state =
        match j.finished with
        | None -> "running " ^ duration (Unix.gettimeofday () -. j.started)
        | Some (at, rc) -> Printf.sprintf "exit %d after %s" rc (duration (at -. j.started))
      in
      let scroll = if ui.scroll > 0 then Printf.sprintf "  [-%d lines]" ui.scroll else "" in
      text ~attr:bold w (Printf.sprintf "%s  %s  %s%s" j.id state (if n = 0 then "(no output yet)" else "") scroll) <-> I.vsnap ~align:`Top h' body

let render (t : Engine.t) ui (w, h) =
  let n = Array.length t.stages in
  ui.sel_stage <- max 0 (min ui.sel_stage (n - 1));
  let s = t.stages.(ui.sel_stage) in
  let jobs = stage_jobs s in
  (match ui.sel_id with
  | Some id -> ( match List.find_index (fun (j : Engine.job) -> j.id = id) jobs with Some k -> ui.sel_job <- k | None -> ())
  | None -> ());
  ui.sel_job <- max 0 (min ui.sel_job (List.length jobs - 1));
  let header =
    let listing = match t.source with Some _ -> " (listing)" | None -> "" in
    let earlier = if t.earlier > 0 then Printf.sprintf " (+%d earlier)" t.earlier else "" in
    let failed = Engine.failures t in
    let state = if Engine.finished t then I.string green "  FINISHED" else I.empty in
    I.string bold ("ferry " ^ ui.spec_name)
    <|> I.string A.empty (Printf.sprintf "   items %d%s   complete %d%s   " t.total listing t.complete earlier)
    <|> I.string (if failed > 0 then red else A.empty) (Printf.sprintf "failed %d" failed)
    <|> I.string dim (Printf.sprintf "   up %s" (duration (Unix.gettimeofday () -. t.started_at)))
    <|> state
  in
  let table = row ~attr:(A.st A.underline) (List.map (fun (c, _, _) -> c) columns) <-> I.vcat (List.mapi (stage_row t ui) (Array.to_list t.stages)) in
  let top = header <-> I.void 1 1 <-> table <-> I.void 1 1 in
  let footer =
    if ui.confirm_quit then I.string (A.st A.bold ++ yellow) "Stop all jobs and quit? (y/n)"
    else
      I.string dim "↑↓ select  tab focus  +/- jobs  p pause  r retry  PgUp/Dn log  q quit"
      <|> I.string yellow (if ui.message = "" then "" else "   " ^ ui.message)
  in
  let mid_h = max 0 (h - I.height top - 1) in
  let left_w = min 48 (w / 2) in
  let right_w = w - left_w - 1 in
  let title =
    text ~attr:bold left_w
      (Printf.sprintf "%s: %d running, %d failed" s.spec.name (List.length s.running) (List.length s.failed))
  in
  let list_h = max 0 (mid_h - 1) in
  (* keep the selected job visible *)
  let first = max 0 (ui.sel_job - list_h + 1) in
  let visible = List.filteri (fun k _ -> k >= first && k < first + list_h) jobs in
  let list = I.vcat (List.mapi (fun k j -> job_line t j left_w (first + k = ui.sel_job) (ui.focus = Jobs)) visible) in
  let left = title <-> I.vsnap ~align:`Top list_h list in
  let job = List.nth_opt jobs ui.sel_job in
  let right = log_pane job ui right_w mid_h in
  let sep = I.char dim '|' 1 mid_h in
  top <-> I.vsnap ~align:`Top mid_h (I.hsnap ~align:`Left left_w left <|> sep <|> right) <-> footer |> I.hsnap ~align:`Left w

(* Returns true to quit. *)
let handle (t : Engine.t) ui (key : Unescape.key) =
  let s = t.stages.(ui.sel_stage) in
  let move d =
    ui.scroll <- 0;
    match ui.focus with
    | Stages ->
        ui.sel_stage <- max 0 (min (Array.length t.stages - 1) (ui.sel_stage + d));
        ui.sel_job <- 0;
        ui.sel_id <- None
    | Jobs ->
        let jobs = stage_jobs s in
        ui.sel_job <- max 0 (min (List.length jobs - 1) (ui.sel_job + d));
        ui.sel_id <- Option.map (fun (j : Engine.job) -> j.id) (List.nth_opt jobs ui.sel_job)
  in
  if ui.confirm_quit then begin
    ui.confirm_quit <- false;
    match key with `ASCII ('y' | 'Y'), _ -> true | _ -> false
  end
  else begin
    (match key with
    | `ASCII ('q' | 'Q'), [] | `ASCII 'C', [ `Ctrl ] -> ui.confirm_quit <- true
    | `Arrow `Up, _ | `ASCII 'k', [] -> move (-1)
    | `Arrow `Down, _ | `ASCII 'j', [] -> move 1
    | `Tab, _ | `Arrow (`Left | `Right), _ -> ui.focus <- (if ui.focus = Stages then Jobs else Stages)
    | `Page `Up, _ -> ui.scroll <- ui.scroll + 10
    | `Page `Down, _ -> ui.scroll <- max 0 (ui.scroll - 10)
    | `End, _ -> ui.scroll <- 0
    | `ASCII ('+' | '='), _ ->
        s.jobs <- s.jobs + 1;
        ui.message <- Printf.sprintf "%s: jobs=%d" s.spec.name s.jobs
    | `ASCII '-', _ ->
        s.jobs <- max 0 (s.jobs - 1);
        ui.message <- Printf.sprintf "%s: jobs=%d" s.spec.name s.jobs
    | `ASCII 'p', [] ->
        s.paused <- not s.paused;
        ui.message <- Printf.sprintf "%s %s" s.spec.name (if s.paused then "paused" else "resumed")
    | `ASCII 'r', [] ->
        let n = List.length s.failed in
        Engine.retry_failed t ui.sel_stage;
        ui.message <- Printf.sprintf "%s: retrying %d failed" s.spec.name n
    | _ -> ());
    false
  end
