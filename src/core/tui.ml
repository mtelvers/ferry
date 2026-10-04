open Notty
open Notty.Infix

type focus =
  | Stages
  | Jobs

type ui = {
  title : string;
  mutable focus : focus;
  mutable sel_row : int;
  mutable sel_job : int;
  mutable sel_id : string option;  (** keeps the selection on the same job as the list moves *)
  mutable scroll : int;
  mutable confirm_quit : bool;
  mutable message : string;
}

let create title = { title; focus = Stages; sel_row = 0; sel_job = 0; sel_id = None; scroll = 0; confirm_quit = false; message = "" }

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
let magenta = A.fg A.lightmagenta

(* exactly [w] columns *)
let text ?(attr = A.empty) w s = I.hsnap ~align:`Left (max 0 w) (I.string attr s)
let rtext ?(attr = A.empty) w s = I.hsnap ~align:`Right (max 0 w) (I.string attr s)

(* jobs shown for a stage: running, waiting to retry, failed, then recent *)
let stage_jobs (w : Engine.work) = w.running @ w.awaiting @ w.failed @ List.rev (List.of_seq (Queue.to_seq w.recent))

let columns = [ ("stage", 14, false); ("where", 14, false); ("run", 6, true); ("wait", 6, true); ("done", 6, true); ("skip", 5, true); ("fail", 4, true); ("avg", 7, true); ("/hour", 6, true) ]

let row ?(attr = A.empty) cells =
  List.map2 (fun (_, w, right) c -> if right then rtext ~attr w c else text ~attr w c) columns cells |> List.mapi (fun k i -> if k = 0 then i else I.string attr " " <|> i) |> I.hcat

let indent d = String.make (2 * d) ' '

let gate_name (g : Engine.gate) = if Array.length g.entries = 1 then "limit" else "either"

let gate_load (g : Engine.gate) =
  let cap = if g.capacity = max_int then "" else Printf.sprintf "/%d" g.capacity in
  String.concat " " (Array.to_list (Array.mapi (fun i _ -> Printf.sprintf "%d%s" g.inside.(i) cap) g.entries))

let stage_row (t : Engine.t) ui k r =
  let selected = k = ui.sel_row in
  match r with
  | Engine.Row_gate g ->
      let attr = if selected then if ui.focus = Stages then sel else A.(st bold ++ fg lightmagenta) else magenta in
      row ~attr [ indent g.gdepth ^ gate_name g; gate_load g; ""; string_of_int (Queue.length g.queue); ""; ""; ""; ""; "" ]
  | Engine.Row_work w ->
      let running = List.length w.running in
      let waiting = Queue.length w.input + w.retrying in
      let cells =
        [
          indent w.depth ^ w.name;
          Engine.work_label w;
          (if w.paused then Printf.sprintf "P %d/%d" running w.jobs else Printf.sprintf "%d/%d" running w.jobs);
          string_of_int waiting;
          string_of_int w.done_;
          string_of_int w.skipped;
          string_of_int (List.length w.failed);
          (match Engine.mean_duration w with Some d -> duration d | None -> "-");
          Printf.sprintf "%.0f" (Engine.rate_per_hour t w);
        ]
      in
      let attr = if selected then if ui.focus = Stages then sel else bold else if w.failed <> [] then red else if w.paused then yellow else A.empty in
      row ~attr cells

let job_line (t : Engine.t) (j : Engine.job) w selected focused =
  let now = Unix.gettimeofday () in
  let glyph, attr, time =
    match j.finished with
    | None -> ("▶", cyan, duration (now -. j.started))
    | Some (at, 0) -> ("✓", green, duration (at -. j.started))
    | Some (at, rc) -> (
        match Engine.retry_at t j.work j.item with
        | Some retry when List.memq j j.work.awaiting -> ("↻", yellow, Printf.sprintf "exit %d, retry in %s" rc (duration (Float.max 0. (retry -. now))))
        | _ -> ("✗", red, Printf.sprintf "exit %d %s" rc (duration (at -. j.started))))
  in
  let attempt = if j.attempt > 1 then Printf.sprintf " #%d" j.attempt else "" in
  let tw = String.length time + 1 in
  let hl = if not selected then A.empty else if focused then sel else A.st A.underline in
  I.string A.(attr ++ hl) (glyph ^ " ") <|> text ~attr:hl (w - 2 - tw) (j.item.id ^ attempt) <|> rtext ~attr:hl tw time

let log_pane (j : Engine.job option) ui w h =
  match j with
  | None -> I.void w h
  | Some j ->
      let lines = Array.of_seq (Queue.to_seq j.log) in
      let n = Array.length lines in
      let h' = max 0 (h - 2) in
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
      let props =
        Hashtbl.fold (fun k v acc -> if k = "id" then acc else Printf.sprintf "%s=%s" k v :: acc) j.item.props [] |> List.sort compare |> String.concat "  "
      in
      text ~attr:bold w (Printf.sprintf "%s  %s%s%s" j.item.id state (if n = 0 then "  (no output yet)" else "") scroll)
      <-> text ~attr:dim w props
      <-> I.vsnap ~align:`Top h' body

let gate_pane (g : Engine.gate) w h =
  let line i e =
    let cap = if g.capacity = max_int then "no limit" else Printf.sprintf "capacity %d" g.capacity in
    text w (Printf.sprintf "branch %d (%s): %d inside, %s" (i + 1) (Engine.branch_label e) g.inside.(i) cap)
  in
  I.vsnap ~align:`Top h
    (text ~attr:bold w (Printf.sprintf "%s: %d waiting to enter" (gate_name g) (Queue.length g.queue))
    <-> I.vcat (Array.to_list (Array.mapi line g.entries))
    <-> I.void 1 1
    <-> text ~attr:dim w "An item goes to the least loaded branch with room whose first stage has a free slot.")

let render (t : Engine.t) ui (w, h) =
  let rows = Array.of_list t.rows in
  ui.sel_row <- max 0 (min ui.sel_row (Array.length rows - 1));
  let header =
    let listing = match t.src_proc with Some _ -> " (listing)" | None -> "" in
    let earlier = if t.earlier > 0 then Printf.sprintf " (+%d earlier)" t.earlier else "" in
    let failed = Engine.failures t in
    I.string bold ("ferry " ^ ui.title)
    <|> I.string A.empty (Printf.sprintf "   items %d%s   complete %d%s   " t.total listing t.complete earlier)
    <|> I.string (if failed > 0 then red else A.empty) (Printf.sprintf "failed %d" failed)
    <|> I.string dim (Printf.sprintf "   up %s" (duration (Unix.gettimeofday () -. t.started_at)))
    <|> if Engine.finished t then I.string green "  FINISHED" else I.empty
  in
  let table = row ~attr:(A.st A.underline) (List.map (fun (c, _, _) -> c) columns) <-> I.vcat (List.mapi (stage_row t ui) t.rows) in
  let top = header <-> I.void 1 1 <-> table <-> I.void 1 1 in
  let footer =
    if ui.confirm_quit then I.string A.(st bold ++ fg lightyellow) "Stop all jobs and quit? (y/n)"
    else I.string dim "↑↓ select  tab focus  +/- jobs  p pause  r retry  PgUp/Dn log  q quit" <|> I.string yellow (if ui.message = "" then "" else "   " ^ ui.message)
  in
  let mid_h = max 0 (h - I.height top - 1) in
  let left_w = min 48 (w / 2) in
  let right_w = w - left_w - 1 in
  let middle =
    match rows.(ui.sel_row) with
    | Engine.Row_gate g -> gate_pane g w mid_h
    | Engine.Row_work s ->
        let jobs = stage_jobs s in
        (match ui.sel_id with
        | Some id -> ( match List.find_index (fun (j : Engine.job) -> j.item.id = id) jobs with Some k -> ui.sel_job <- k | None -> ())
        | None -> ());
        ui.sel_job <- max 0 (min ui.sel_job (List.length jobs - 1));
        let title = text ~attr:bold left_w (Printf.sprintf "%s: %d running, %d failed" s.key (List.length s.running) (List.length s.failed)) in
        let list_h = max 0 (mid_h - 1) in
        let first = max 0 (ui.sel_job - list_h + 1) in
        let visible = List.filteri (fun k _ -> k >= first && k < first + list_h) jobs in
        let list = I.vcat (List.mapi (fun k j -> job_line t j left_w (first + k = ui.sel_job) (ui.focus = Jobs)) visible) in
        let left = title <-> I.vsnap ~align:`Top list_h list in
        I.hsnap ~align:`Left left_w left <|> I.char dim '|' 1 mid_h <|> log_pane (List.nth_opt jobs ui.sel_job) ui right_w mid_h
  in
  top <-> I.vsnap ~align:`Top mid_h middle <-> footer |> I.hsnap ~align:`Left w

(* Returns true to quit. *)
let handle (t : Engine.t) ui (key : Unescape.key) =
  let rows = Array.of_list t.rows in
  let work = match rows.(ui.sel_row) with Engine.Row_work w -> Some w | Engine.Row_gate _ -> None in
  let move d =
    ui.scroll <- 0;
    match (ui.focus, work) with
    | Jobs, Some w ->
        let jobs = stage_jobs w in
        ui.sel_job <- max 0 (min (List.length jobs - 1) (ui.sel_job + d));
        ui.sel_id <- Option.map (fun (j : Engine.job) -> j.item.id) (List.nth_opt jobs ui.sel_job)
    | _ ->
        ui.focus <- Stages;
        ui.sel_row <- max 0 (min (Array.length rows - 1) (ui.sel_row + d));
        ui.sel_job <- 0;
        ui.sel_id <- None
  in
  let on_work f = match work with Some w -> f w | None -> ui.message <- "select a stage, not a gate" in
  if ui.confirm_quit then begin
    ui.confirm_quit <- false;
    match key with `ASCII ('y' | 'Y'), _ -> true | _ -> false
  end
  else begin
    (match key with
    | `ASCII ('q' | 'Q'), [] | `ASCII 'C', [ `Ctrl ] -> ui.confirm_quit <- true
    | `Arrow `Up, _ | `ASCII 'k', [] -> move (-1)
    | `Arrow `Down, _ | `ASCII 'j', [] -> move 1
    | `Tab, _ | `Arrow (`Left | `Right), _ -> if work <> None then ui.focus <- (if ui.focus = Stages then Jobs else Stages)
    | `Page `Up, _ -> ui.scroll <- ui.scroll + 10
    | `Page `Down, _ -> ui.scroll <- max 0 (ui.scroll - 10)
    | `End, _ -> ui.scroll <- 0
    | `ASCII ('+' | '='), _ ->
        on_work (fun w ->
            w.jobs <- w.jobs + 1;
            ui.message <- Printf.sprintf "%s: jobs=%d" w.key w.jobs)
    | `ASCII '-', _ ->
        on_work (fun w ->
            w.jobs <- max 0 (w.jobs - 1);
            ui.message <- Printf.sprintf "%s: jobs=%d" w.key w.jobs)
    | `ASCII 'p', [] ->
        on_work (fun w ->
            w.paused <- not w.paused;
            ui.message <- Printf.sprintf "%s %s" w.key (if w.paused then "paused" else "resumed"))
    | `ASCII 'r', [] ->
        on_work (fun w ->
            let n = List.length w.failed in
            Engine.retry_failed w;
            ui.message <- Printf.sprintf "%s: retrying %d failed" w.key n)
    | _ -> ());
    false
  end
