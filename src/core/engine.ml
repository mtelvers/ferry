(* The scheduler, over a graph built from a [Plan.t].

   Work nodes run a command per item; each has
     input  - items waiting for their skip check
     ready  - items waiting for a job slot
   Gates split the flow into branches and rejoin it. A gate admits an item
   into a branch only when that branch has fewer than [capacity] items inside
   and its first stage has a free slot, so the gate's queue - not the first
   stage's - is where items wait. With several branches, the item goes to the
   least loaded one, so faster branches take more items.

   Commands, skip checks and paths are functions of the item (its ID, and
   properties set by earlier stages printing "ferry-set NAME=VALUE").
   Completed stages are journalled with the item's properties, so a restart
   resumes each item at the same place - on the same branch. *)

open Plan

type item = {
  id : string;
  props : (string, string) Hashtbl.t;
  mutable gates : (gate * int) list;  (** gates entered, innermost first *)
}

and kind =
  | Check
  | Run of int

and job = {
  work : work;
  item : item;
  kind : kind;
  proc : Proc.t;
  started : float;
  log : string Queue.t;
  mutable finished : (float * int) option;
  mutable cancelled : bool;
}

and work = {
  name : string;
  mutable key : string;
  step : Plan.step;
  opts : opts;
  depth : int;
  enclosing : (gate * int) list;
  mutable next : node;
  input : item Queue.t;
  ready : item Queue.t;
  mutable jobs : int;
  mutable paused : bool;
  mutable checking : job list;
  mutable running : job list;
  mutable retrying : int;
  mutable awaiting : job list;  (** failed attempts waiting to retry *)
  mutable done_ : int;
  mutable skipped : int;
  mutable failed : job list;
  recent : job Queue.t;
  durations : float Queue.t;
  finishes : float Queue.t;
  attempts : (string, int) Hashtbl.t;
  mutable log_oc : out_channel;
}

and gate = {
  capacity : int;
  gdepth : int;
  mutable entries : node array;
  mutable inside : int array;
  queue : item Queue.t;
  mutable after : node;
}

and node =
  | Work of work
  | Gate of gate
  | Exit of gate * int
  | Finish

type row =
  | Row_work of work
  | Row_gate of gate

type t = {
  rows : row list;
  works : work array;
  entry : node;
  source : Plan.source;
  reach : string -> string -> bool;
  journal : out_channel;
  resume : (string, string * (string * string) list) Hashtbl.t;
  finished_ids : (string, unit) Hashtbl.t;
  seen : (string, unit) Hashtbl.t;
  mutable src_proc : Proc.t option;
  intake : item Queue.t;
  mutable intake_checks : (item * Proc.t) list;
  mutable total : int;
  mutable complete : int;
  mutable earlier : int;
  mutable timers : (float * work * item) list;  (** retry at, stage, item *)
  notify : string -> unit;
  started_at : float;
}

let log_lines = 2000
let recent_jobs = 50
let intake_checks = 8

exception Error of string

let strip_slash p =
  let rec strip p = if String.length p > 1 && p.[String.length p - 1] = '/' then strip (String.sub p 0 (String.length p - 1)) else p in
  strip p

let parent path =
  let p = strip_slash path in
  match String.rindex_opt p '/' with Some 0 -> "/" | Some i -> String.sub p 0 i | None -> "."

let short h = List.hd (String.split_on_char '.' h)
let ( let* ) = Result.bind

(* ---- building the graph ---- *)

let new_work step ~depth ~enclosing ~next =
  let name, opts = match step with Shell { name; opts; _ } | Copy { name; opts; _ } -> (name, opts) | Gate _ -> assert false in
  {
    name;
    key = name;
    step;
    opts;
    depth;
    enclosing;
    next;
    input = Queue.create ();
    ready = Queue.create ();
    jobs = opts.jobs;
    paused = false;
    checking = [];
    running = [];
    retrying = 0;
    awaiting = [];
    done_ = 0;
    skipped = 0;
    failed = [];
    recent = Queue.create ();
    durations = Queue.create ();
    finishes = Queue.create ();
    attempts = Hashtbl.create 64;
    log_oc = stderr;
  }

let rec chain steps ~depth ~enclosing ~next =
  match steps with
  | [] -> next
  | step :: rest -> (
      let rest_entry = chain rest ~depth ~enclosing ~next in
      match step with
      | Shell _ | Copy _ -> Work (new_work step ~depth ~enclosing ~next:rest_entry)
      | Gate { capacity; branches } ->
          let g = { capacity; gdepth = depth; entries = [||]; inside = [||]; queue = Queue.create (); after = rest_entry } in
          g.entries <- Array.of_list (List.mapi (fun i b -> chain b ~depth:(depth + 1) ~enclosing:((g, i) :: enclosing) ~next:(Exit (g, i))) branches);
          g.inside <- Array.make (Array.length g.entries) 0;
          Gate g)

let rec rows_of node acc =
  match node with
  | Work w -> rows_of w.next (Row_work w :: acc)
  | Gate g -> rows_of g.after (Array.fold_left (fun acc e -> rows_of e acc) (Row_gate g :: acc) g.entries)
  | Exit _ | Finish -> acc

let where_label = function Local -> "local" | At h -> short h

let work_label w =
  match w.step with
  | Shell { where; _ } -> where_label where
  | Copy { src = sw, _; dst = dw, _; move; _ } -> Printf.sprintf "%s%s>%s" (where_label sw) (if move then "=" else "-") (where_label dw)
  | Gate _ -> ""

let branch_label = function Work w -> work_label w | Gate _ -> "gate" | Exit _ | Finish -> "-"

(* ---- calling the item functions ---- *)

let host_of = function At h -> Some h | Local -> None

let view ?out item = { Plan.id = item.id; props = item.props; out }

(* A user function can raise; that fails the job, with the reason as its log. *)
let call f v = try Ok (f v) with Failure m | Invalid_argument m -> Error m | e -> Error (Printexc.to_string e)

let remote host path = match host with None -> path | Some h -> h ^ ":" ^ path

(* rsync from (sh, sp) to (dh, dp), run on whichever side can reach the
   other. A per-item destination is filled under a temporary name first. *)
let copy_script t ~key ~move ~per_item (sh, sp) (dh, dp) =
  let target, finish =
    if per_item then
      let final = strip_slash dp in
      let tmp = Printf.sprintf "%s/.ferry-%s-%s" (parent final) key (Filename.basename final) in
      (tmp, Printf.sprintf "rm -rf %s && mv %s %s" final tmp final)
    else (dp, "")
  in
  let rm_src = if move then " --remove-source-files" else "" in
  let tidy = Printf.sprintf "{ test ! -d %s || find %s -depth -type d -empty -delete; }" sp sp in
  let on host cmd = match host with Some h -> Printf.sprintf "ssh -o BatchMode=yes %s %s" h (Filename.quote cmd) | None -> cmd in
  let steps l = String.concat " && " (List.filter (( <> ) "") l) in
  let pull runner =
    let tidy = if not move then "" else if sh = runner then tidy else on sh tidy in
    Ok (runner, steps [ "mkdir -p " ^ parent target; Printf.sprintf "rsync -a%s %s %s" rm_src (if sh = runner then sp else remote sh sp) target; finish; tidy ])
  in
  let push runner =
    let rsync_path = Filename.quote (Printf.sprintf "mkdir -p %s && rsync" (parent target)) in
    Ok
      ( runner,
        steps
          [
            Printf.sprintf "rsync -a%s --rsync-path=%s %s %s" rm_src rsync_path sp (remote dh target);
            (if finish = "" then "" else on dh finish);
            (if move then tidy else "");
          ] )
  in
  if sh = dh then pull sh
  else
    match (sh, dh) with
    | None, Some _ -> push None
    | Some _, None -> pull None
    | Some a, Some b -> if t.reach b a then pull dh else if t.reach a b then push sh else Error (Printf.sprintf "neither %s nor %s can ssh to the other" a b)
    | None, None -> assert false

let has_skip w = match w.step with Shell { skip = Some _; _ } | Copy { skip = Some _; _ } -> true | _ -> false

(* A destination is per item - safe to fill under a temporary name and
   rename into place - when its last component contains the item's ID. *)
let per_item item dp =
  let base = Filename.basename (strip_slash dp) in
  let n = String.length item.id in
  let rec has i = i + n <= String.length base && (String.sub base i n = item.id || has (i + 1)) in
  n > 0 && has 0

(* For one item: the host to run on, and the script. *)
let script t w item kind =
  match (w.step, kind) with
  | Shell { where; skip = Some skip; _ }, Check ->
      let* s = call skip (view item) in
      Ok (host_of where, s)
  | Copy { skip = Some (where, skip); _ }, Check ->
      let* s = call skip (view item) in
      Ok (host_of where, s)
  | Shell { where; cmd; atomic = None; _ }, _ ->
      let* s = call cmd (view item) in
      Ok (host_of where, s)
  | Shell { where; cmd; atomic = Some dir; _ }, _ ->
      let* dir = call dir (view item) in
      let tmp = Printf.sprintf "%s/.ferry-%s-%s" (parent dir) w.key item.id in
      let* cmd = call cmd (view ~out:tmp item) in
      Ok (host_of where, Printf.sprintf "rm -rf %s && mkdir -p %s && (\n%s\n) && mkdir -p %s && mv %s/* %s/ && rmdir %s" tmp tmp cmd dir tmp dir tmp)
  | Copy { src = sw, sp; dst = dw, dp; move; _ }, _ ->
      let* sp = call sp (view item) in
      let* dp = call dp (view item) in
      copy_script t ~key:w.key ~move ~per_item:(per_item item dp) (host_of sw, sp) (host_of dw, dp)
  | Gate _, _ -> assert false

(* ---- journal ---- *)

let open_append path = open_out_gen [ Open_wronly; Open_creat; Open_append; Open_text ] 0o644 path

let journal t w status item =
  let props = Hashtbl.fold (fun k v acc -> if k = "id" then acc else Printf.sprintf "\t%s=%s" k v :: acc) item.props [] |> List.sort compare in
  Printf.fprintf t.journal "%.0f\t%s\t%s\t%s%s\n" (Unix.gettimeofday ()) w.key status item.id (String.concat "" props);
  flush t.journal

(* id -> (key of the last finished stage, properties) *)
let load_journal path =
  let resume = Hashtbl.create 1024 in
  let prop p = match String.index_opt p '=' with Some i -> Some (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1)) | None -> None in
  if Sys.file_exists path then
    In_channel.with_open_text path (fun ic ->
        let rec loop () =
          match In_channel.input_line ic with
          | None -> ()
          | Some l ->
              (match String.split_on_char '\t' l with
              | _ :: key :: ("done" | "skip") :: id :: props -> Hashtbl.replace resume id (key, List.filter_map prop props)
              | _ -> ());
              loop ()
        in
        loop ());
  resume

(* ---- moving items ---- *)

let backlog w = Queue.length w.input + Queue.length w.ready + List.length w.checking + List.length w.running + w.retrying

let rec forward t node item =
  match node with
  | Work w -> Queue.push item w.input
  | Gate g -> Queue.push item g.queue
  | Exit (g, i) ->
      g.inside.(i) <- g.inside.(i) - 1;
      item.gates <- (match item.gates with _ :: rest -> rest | [] -> []);
      forward t g.after item
  | Finish -> t.complete <- t.complete + 1

let rec can_accept = function
  | Work w -> (not w.paused) && backlog w < w.jobs
  | Gate g ->
      let ok = ref false in
      Array.iteri (fun i e -> if g.inside.(i) < g.capacity && can_accept e then ok := true) g.entries;
      !ok
  | Exit _ | Finish -> true

(* leaving the pipeline for good (failed): free the gate places it holds *)
let release item = List.iter (fun (g, i) -> g.inside.(i) <- g.inside.(i) - 1) item.gates
let reacquire item = List.iter (fun (g, i) -> g.inside.(i) <- g.inside.(i) + 1) item.gates

(* ---- jobs ---- *)

let push_bounded q n x =
  Queue.push x q;
  if Queue.length q > n then ignore (Queue.pop q)

let time () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

let spawn t w item kind =
  match script t w item kind with
  | Error msg ->
      (* could not even start: fail it now, with the reason as its log *)
      let now = Unix.gettimeofday () in
      let job = { work = w; item; kind; proc = Proc.dummy (); started = now; log = Queue.create (); finished = Some (now, 1); cancelled = false } in
      Queue.push msg job.log;
      Printf.fprintf w.log_oc "%s [%s] %s\n" (time ()) item.id msg;
      w.failed <- job :: List.filter (fun j -> j.item.id <> item.id) w.failed;
      release item;
      t.notify (Printf.sprintf "%s %s: %s" w.key item.id msg)
  | Ok (runner, script) -> (
      let tty = match kind with Check -> false | Run _ -> true in
      let job = { work = w; item; kind; proc = Proc.spawn ~tty runner script; started = Unix.gettimeofday (); log = Queue.create (); finished = None; cancelled = false } in
      match kind with Check -> w.checking <- job :: w.checking | Run _ -> w.running <- w.running @ [ job ])

let schedule t =
  List.iter
    (function
      | Row_gate g ->
          let stuck = ref false in
          while (not !stuck) && not (Queue.is_empty g.queue) do
            let best = ref (-1) in
            Array.iteri (fun i e -> if g.inside.(i) < g.capacity && can_accept e && (!best < 0 || g.inside.(i) < g.inside.(!best)) then best := i) g.entries;
            if !best < 0 then stuck := true
            else begin
              let item = Queue.pop g.queue in
              g.inside.(!best) <- g.inside.(!best) + 1;
              item.gates <- (g, !best) :: item.gates;
              forward t g.entries.(!best) item
            end
          done
      | Row_work w ->
          if has_skip w then
            while List.length w.checking < w.opts.checks && not (Queue.is_empty w.input) do
              spawn t w (Queue.pop w.input) Check
            done
          else Queue.transfer w.input w.ready;
          while (not w.paused) && List.length w.running < w.jobs && not (Queue.is_empty w.ready) do
            let item = Queue.pop w.ready in
            let attempt = 1 + Option.value ~default:0 (Hashtbl.find_opt w.attempts item.id) in
            Hashtbl.replace w.attempts item.id attempt;
            spawn t w item (Run attempt)
          done)
    t.rows

let set_prefix = "ferry-set "

let read_job job =
  let w = job.work in
  let tag = match job.kind with Check -> "check " | Run _ -> "" in
  Proc.read job.proc (fun line ->
      push_bounded job.log log_lines line;
      (match job.kind with
      | Run _ when String.starts_with ~prefix:set_prefix line -> (
          let kv = String.trim (String.sub line (String.length set_prefix) (String.length line - String.length set_prefix)) in
          match String.index_opt kv '=' with
          | Some i when i > 0 -> Hashtbl.replace job.item.props (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1))
          | _ -> ())
      | _ -> ());
      Printf.fprintf w.log_oc "%s [%s] %s%s\n" (time ()) job.item.id tag line)

let finish t job rc =
  let w = job.work in
  let now = Unix.gettimeofday () in
  read_job job;
  Proc.close job.proc;
  job.finished <- Some (now, rc);
  let item = job.item in
  let moved_on status =
    journal t w status item;
    forward t w.next item
  in
  match job.kind with
  | Check ->
      w.checking <- List.filter (( != ) job) w.checking;
      if not job.cancelled then
        if rc = 0 then begin
          w.skipped <- w.skipped + 1;
          moved_on "skip"
        end
        else Queue.push item w.ready
  | Run attempt ->
      w.running <- List.filter (( != ) job) w.running;
      if job.cancelled then ()
      else if rc = 0 then begin
        w.done_ <- w.done_ + 1;
        push_bounded w.durations 20 (now -. job.started);
        Queue.push now w.finishes;
        push_bounded w.recent recent_jobs job;
        moved_on "done"
      end
      else if attempt <= w.opts.retries then begin
        w.retrying <- w.retrying + 1;
        w.awaiting <- job :: w.awaiting;
        t.timers <- (now +. (15. *. float attempt), w, item) :: t.timers;
        t.notify (Printf.sprintf "%s %s failed (exit %d), retry %d/%d" w.key item.id rc attempt w.opts.retries)
      end
      else begin
        w.failed <- job :: List.filter (fun j -> j.item.id <> item.id) w.failed;
        release item;
        journal t w "fail" item;
        t.notify (Printf.sprintf "%s %s FAILED (exit %d)" w.key item.id rc)
      end

let jobs t = Array.to_list t.works |> List.concat_map (fun w -> w.checking @ w.running)

(* ---- intake ---- *)

let new_item id props =
  let h = Hashtbl.create 8 in
  List.iter (fun (k, v) -> Hashtbl.replace h k v) props;
  Hashtbl.replace h "id" id;
  { id; props = h; gates = [] }

let find_work t key = Array.to_list t.works |> List.find_opt (fun w -> w.key = key)

(* resume where the journal says, or start at the beginning *)
let admit t item =
  match Option.bind (Hashtbl.find_opt t.resume item.id) (fun (key, _) -> find_work t key) with
  | Some { next = Finish; _ } -> t.earlier <- t.earlier + 1
  | Some w ->
      item.gates <- w.enclosing;
      reacquire item;
      forward t w.next item
  | None -> forward t t.entry item

let arrive t id =
  if id <> "" && not (Hashtbl.mem t.seen id) then begin
    Hashtbl.replace t.seen id ();
    t.total <- t.total + 1;
    if Hashtbl.mem t.finished_ids id then t.earlier <- t.earlier + 1
    else
      let journalled = Hashtbl.find_opt t.resume id in
      let item = new_item id (match journalled with Some (_, p) -> p | None -> []) in
      if t.source.done_if <> None && journalled = None then Queue.push item t.intake else admit t item
  end

let pump_intake t =
  (match t.src_proc with
  | None -> ()
  | Some p -> (
      Proc.read p (fun line -> arrive t (String.trim line));
      match Proc.poll p with
      | None -> ()
      | Some rc ->
          Proc.read p (fun line -> arrive t (String.trim line));
          Proc.close p;
          t.src_proc <- None;
          if rc <> 0 then t.notify (Printf.sprintf "items command exited %d" rc)));
  t.intake_checks <-
    List.filter
      (fun (item, p) ->
        Proc.read p ignore;
        match Proc.poll p with
        | None -> true
        | Some rc ->
            Proc.close p;
            if rc = 0 then t.earlier <- t.earlier + 1 else admit t item;
            false)
      t.intake_checks;
  match t.source.done_if with
  | None -> ()
  | Some (where, cmd) ->
      while List.length t.intake_checks < intake_checks && not (Queue.is_empty t.intake) do
        let item = Queue.pop t.intake in
        match call cmd (view item) with
        | Ok s -> t.intake_checks <- (item, Proc.spawn (host_of where) s) :: t.intake_checks
        | Error msg ->
            t.notify ("done_if: " ^ msg);
            admit t item
      done

(* ---- the main step ---- *)

let pump t =
  pump_intake t;
  List.iter
    (fun job ->
      read_job job;
      match Proc.poll job.proc with Some rc -> finish t job rc | None -> ())
    (jobs t);
  let now = Unix.gettimeofday () in
  let due, later = List.partition (fun (at, _, _) -> at <= now) t.timers in
  t.timers <- later;
  List.iter
    (fun (_, w, item) ->
      w.retrying <- w.retrying - 1;
      w.awaiting <- List.filter (fun j -> j.item != item) w.awaiting;
      Queue.push item w.ready)
    due;
  schedule t;
  Array.iter (fun w -> flush w.log_oc) t.works

let fds t =
  let open_ p = if p.Proc.open_ && not p.Proc.closed then [ p.Proc.fd ] else [] in
  (match t.src_proc with Some p -> open_ p | None -> []) @ List.concat_map (fun (_, p) -> open_ p) t.intake_checks @ List.concat_map (fun j -> open_ j.proc) (jobs t)

let finished t =
  t.src_proc = None && t.timers = [] && Queue.is_empty t.intake && t.intake_checks = []
  && List.for_all (function Row_work w -> backlog w = 0 | Row_gate g -> Queue.is_empty g.queue) t.rows

let failures t = Array.fold_left (fun n w -> n + List.length w.failed) 0 t.works

(* Kill everything still running; give it a few seconds, then SIGKILL. *)
let shutdown t =
  Option.iter (fun p -> Proc.kill p) t.src_proc;
  List.iter (fun (_, p) -> Proc.kill p) t.intake_checks;
  List.iter
    (fun j ->
      j.cancelled <- true;
      Proc.kill j.proc)
    (jobs t);
  let deadline = Unix.gettimeofday () +. 5. in
  let rec wait () =
    List.iter (fun j -> match Proc.poll j.proc with Some rc -> finish t j rc | None -> ()) (jobs t);
    if jobs t <> [] then
      if Unix.gettimeofday () < deadline then begin
        ignore (Unix.select [] [] [] 0.1);
        wait ()
      end
      else List.iter (fun j -> Proc.kill ~signal:Sys.sigkill j.proc) (jobs t)
  in
  wait ();
  flush t.journal

(* ---- statistics and controls for the TUI ---- *)

let mean_duration w = if Queue.is_empty w.durations then None else Some (Queue.fold ( +. ) 0. w.durations /. float (Queue.length w.durations))

let rate_per_hour t w =
  let now = Unix.gettimeofday () in
  let window = 900. in
  while (not (Queue.is_empty w.finishes)) && Queue.peek w.finishes < now -. window do
    ignore (Queue.pop w.finishes)
  done;
  let span = Float.min window (now -. t.started_at) in
  if span < 1. then 0. else float (Queue.length w.finishes) *. 3600. /. span

let retry_failed w =
  List.iter
    (fun j ->
      Hashtbl.remove w.attempts j.item.id;
      reacquire j.item;
      Queue.push j.item w.ready)
    (List.rev w.failed);
  w.failed <- []

let retry_at t w item = List.find_map (fun (at, w', i) -> if w' == w && i == item then Some at else None) t.timers

(* ---- creation ---- *)

let hosts_of (plan : Plan.t) =
  let of_where = function At h -> [ h ] | Local -> [] in
  let rec of_steps steps =
    List.concat_map
      (function
        | Shell { where; _ } -> of_where where
        | Copy { src = sw, _; dst = dw, _; skip; _ } -> of_where sw @ of_where dw @ (match skip with Some (w, _) -> of_where w | None -> [])
        | Gate { branches; _ } -> List.concat_map of_steps branches)
      steps
  in
  of_where (fst plan.source.items)
  @ List.concat_map (fun (w, _) -> of_where w) plan.source.done_list
  @ (match plan.source.done_if with Some (w, _) -> of_where w | None -> [])
  @ of_steps plan.steps
  |> List.sort_uniq compare

let run_list host cmd add =
  let p = Proc.spawn host cmd in
  let rec wait () =
    Proc.read p add;
    match Proc.poll p with
    | Some rc ->
        Proc.read p add;
        Proc.close p;
        rc
    | None ->
        ignore (Unix.select [ p.Proc.fd ] [] [] 0.1);
        wait ()
  in
  wait ()

let create ?(dry = false) ~(plan : Plan.t) ~state_dir ~fresh ~reach ~notify () =
  let entry = chain plan.steps ~depth:0 ~enclosing:[] ~next:Finish in
  let rows = List.rev (rows_of entry []) in
  let works = List.filter_map (function Row_work w -> Some w | Row_gate _ -> None) rows |> Array.of_list in
  if works = [||] then raise (Error "the pipeline has no stages");
  (* journal keys must be unique: stages sharing a name get a host suffix -
     for a copy, the destination, else the source, else both, whichever
     tells them apart. Keys appear in file names. *)
  let label_by pick w =
    match w.step with
    | Copy { src = sw, _; dst = dw, _; _ } -> (
        match pick with `Dst -> where_label dw | `Src -> where_label sw | `Both -> where_label sw ^ "-" ^ where_label dw)
    | _ -> work_label w
  in
  let safe = String.map (function ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | '@') as c -> c | _ -> '_') in
  Array.iter
    (fun w ->
      let group = Array.to_list works |> List.filter (fun w' -> w'.name = w.name) in
      if List.length group > 1 then
        let distinct pick = List.length (List.sort_uniq compare (List.map (label_by pick) group)) = List.length group in
        let pick = List.find_opt distinct [ `Dst; `Src; `Both ] |> Option.value ~default:`Both in
        w.key <- w.name ^ "@" ^ label_by pick w)
    works;
  Array.iter (fun w -> w.key <- safe w.key) works;
  Array.iter
    (fun w -> if Array.exists (fun w' -> w' != w && w'.key = w.key) works then raise (Error (Printf.sprintf "two stages are both called %s; give them different names" w.key)))
    works;
  if (not dry) && not (Sys.file_exists state_dir) then Sys.mkdir state_dir 0o755;
  let journal_path = Filename.concat state_dir "journal" in
  if fresh && (not dry) && Sys.file_exists journal_path then Sys.rename journal_path (journal_path ^ ".old");
  Array.iter (fun w -> w.log_oc <- (if dry then open_out "/dev/null" else open_append (Filename.concat state_dir (w.key ^ ".log")))) works;
  let t =
    {
      rows;
      works;
      entry;
      source = plan.source;
      reach;
      journal = (if dry then open_out "/dev/null" else open_append journal_path);
      resume = (if dry then Hashtbl.create 1 else load_journal journal_path);
      finished_ids = Hashtbl.create 1024;
      seen = Hashtbl.create 1024;
      src_proc = None;
      intake = Queue.create ();
      intake_checks = [];
      total = 0;
      complete = 0;
      earlier = 0;
      timers = [];
      notify;
      started_at = Unix.gettimeofday ();
    }
  in
  if not dry then begin
    List.iter
      (fun (where, cmd) ->
        let rc = run_list (host_of where) cmd (fun l -> Hashtbl.replace t.finished_ids (String.trim l) ()) in
        if rc <> 0 then raise (Error (Printf.sprintf "done_list command exited %d: %s" rc cmd)))
      plan.source.done_list;
    let where, cmd = plan.source.items in
    t.src_proc <- Some (Proc.spawn (host_of where) cmd)
  end;
  t

(* For --dry-run: the commands each stage would run for a sample item. *)
let describe t id =
  let item = new_item id [] in
  List.map
    (function
      | Row_gate g -> `Gate g
      | Row_work w -> `Work (w, script t w item (Run 1), if has_skip w then Some (script t w item Check) else None))
    t.rows
