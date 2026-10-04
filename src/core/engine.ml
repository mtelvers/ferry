(* The scheduler, over a graph built from a [Plan.t].

   Work nodes run a command per item; each has an input queue of items
   waiting for a job slot. Gates split the flow into branches and rejoin it.
   A gate admits an item into a branch only when that branch has fewer than
   [capacity] items inside and its first stage has a free slot, so the gate's
   queue - not the first stage's - is where items wait. With several
   branches, the item goes to the least loaded one, so faster branches take
   more items.

   Stages declare what they produce. From that follows everything about
   where an item stands: at startup each output is listed once, and an item
   starts after the furthest stage whose output exists (or that the journal
   records as done). A stage that mentions {out} writes there and ferry moves
   the result into place only on success; a copy's source is the previous
   output; [consume] deletes it. *)

open Plan

type item = {
  id : string;
  props : (string, string) Hashtbl.t;
  mutable gates : (gate * int) list;  (** gates entered, innermost first *)
  mutable last : work option;  (** the most recent stage with an output this item passed *)
  sample : bool;  (** a dry run's stand-in: unknown properties show as <NAME> *)
  mutable resumed_from : string option;  (** the stage whose listed output placed it, beyond anything journalled *)
}

and job = {
  work : work;
  item : item;
  attempt : int;
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
  prev : work option;  (** the previous stage with an output, when that is fixed *)
  mutable next : node;
  input : item Queue.t;
  mutable jobs : int;
  mutable paused : bool;
  mutable running : job list;
  mutable retrying : int;
  mutable awaiting : job list;  (** failed attempts waiting to retry *)
  mutable done_ : int;
  mutable skipped : int;  (** items that passed without running: output found, or journalled *)
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
  works : work array;  (** in flow order *)
  entry : node;
  reach : string -> string -> bool;
  journal : out_channel;
  resume : (string, string * (string * string) list) Hashtbl.t;
  found : (string, int) Hashtbl.t;  (** id -> index of the furthest stage whose output was listed *)
  seen : (string, unit) Hashtbl.t;
  mutable src_proc : Proc.t option;
  src_err : Unix.file_descr;  (** stderr of the items and listing commands *)
  mutable total : int;
  mutable complete : int;
  mutable earlier : int;
  mutable timers : (float * work * item) list;  (** retry at, stage, item *)
  notify : string -> unit;
  started_at : float;
}

let log_lines = 2000
let recent_jobs = 50

exception Error of string

let strip_slash p =
  let rec strip p = if String.length p > 1 && p.[String.length p - 1] = '/' then strip (String.sub p 0 (String.length p - 1)) else p in
  strip p

let parent path =
  let p = strip_slash path in
  match String.rindex_opt p '/' with Some 0 -> "/" | Some i -> String.sub p 0 i | None -> "."

let is_dir p = String.length p > 1 && p.[String.length p - 1] = '/'
let short h = List.hd (String.split_on_char '.' h)
let ( let* ) = Result.bind

(* ---- outputs ---- *)

let output_of_step = function Shell { out; _ } -> out | Copy { out; _ } -> Some out | Gate _ -> None
let where_of_step = function Shell { where; _ } | Copy { where; _ } -> where | Gate _ -> assert false
let output w = output_of_step w.step
let has_output w = output w <> None

(* ---- building the graph ---- *)

let new_work step ~depth ~enclosing ~prev ~next =
  let name, opts = match step with Shell { name; opts; _ } | Copy { name; opts; _ } -> (name, opts) | Gate _ -> assert false in
  {
    name;
    key = name;
    step;
    opts;
    depth;
    enclosing;
    prev;
    next;
    input = Queue.create ();
    jobs = opts.jobs;
    paused = false;
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

(* Builds back to front, so each node knows its successor; [prev] - the last
   output-bearing stage before a node - is threaded front to back by
   building the list of works first. *)
let rec chain steps ~depth ~enclosing ~prev ~next =
  match steps with
  | [] -> next
  | step :: rest -> (
      match step with
      | Shell _ | Copy _ ->
          let w = new_work step ~depth ~enclosing ~prev ~next:Finish in
          w.next <- chain rest ~depth ~enclosing ~prev:(if has_output w then Some w else prev) ~next;
          Work w
      | Gate { capacity; branches } ->
          let g = { capacity; gdepth = depth; entries = [||]; inside = [||]; queue = Queue.create (); after = next } in
          g.entries <- Array.of_list (List.mapi (fun i b -> chain b ~depth:(depth + 1) ~enclosing:((g, i) :: enclosing) ~prev ~next:(Exit (g, i))) branches);
          g.inside <- Array.make (Array.length g.entries) 0;
          (* after the gate, the previous output depends on the branch taken;
             the first branch's last output stands in for dry runs and resumes *)
          let rec last_out node acc =
            match node with
            | Work w -> last_out w.next (if has_output w then Some w else acc)
            | Gate g -> last_out g.after (if Array.length g.entries = 0 then acc else last_out g.entries.(0) acc)
            | Exit _ | Finish -> acc
          in
          let prev' = if Array.length g.entries = 0 then prev else last_out g.entries.(0) prev in
          g.after <- chain rest ~depth ~enclosing ~prev:prev' ~next;
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
  | Copy { where; move; _ } -> (
      match Option.map (fun p -> where_label (where_of_step p.step)) w.prev with
      | Some src -> Printf.sprintf "%s%s>%s" src (if move then "=" else "-") (where_label where)
      | None -> (if move then "=>" else "->") ^ where_label where)
  | Gate _ -> ""

let branch_label = function Work w -> work_label w | Gate _ -> "gate" | Exit _ | Finish -> "-"

(* ---- templates for one item ---- *)

let host_of = function At h -> Some h | Local -> None

let template ?out item s =
  let missing = ref [] in
  let r =
    Plan.subst
      (fun name ->
        let v =
          match name with
          | "id" -> Some item.id
          | "out" -> out
          | _ -> ( match Hashtbl.find_opt item.props name with None when item.sample -> Some ("<" ^ name ^ ">") | v -> v)
        in
        if v = None then missing := name :: !missing;
        v)
      s
  in
  match !missing with
  | [] -> Ok r
  | m ->
      let why =
        match item.resumed_from with
        | Some key -> Printf.sprintf " (resumed after %s because its output existed, so properties set by earlier stages are unknown)" key
        | None -> ""
      in
      Error ("unknown variable " ^ String.concat ", " (List.map (fun n -> "{" ^ n ^ "}") (List.rev m)) ^ why)

(* The resolved output of [w] for [item]: host, path, and whether it is a directory. *)
let output_place w item =
  match output w with
  | None -> Result.Error (w.key ^ " has no output")
  | Some o ->
      let* p = template item o.path in
      Ok (where_of_step w.step, strip_slash p, is_dir o.path)

(* What this item brings into [w]: the output of the last stage that had one. *)
let input_place w item =
  match item.last with
  | Some p -> output_place p item
  | None -> Result.Error (w.key ^ ": no earlier stage declares an output")

let on host cmd = match host with Some h -> Printf.sprintf "ssh -o BatchMode=yes %s %s" h (Filename.quote cmd) | None -> cmd
let steps l = String.concat " && " (List.filter (( <> ) "") l)
let remote host path = match host with None -> path | Some h -> h ^ ":" ^ path

(* rsync from (sh, sp) to (dh, dp), run on whichever side can reach the
   other. A per-item destination is filled under a temporary name first. *)
let copy_script t ~key ~move ~per_item (sh, sp, sdir) (dh, dp) =
  let target, finish =
    if per_item then
      let tmp = Printf.sprintf "%s/.ferry-%s-%s" (parent dp) key (Filename.basename dp) in
      (tmp, Printf.sprintf "rm -rf %s && mv %s %s" dp tmp dp)
    else (dp, "")
  in
  let src = if sdir then sp ^ "/" else sp in
  let rm_src = if move then " --remove-source-files" else "" in
  let tidy = if move && sdir then Printf.sprintf "{ test ! -d %s || find %s -depth -type d -empty -delete; }" sp sp else "" in
  let pull runner =
    let tidy = if tidy = "" || sh = runner then tidy else on sh tidy in
    Ok (runner, steps [ "mkdir -p " ^ target; Printf.sprintf "rsync -a%s %s %s/" rm_src (if sh = runner then src else remote sh src) target; finish; tidy ])
  in
  let push runner =
    let rsync_path = Filename.quote (Printf.sprintf "mkdir -p %s && rsync" target) in
    Ok (runner, steps [ Printf.sprintf "rsync -a%s --rsync-path=%s %s %s/" rm_src rsync_path src (remote dh target); (if finish = "" then "" else on dh finish); tidy ])
  in
  if sh = dh then pull sh
  else
    match (sh, dh) with
    | None, Some _ -> push None
    | Some _, None -> pull None
    | Some a, Some b -> if t.reach b a then pull dh else if t.reach a b then push sh else Error (Printf.sprintf "neither %s nor %s can ssh to the other" a b)
    | None, None -> assert false

(* A destination is per item - safe to fill under a temporary name and
   rename into place - when its last component contains the item's ID. *)
let per_item item dp =
  let base = Filename.basename (strip_slash dp) in
  let n = String.length item.id in
  let rec has i = i + n <= String.length base && (String.sub base i n = item.id || has (i + 1)) in
  n > 0 && has 0

(* For one item: the host to run on, and the script. *)
let script t w item =
  match w.step with
  | Shell { where; cmd; out; consume; _ } ->
      let runner = host_of where in
      let* body =
        if not (Plan.mentions "out" cmd) then template item cmd
        else
          match out with
          | None -> Result.Error (w.key ^ " mentions {out} but declares no ~out")
          | Some _ ->
              let* _, p, dir = output_place w item in
              let tmp = Printf.sprintf "%s/.ferry-%s-%s" (parent p) w.key item.id in
              let* cmd = template ~out:tmp item cmd in
              let place =
                if dir then Printf.sprintf "mkdir -p %s && rm -rf %s && mv %s %s" (parent p) p tmp p
                else Printf.sprintf "mkdir -p %s && mv %s/* %s/ && rmdir %s" (parent p) tmp (parent p) tmp
              in
              Ok (Printf.sprintf "rm -rf %s && mkdir -p %s && (\n%s\n) && %s" tmp tmp cmd place)
      in
      if not consume then Ok (runner, body)
      else
        let* ih, ip, _ = input_place w item in
        let rm = "rm -rf " ^ ip in
        Ok (runner, Printf.sprintf "(\n%s\n) && %s" body (if host_of ih = runner then rm else on (host_of ih) rm))
  | Copy { where; move; _ } ->
      let* sh, sp, sdir = input_place w item in
      let* _, dp, _ = output_place w item in
      copy_script t ~key:w.key ~move ~per_item:(per_item item dp) (host_of sh, sp, sdir) (host_of where, dp)
  | Gate _ -> assert false

(* ---- listing outputs ---- *)

(* A command printing the IDs whose output exists, from the path template:
   list the first component that mentions {id}, and keep the matches for
   which the whole path exists. Only templates whose sole variable is {id}
   can be listed this way. *)
let derived_ls path =
  if Plan.vars path <> [ "id" ] then None
  else
    let comps = String.split_on_char '/' (strip_slash path) in
    let rec split before = function
      | [] -> None
      | c :: after when Plan.mentions "id" c -> Some (List.rev before, c, after)
      | c :: after -> split (c :: before) after
    in
    match split [] comps with
    | None -> None
    | Some (before, comp, after) ->
        let dir = match String.concat "/" before with "" when before = [ "" ] -> "/" | "" -> "." | d -> d in
        let var = "{id}" in
        let rec find from = if String.sub comp from (String.length var) = var then from else find (from + 1) in
        let rec find_last from = if from < 0 then assert false else if String.sub comp from (String.length var) = var then from else find_last (from - 1) in
        let i = find 0 and j = find_last (String.length comp - String.length var) in
        let pre = String.sub comp 0 i and post = String.sub comp (j + 4) (String.length comp - j - 4) in
        let rest = String.concat "/" (comp :: after) in
        let full = Plan.subst (function "id" -> Some "$i" | _ -> None) rest in
        Some
          (Printf.sprintf "cd %s 2>/dev/null && for f in %s*%s; do i=${f#%s}; i=${i%%%s}; [ -e \"%s\" ] && echo \"$i\"; done; true" dir pre post pre post full)

let ls_of w = match output w with None -> None | Some { ls = Some c; _ } -> Some c | Some { path; ls = None } -> derived_ls path

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

let backlog w = Queue.length w.input + List.length w.running + w.retrying

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
      (* items already waiting at the gate go first *)
      let rec room i = i < Array.length g.entries && ((g.inside.(i) < g.capacity && can_accept g.entries.(i)) || room (i + 1)) in
      Queue.is_empty g.queue && room 0
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

let spawn t w item attempt =
  match script t w item with
  | Error msg ->
      (* could not even start: fail it now, with the reason as its log *)
      let now = Unix.gettimeofday () in
      let job = { work = w; item; attempt; proc = Proc.dummy (); started = now; log = Queue.create (); finished = Some (now, 1); cancelled = false } in
      Queue.push msg job.log;
      Printf.fprintf w.log_oc "%s [%s] %s\n" (time ()) item.id msg;
      w.failed <- job :: List.filter (fun j -> j.item.id <> item.id) w.failed;
      release item;
      t.notify (Printf.sprintf "%s %s: %s" w.key item.id msg)
  | Ok (runner, script) ->
      let job = { work = w; item; attempt; proc = Proc.spawn ~tty:true runner script; started = Unix.gettimeofday (); log = Queue.create (); finished = None; cancelled = false } in
      w.running <- w.running @ [ job ]

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
          while (not w.paused) && List.length w.running < w.jobs && not (Queue.is_empty w.input) do
            let item = Queue.pop w.input in
            let attempt = 1 + Option.value ~default:0 (Hashtbl.find_opt w.attempts item.id) in
            Hashtbl.replace w.attempts item.id attempt;
            spawn t w item attempt
          done)
    t.rows

let set_prefix = "ferry-set "

let read_job job =
  let w = job.work in
  Proc.read job.proc (fun line ->
      push_bounded job.log log_lines line;
      (if String.starts_with ~prefix:set_prefix line then
         let kv = String.trim (String.sub line (String.length set_prefix) (String.length line - String.length set_prefix)) in
         match String.index_opt kv '=' with
         | Some i when i > 0 -> Hashtbl.replace job.item.props (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1))
         | _ -> ());
      Printf.fprintf w.log_oc "%s [%s] %s\n" (time ()) job.item.id line)

let passed w item =
  if has_output w then item.last <- Some w

let finish t job rc =
  let w = job.work in
  let now = Unix.gettimeofday () in
  read_job job;
  Proc.close job.proc;
  job.finished <- Some (now, rc);
  let item = job.item in
  w.running <- List.filter (( != ) job) w.running;
  if job.cancelled then ()
  else if rc = 0 then begin
    w.done_ <- w.done_ + 1;
    push_bounded w.durations 20 (now -. job.started);
    Queue.push now w.finishes;
    push_bounded w.recent recent_jobs job;
    journal t w "done" item;
    passed w item;
    forward t w.next item
  end
  else if job.attempt <= w.opts.retries then begin
    w.retrying <- w.retrying + 1;
    w.awaiting <- job :: w.awaiting;
    t.timers <- (now +. (15. *. float job.attempt), w, item) :: t.timers;
    t.notify (Printf.sprintf "%s %s failed (exit %d), retry %d/%d" w.key item.id rc job.attempt w.opts.retries)
  end
  else begin
    w.failed <- job :: List.filter (fun j -> j.item.id <> item.id) w.failed;
    release item;
    journal t w "fail" item;
    t.notify (Printf.sprintf "%s %s FAILED (exit %d)" w.key item.id rc)
  end

let jobs t = Array.to_list t.works |> List.concat_map (fun w -> w.running)

(* ---- intake ---- *)

let new_item ?(sample = false) id props =
  let h = Hashtbl.create 8 in
  List.iter (fun (k, v) -> Hashtbl.replace h k v) props;
  Hashtbl.replace h "id" id;
  { id; props = h; gates = []; last = None; sample; resumed_from = None }

let index_of t key = Array.find_index (fun w -> w.key = key) t.works

(* [w'] lies on the path of an item that is at [w]: outside or in the same
   branches, or in branches the item has left - taking the first branch of
   a gate as the one it went through. *)
let on_path w' w =
  let rec suffix a b = a == b || (match b with _ :: rest -> suffix a rest | [] -> false) in
  let rec first_branches a = a == w.enclosing || (match a with (_, 0) :: rest -> first_branches rest | _ -> false) in
  suffix w'.enclosing w.enclosing || first_branches w'.enclosing

(* Start after stage [i] (or at the beginning when [i] < 0). *)
let admit t item i =
  if i < 0 then forward t t.entry item
  else
    let w = t.works.(i) in
    Array.iteri (fun k w' -> if k <= i && on_path w' w then w'.skipped <- w'.skipped + 1) t.works;
    let rec at_end = function Finish -> true | Exit (g, _) -> at_end g.after | Work _ | Gate _ -> false in
    if at_end w.next then t.earlier <- t.earlier + 1
    else begin
      item.gates <- w.enclosing;
      reacquire item;
      item.last <- (if has_output w then Some w else w.prev);
      forward t w.next item
    end

(* An item starts after the furthest stage that the journal or a listing
   says is done. *)
let arrive t id =
  if id <> "" && not (Hashtbl.mem t.seen id) then begin
    Hashtbl.replace t.seen id ();
    t.total <- t.total + 1;
    let journalled = Hashtbl.find_opt t.resume id in
    let item = new_item id (match journalled with Some (_, p) -> p | None -> []) in
    let from_journal = Option.value ~default:(-1) (Option.bind journalled (fun (key, _) -> index_of t key)) in
    let from_listing = Option.value ~default:(-1) (Hashtbl.find_opt t.found id) in
    if from_listing > from_journal then item.resumed_from <- Some t.works.(from_listing).key;
    admit t item (max from_journal from_listing)
  end

let pump_intake t =
  match t.src_proc with
  | None -> ()
  | Some p -> (
      Proc.read p (fun line -> arrive t (String.trim line));
      match Proc.poll p with
      | None -> ()
      | Some rc ->
          Proc.read p (fun line -> arrive t (String.trim line));
          Proc.close p;
          t.src_proc <- None;
          if rc <> 0 then t.notify (Printf.sprintf "items command exited %d (see items.log)" rc))

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
      Queue.push item w.input)
    due;
  schedule t;
  Array.iter (fun w -> flush w.log_oc) t.works

let fds t =
  let open_ p = if p.Proc.open_ && not p.Proc.closed then [ p.Proc.fd ] else [] in
  (match t.src_proc with Some p -> open_ p | None -> []) @ List.concat_map (fun j -> open_ j.proc) (jobs t)

let finished t = t.src_proc = None && t.timers = [] && List.for_all (function Row_work w -> backlog w = 0 | Row_gate g -> Queue.is_empty g.queue) t.rows
let failures t = Array.fold_left (fun n w -> n + List.length w.failed) 0 t.works

(* Kill everything still running; give it a few seconds, then SIGKILL. *)
let shutdown t =
  Option.iter (fun p -> Proc.kill p) t.src_proc;
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
      Queue.push j.item w.input)
    (List.rev w.failed);
  w.failed <- []

let retry_at t w item = List.find_map (fun (at, w', i) -> if w' == w && i == item then Some at else None) t.timers

(* ---- creation ---- *)

let hosts_of (plan : Plan.t) =
  let of_where = function At h -> [ h ] | Local -> [] in
  let rec of_steps steps = List.concat_map (function Shell { where; _ } | Copy { where; _ } -> of_where where | Gate { branches; _ } -> List.concat_map of_steps branches) steps in
  of_where (fst plan.items) @ of_steps plan.steps |> List.sort_uniq compare

let run_list ~stderr host cmd add =
  let p = Proc.spawn ~stderr host cmd in
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

let create ?(dry = false) ?(progress = ignore) ~(plan : Plan.t) ~state_dir ~fresh ~reach ~notify () =
  let entry = chain plan.steps ~depth:0 ~enclosing:[] ~prev:None ~next:Finish in
  let rows = List.rev (rows_of entry []) in
  let works = List.filter_map (function Row_work w -> Some w | Row_gate _ -> None) rows |> Array.of_list in
  if works = [||] then raise (Error "the pipeline has no stages");
  (* journal keys must be unique: stages sharing a name get a host suffix -
     for a copy, the destination, else the source, else both, whichever
     tells them apart. Keys appear in file names. *)
  let label_by pick w =
    let dst = where_label (where_of_step w.step) in
    match (w.step, pick) with
    | Copy _, `Src | Copy _, `Both -> (
        let src = match w.prev with Some p -> where_label (where_of_step p.step) | None -> "?" in
        match pick with `Src -> src | _ -> src ^ "-" ^ dst)
    | _ -> dst
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
      reach;
      journal = (if dry then open_out "/dev/null" else open_append journal_path);
      resume = (if dry then Hashtbl.create 1 else load_journal journal_path);
      found = Hashtbl.create 1024;
      seen = Hashtbl.create 1024;
      src_proc = None;
      src_err = (if dry then Unix.openfile "/dev/null" [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0 else Unix.openfile (Filename.concat state_dir "items.log") [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_CLOEXEC ] 0o644);
      total = 0;
      complete = 0;
      earlier = 0;
      timers = [];
      notify;
      started_at = Unix.gettimeofday ();
    }
  in
  if not dry then begin
    Array.iteri
      (fun i w ->
        match ls_of w with
        | None -> ()
        | Some cmd ->
            let n = ref 0 in
            let rc =
              run_list ~stderr:t.src_err (host_of (where_of_step w.step)) cmd (fun l ->
                  let id = String.trim l in
                  if id <> "" then begin
                    incr n;
                    if Option.value ~default:(-1) (Hashtbl.find_opt t.found id) < i then Hashtbl.replace t.found id i
                  end)
            in
            progress (Printf.sprintf "%s: %d output(s) found%s" w.key !n (if rc = 0 then "" else Printf.sprintf " (listing exited %d, see items.log)" rc)))
      works;
    let where, cmd = plan.items in
    t.src_proc <- Some (Proc.spawn ~stderr:t.src_err (host_of where) cmd)
  end;
  t

(* For --dry-run: what each stage would list and run, for a sample item. *)
let describe t id =
  let item = new_item ~sample:true id [] in
  List.map
    (function
      | Row_gate g -> `Gate g
      | Row_work w ->
          item.last <- w.prev;
          `Work (w, ls_of w, script t w item))
    t.rows
