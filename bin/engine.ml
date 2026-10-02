(* The scheduler. Items enter the first stage from the items command and move
   to the next stage the moment a job finishes. Each stage has:
     input  - items waiting for their skip check
     ready  - items waiting for a job slot
   A stage starts a run only while it has a free slot (jobs), is not paused,
   and - if it has ahead=N - fewer than N of its items are running here or
   queued/running in the next stage. Completed stages are journalled, so a
   restart resumes each item where it left off. *)

type kind =
  | Check
  | Run of int

type job = {
  stage : int;
  id : string;
  kind : kind;
  proc : Proc.t;
  started : float;
  log : string Queue.t;
  mutable finished : (float * int) option;
  mutable cancelled : bool;
}

type stage = {
  spec : Spec.stage;
  host : string option;
  skip : string option;
  run : string;
  input : string Queue.t;
  ready : string Queue.t;
  mutable jobs : int;
  mutable paused : bool;
  mutable checking : job list;
  mutable running : job list;
  mutable retrying : int;
  mutable awaiting : job list;  (* failed attempts waiting to retry *)
  mutable done_ : int;
  mutable skipped : int;
  mutable failed : job list;
  recent : job Queue.t;
  durations : float Queue.t;
  finishes : float Queue.t;
  attempts : (string, int) Hashtbl.t;
  log_oc : out_channel;
}

type t = {
  stages : stage array;
  journal : out_channel;
  resume : (string, int) Hashtbl.t;
  seen : (string, unit) Hashtbl.t;
  mutable source : Proc.t option;
  mutable source_rc : int option;
  mutable total : int;
  mutable complete : int;
  mutable earlier : int;
  mutable timers : (float * int * string) list;  (* retry at, stage, item *)
  notify : string -> unit;
  started_at : float;
}

let log_lines = 2000
let recent_jobs = 50

exception Error of string

let parent path =
  let rec strip p = if String.length p > 1 && p.[String.length p - 1] = '/' then strip (String.sub p 0 (String.length p - 1)) else p in
  let p = strip path in
  match String.rindex_opt p '/' with Some 0 -> "/" | Some i -> String.sub p 0 i | None -> "."

let with_id id s = Spec.subst (function "id" -> Some id | _ -> None) s

let remote host path = match host with None -> path | Some h -> h ^ ":" ^ path

(* Where a stage's commands run, and the full script for a run. *)
let resolve ~reach (s : Spec.stage) =
  match s.kind with
  | Spec.Shell { host; skip; run; atomic } -> (
      match atomic with
      | None ->
          if Spec.subst (function "out" -> Some "" | _ -> None) run <> run then raise (Error (s.name ^ ": {out} needs atomic:"));
          (host, skip, run)
      | Some dir ->
          let tmp = Printf.sprintf "%s/.ferry-%s-{id}" (parent dir) s.name in
          let run = Spec.subst (function "out" -> Some tmp | _ -> None) run in
          let script = Printf.sprintf "rm -rf %s && mkdir -p %s && (\n%s\n) && mkdir -p %s && mv %s/* %s/ && rmdir %s" tmp tmp run dir tmp dir tmp in
          (host, skip, script))
  | Spec.Copy { src = sh, sp; dst = dh, dp; move; skip } ->
      (* A per-item destination (one containing {id}) is filled under a
         temporary name and renamed into place, so a half-finished copy is
         never mistaken for a finished one. *)
      let per_item = Spec.subst (function "id" -> Some "" | _ -> None) dp <> dp in
      let rec strip p = if String.length p > 1 && p.[String.length p - 1] = '/' then strip (String.sub p 0 (String.length p - 1)) else p in
      let target, finish =
        if per_item then
          let final = strip dp in
          let tmp = Printf.sprintf "%s/.ferry-%s-{id}" (parent final) s.name in
          (tmp, Printf.sprintf "rm -rf %s && mv %s %s" final tmp final)
        else (dp, "")
      in
      let rm_src = if move then " --remove-source-files" else "" in
      let tidy = Printf.sprintf "{ test ! -d %s || find %s -depth -type d -empty -delete; }" sp sp in
      let on host cmd = match host with Some h -> Printf.sprintf "ssh -o BatchMode=yes %s %s" h (Filename.quote cmd) | None -> cmd in
      let steps l = String.concat " && " (List.filter (( <> ) "") l) in
      (* runner is local to the destination (pull) or to the source (push) *)
      let pull ~runner =
        let tidy = if move then if sh = runner then tidy else on sh tidy else "" in
        (runner, skip, steps [ "mkdir -p " ^ parent target; Printf.sprintf "rsync -a%s %s %s" rm_src (if sh = runner then sp else remote sh sp) target; finish; tidy ])
      in
      let push ~runner =
        let rsync_path = Filename.quote (Printf.sprintf "mkdir -p %s && rsync" (parent target)) in
        ( runner,
          skip,
          steps
            [
              Printf.sprintf "rsync -a%s --rsync-path=%s %s %s" rm_src rsync_path sp (remote dh target);
              (if finish = "" then "" else on dh finish);
              (if move then tidy else "");
            ] )
      in
      if sh = dh then pull ~runner:sh
      else begin
        match (sh, dh) with
        | None, Some _ -> push ~runner:None
        | Some _, None -> pull ~runner:None
        | Some a, Some b ->
            if reach b a then pull ~runner:dh
            else if reach a b then push ~runner:sh
            else raise (Error (Printf.sprintf "%s: neither %s nor %s can ssh to the other" s.name a b))
        | None, None -> assert false
      end

let open_append path = open_out_gen [ Open_wronly; Open_creat; Open_append; Open_text ] 0o644 path

let load_journal path index =
  let resume = Hashtbl.create 1024 in
  if Sys.file_exists path then
    In_channel.with_open_text path (fun ic ->
        let rec loop () =
          match In_channel.input_line ic with
          | None -> ()
          | Some l ->
              (match String.split_on_char '\t' l with
              | [ _; st; ("done" | "skip"); id ] -> (
                  match index st with
                  | Some i -> if i + 1 > Option.value ~default:0 (Hashtbl.find_opt resume id) then Hashtbl.replace resume id (i + 1)
                  | None -> ())
              | _ -> ());
              loop ()
        in
        loop ());
  resume

let create ?(dry = false) ~(spec : Spec.t) ~state_dir ~fresh ~reach ~notify () =
  if (not dry) && not (Sys.file_exists state_dir) then Sys.mkdir state_dir 0o755;
  let journal_path = Filename.concat state_dir "journal" in
  if fresh && (not dry) && Sys.file_exists journal_path then Sys.rename journal_path (journal_path ^ ".old");
  let names = List.map (fun (s : Spec.stage) -> s.name) spec.stages in
  let index n =
    let rec find i = function [] -> None | x :: _ when x = n -> Some i | _ :: r -> find (i + 1) r in
    find 0 names
  in
  let resume = if dry then Hashtbl.create 1 else load_journal journal_path index in
  let stages =
    List.map
      (fun (s : Spec.stage) ->
        let host, skip, run = resolve ~reach s in
        {
          spec = s;
          host;
          skip;
          run;
          input = Queue.create ();
          ready = Queue.create ();
          jobs = s.jobs;
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
          log_oc = (if dry then open_out "/dev/null" else open_append (Filename.concat state_dir (s.name ^ ".log")));
        })
      spec.stages
    |> Array.of_list
  in
  let host, cmd = spec.items in
  {
    stages;
    journal = (if dry then open_out "/dev/null" else open_append journal_path);
    resume;
    seen = Hashtbl.create 1024;
    source = (if dry then None else Some (Proc.spawn host cmd));
    source_rc = None;
    total = 0;
    complete = 0;
    earlier = 0;
    timers = [];
    notify;
    started_at = Unix.gettimeofday ();
  }

let journal t stage status id =
  Printf.fprintf t.journal "%.0f\t%s\t%s\t%s\n" (Unix.gettimeofday ()) t.stages.(stage).spec.name status id;
  flush t.journal

let forward t i id = if i >= Array.length t.stages then t.complete <- t.complete + 1 else Queue.push id t.stages.(i).input

let backlog s = Queue.length s.input + Queue.length s.ready + List.length s.checking + List.length s.running + s.retrying

let ahead_ok t i =
  let s = t.stages.(i) in
  match s.spec.ahead with
  | None -> true
  | Some n -> i + 1 >= Array.length t.stages || List.length s.running + backlog t.stages.(i + 1) < n

let spawn t i id kind =
  let s = t.stages.(i) in
  let script, tty = match kind with Check -> (Option.get s.skip, false) | Run _ -> (s.run, true) in
  let proc = Proc.spawn ~tty s.host (with_id id script) in
  let job = { stage = i; id; kind; proc; started = Unix.gettimeofday (); log = Queue.create (); finished = None; cancelled = false } in
  (match kind with Check -> s.checking <- job :: s.checking | Run _ -> s.running <- s.running @ [ job ]);
  job

let schedule t =
  Array.iteri
    (fun i s ->
      (match s.skip with
      | None -> Queue.transfer s.input s.ready
      | Some _ ->
          while List.length s.checking < s.spec.checks && not (Queue.is_empty s.input) do
            ignore (spawn t i (Queue.pop s.input) Check)
          done);
      while (not s.paused) && List.length s.running < s.jobs && (not (Queue.is_empty s.ready)) && ahead_ok t i do
        let id = Queue.pop s.ready in
        let attempt = 1 + Option.value ~default:0 (Hashtbl.find_opt s.attempts id) in
        Hashtbl.replace s.attempts id attempt;
        ignore (spawn t i id (Run attempt))
      done)
    t.stages

let push_bounded q n x =
  Queue.push x q;
  if Queue.length q > n then ignore (Queue.pop q)

let time () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

let read_job t job =
  let s = t.stages.(job.stage) in
  let tag = match job.kind with Check -> "check " | Run _ -> "" in
  Proc.read job.proc (fun line ->
      push_bounded job.log log_lines line;
      Printf.fprintf s.log_oc "%s [%s] %s%s\n" (time ()) job.id tag line)

let finish t job rc =
  let s = t.stages.(job.stage) in
  let now = Unix.gettimeofday () in
  read_job t job;
  Proc.close job.proc;
  job.finished <- Some (now, rc);
  let name = s.spec.name in
  match job.kind with
  | Check ->
      s.checking <- List.filter (( != ) job) s.checking;
      if not job.cancelled then
        if rc = 0 then begin
          s.skipped <- s.skipped + 1;
          journal t job.stage "skip" job.id;
          forward t (job.stage + 1) job.id
        end
        else Queue.push job.id s.ready
  | Run attempt ->
      s.running <- List.filter (( != ) job) s.running;
      if job.cancelled then ()
      else if rc = 0 then begin
        let dt = now -. job.started in
        s.done_ <- s.done_ + 1;
        push_bounded s.durations 20 dt;
        Queue.push now s.finishes;
        push_bounded s.recent recent_jobs job;
        journal t job.stage "done" job.id;
        forward t (job.stage + 1) job.id
      end
      else if attempt <= s.spec.retries then begin
        s.retrying <- s.retrying + 1;
        s.awaiting <- job :: s.awaiting;
        t.timers <- (now +. (15. *. float attempt), job.stage, job.id) :: t.timers;
        t.notify (Printf.sprintf "%s %s failed (exit %d), retry %d/%d" name job.id rc attempt s.spec.retries)
      end
      else begin
        s.failed <- job :: List.filter (fun j -> j.id <> job.id) s.failed;
        journal t job.stage "fail" job.id;
        t.notify (Printf.sprintf "%s %s FAILED (exit %d)" name job.id rc)
      end

let jobs t = Array.to_list t.stages |> List.concat_map (fun s -> s.checking @ s.running)

let read_source t =
  match t.source with
  | None -> ()
  | Some p -> (
      Proc.read p (fun line ->
          let id = String.trim line in
          if id <> "" && not (Hashtbl.mem t.seen id) then begin
            Hashtbl.replace t.seen id ();
            t.total <- t.total + 1;
            let k = Option.value ~default:0 (Hashtbl.find_opt t.resume id) in
            if k >= Array.length t.stages then t.earlier <- t.earlier + 1 else forward t k id
          end);
      match Proc.poll p with
      | None -> ()
      | Some rc ->
          Proc.read p ignore;
          Proc.close p;
          t.source <- None;
          t.source_rc <- Some rc;
          if rc <> 0 then t.notify (Printf.sprintf "items command exited %d" rc))

(* Read output, reap finished jobs, fire retries, start new work. *)
let pump t =
  read_source t;
  List.iter
    (fun job ->
      read_job t job;
      match Proc.poll job.proc with Some rc -> finish t job rc | None -> ())
    (jobs t);
  let now = Unix.gettimeofday () in
  let due, later = List.partition (fun (at, _, _) -> at <= now) t.timers in
  t.timers <- later;
  List.iter
    (fun (_, i, id) ->
      let s = t.stages.(i) in
      s.retrying <- s.retrying - 1;
      s.awaiting <- List.filter (fun j -> j.id <> id) s.awaiting;
      Queue.push id s.ready)
    due;
  schedule t;
  Array.iter (fun s -> flush s.log_oc) t.stages

let fds t =
  let open_ p = if p.Proc.open_ && not p.Proc.closed then [ p.Proc.fd ] else [] in
  (match t.source with Some p -> open_ p | None -> []) @ List.concat_map (fun j -> open_ j.proc) (jobs t)

let finished t = t.source = None && t.timers = [] && Array.for_all (fun s -> backlog s = 0) t.stages

let failures t = Array.fold_left (fun n s -> n + List.length s.failed) 0 t.stages

(* Kill everything still running; give it a few seconds, then SIGKILL. *)
let shutdown t =
  Option.iter (fun p -> Proc.kill p) t.source;
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

(* Per-stage statistics for display. *)
let mean_duration s =
  if Queue.is_empty s.durations then None else Some (Queue.fold ( +. ) 0. s.durations /. float (Queue.length s.durations))

let rate_per_hour t s =
  let now = Unix.gettimeofday () in
  let window = 900. in
  while (not (Queue.is_empty s.finishes)) && Queue.peek s.finishes < now -. window do
    ignore (Queue.pop s.finishes)
  done;
  let span = Float.min window (now -. t.started_at) in
  if span < 1. then 0. else float (Queue.length s.finishes) *. 3600. /. span

let retry_failed t i =
  let s = t.stages.(i) in
  List.iter
    (fun j ->
      Hashtbl.remove s.attempts j.id;
      Queue.push j.id s.ready)
    (List.rev s.failed);
  s.failed <- []

let retry_at t i id = List.find_map (fun (at, j, x) -> if j = i && x = id then Some at else None) t.timers
