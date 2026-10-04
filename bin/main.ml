open Ferry_core
open Notty_unix

let stop_requested = ref false

(* ---- loading a pipeline ---- *)

(* The embedded .cmi files, unpacked once per build into a cache directory. *)
let cmi_dir () =
  let digest = Digest.to_hex (Digest.string (String.concat "" (List.map snd Cmis.files))) in
  let base = match Sys.getenv_opt "XDG_CACHE_HOME" with Some d -> d | None -> Filename.concat (Sys.getenv "HOME") ".cache" in
  let dir = List.fold_left Filename.concat base [ "ferry"; "cmi-" ^ String.sub digest 0 12 ] in
  if not (Sys.file_exists (Filename.concat dir "ferry.cmi")) then begin
    ignore (Sys.command ("mkdir -p " ^ Filename.quote dir));
    List.iter
      (fun (name, contents) ->
        let tmp = Filename.concat dir (name ^ ".tmp") in
        Out_channel.with_open_bin tmp (fun oc -> output_string oc contents);
        Sys.rename tmp (Filename.concat dir name))
      Cmis.files
  end;
  dir

(* Evaluate an OCaml pipeline script; it registers its pipeline with Ferry.run. *)
let load_ml path =
  Clflags.include_dirs := [ cmi_dir () ];
  Clflags.no_std_include := true;
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env ();
  if not (Toploop.use_silently Format.err_formatter (Toploop.File path)) then Error "the script failed (see above)"
  else match !Plan.registered with Some plan -> Ok plan | None -> Error "the script did not call Ferry.run"

let load path = load_ml path

(* ---- hosts ---- *)

(* Connect to every host up front: fails fast on a bad name or key, and
   leaves a ControlMaster running for the jobs to share. *)
let check_hosts hosts =
  List.iter
    (fun h ->
      Printf.printf "connecting to %s... %!" h;
      match Proc.run_sync (Some h) "true" with
      | 0 -> print_endline "ok"
      | rc ->
          print_endline "FAILED";
          Printf.eprintf "ferry: cannot run commands on %s over ssh (exit %d)\n" h rc;
          exit 1)
    hosts

let reach =
  let memo = Hashtbl.create 8 in
  fun a b ->
    match Hashtbl.find_opt memo (a, b) with
    | Some r -> r
    | None ->
        let r = Proc.run_sync (Some a) ("ssh -o BatchMode=yes -o ConnectTimeout=5 " ^ Filename.quote b ^ " true") = 0 in
        Hashtbl.replace memo (a, b) r;
        r

(* Probe every pair now, so a copy never stalls the TUI doing it. *)
let probe_routes hosts =
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          if a <> b then begin
            Printf.printf "checking %s can ssh to %s... %!" a b;
            print_endline (if reach a b then "yes" else "no")
          end)
        hosts)
    hosts

(* ---- running ---- *)

let dry_run (eng : Engine.t) =
  List.iter
    (function
      | `Gate (g : Engine.gate) ->
          Printf.printf "\n%s%s capacity=%s: %d branch(es)\n" (String.make (2 * g.gdepth) ' ') (Tui.gate_name g)
            (if g.capacity = max_int then "unlimited" else string_of_int g.capacity)
            (Array.length g.entries)
      | `Work ((w : Engine.work), ls, run) ->
          let pad = String.make (2 * w.depth) ' ' in
          let show label = function
            | Ok (runner, s) -> Printf.printf "%s-- %s on %s:\n%s%s\n" pad label (Option.value ~default:"local" runner) pad (String.concat ("\n" ^ pad) (String.split_on_char '\n' s))
            | Error e -> Printf.printf "%s-- %s: %s\n" pad label e
          in
          Printf.printf "\n%s== %s  jobs=%d retries=%d\n" pad w.key w.jobs w.opts.retries;
          (match Engine.output w with
          | Some o -> Printf.printf "%s-- out: %s%s\n" pad o.path (if ls = None then "  (not listable: resumes from the journal only)" else "")
          | None -> ());
          Option.iter (fun c -> show "list" (Ok (Engine.host_of (Engine.where_of_step w.step), c))) ls;
          show "run" run)
    (Engine.describe eng "<id>")

let run_tui eng ui =
  let term = Term.create () in
  let tty = fst (Term.fds term) in
  let quit = ref false in
  let last_draw = ref 0. in
  while not !quit do
    Engine.pump eng;
    let now = Unix.gettimeofday () in
    if now -. !last_draw > 0.1 then begin
      Term.image term (Tui.render eng ui (Term.size term));
      last_draw := now
    end;
    let readable = try (fun (r, _, _) -> r) (Unix.select (tty :: Engine.fds eng) [] [] 0.25) with Unix.Unix_error (Unix.EINTR, _, _) -> [] in
    if List.mem tty readable then begin
      let rec events () =
        (match Term.event term with
        | `Key key ->
            if Engine.finished eng && fst key = `ASCII 'q' then quit := true else if Tui.handle eng ui key then quit := true;
            last_draw := 0.
        | `Resize _ -> last_draw := 0.
        | `End -> quit := true
        | _ -> ());
        if (not !quit) && Term.pending term then events ()
      in
      events ()
    end;
    if !stop_requested then quit := true
  done;
  Term.image term (Notty.I.string Notty.A.empty "stopping jobs...");
  Engine.shutdown eng;
  Term.release term

let run_headless (eng : Engine.t) =
  let last_status = ref 0. in
  let status () =
    let parts =
      Array.to_list eng.works
      |> List.map (fun (w : Engine.work) -> Printf.sprintf "%s run %d/%d done %d skip %d fail %d" w.key (List.length w.running) w.jobs w.done_ w.skipped (List.length w.failed))
    in
    Printf.printf "%s items %d complete %d earlier %d | %s\n%!" (Engine.time ()) eng.total eng.complete eng.earlier (String.concat " | " parts)
  in
  while (not (Engine.finished eng)) && not !stop_requested do
    Engine.pump eng;
    let now = Unix.gettimeofday () in
    if now -. !last_status > 30. then begin
      status ();
      last_status := now
    end;
    try ignore (Unix.select (Engine.fds eng) [] [] 0.25) with Unix.Unix_error (Unix.EINTR, _, _) -> ()
  done;
  if !stop_requested then begin
    print_endline "stopping jobs...";
    Engine.shutdown eng
  end;
  status ();
  if Engine.failures eng > 0 then 1 else 0

let main path state_dir fresh no_tui dry =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  match load path with
  | Error e ->
      Printf.eprintf "%s: %s\n" path e;
      2
  | Ok plan -> (
      List.iter (fun s -> Sys.set_signal s (Sys.Signal_handle (fun _ -> stop_requested := true))) [ Sys.sigterm; Sys.sighup; Sys.sigint ];
      let hosts = Engine.hosts_of plan in
      check_hosts hosts;
      probe_routes hosts;
      let state_dir = Option.value state_dir ~default:(path ^ ".state") in
      let ui = Tui.create (Filename.basename path) in
      let notify msg = if no_tui then Printf.printf "%s %s\n%!" (Engine.time ()) msg else ui.message <- msg in
      let progress msg = Printf.printf "%s\n%!" msg in
      match Engine.create ~dry ~progress ~plan ~state_dir ~fresh ~reach ~notify () with
      | exception Engine.Error e ->
          Printf.eprintf "%s: %s\n" path e;
          2
      | eng ->
          if dry then begin
            dry_run eng;
            0
          end
          else if no_tui then run_headless eng
          else begin
            run_tui eng ui;
            0
          end)

open Cmdliner

let path = Arg.(required & pos 0 (some file) None & info [] ~docv:"PIPELINE" ~doc:"An OCaml pipeline script.")

let state_dir =
  Arg.(value & opt (some string) None & info [ "state" ] ~docv:"DIR" ~doc:"Directory for the journal and per-stage logs (default PIPELINE.state).")

let fresh = Arg.(value & flag & info [ "fresh" ] ~doc:"Ignore the journal from earlier runs (it is kept as journal.old).")
let no_tui = Arg.(value & flag & info [ "no-tui" ] ~doc:"Print progress lines instead of running the TUI.")
let dry = Arg.(value & flag & info [ "dry-run"; "n" ] ~doc:"Load the pipeline, check hosts and routes, and print the resolved commands.")

let cmd =
  let doc = "run an ad-hoc streaming pipeline across ssh hosts" in
  Cmd.v (Cmd.info "ferry" ~doc) Term.(const main $ path $ state_dir $ fresh $ no_tui $ dry)

let () = exit (Cmd.eval' cmd)
