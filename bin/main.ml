open Notty_unix

let stop_requested = ref false

let hosts (spec : Spec.t) =
  let of_stage (s : Spec.stage) =
    match s.kind with Spec.Shell { host; _ } -> [ host ] | Spec.Copy { src = sh, _; dst = dh, _; _ } -> [ sh; dh ]
  in
  fst spec.items :: List.concat_map of_stage spec.stages |> List.filter_map Fun.id |> List.sort_uniq compare

(* Connect to every host up front: fails fast on a bad name or key, and
   leaves a ControlMaster running for the jobs to share. *)
let check_hosts spec =
  List.iter
    (fun h ->
      Printf.printf "connecting to %s... %!" h;
      match Proc.run_sync (Some h) "true" with
      | 0 -> print_endline "ok"
      | rc ->
          print_endline "FAILED";
          Printf.eprintf "ferry: cannot run commands on %s over ssh (exit %d)\n" h rc;
          exit 1)
    (hosts spec)

let reach =
  let memo = Hashtbl.create 8 in
  fun a b ->
    match Hashtbl.find_opt memo (a, b) with
    | Some r -> r
    | None ->
        Printf.printf "checking %s can ssh to %s... %!" a b;
        let r = Proc.run_sync (Some a) ("ssh -o BatchMode=yes -o ConnectTimeout=5 " ^ Filename.quote b ^ " true") = 0 in
        print_endline (if r then "yes" else "no");
        Hashtbl.replace memo (a, b) r;
        r

let dry_run (eng : Engine.t) =
  Array.iter
    (fun (s : Engine.stage) ->
      Printf.printf "\n== %s  on %s  jobs=%d%s retries=%d\n" s.spec.name
        (Option.value ~default:"local" s.host)
        s.jobs
        (match s.spec.ahead with Some n -> Printf.sprintf " ahead=%d" n | None -> "")
        s.spec.retries;
      Option.iter (Printf.printf "-- skip:\n%s\n") s.skip;
      Printf.printf "-- run:\n%s\n" s.run)
    eng.stages

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

let run_headless eng =
  let last_status = ref 0. in
  let status () =
    let parts =
      Array.to_list eng.Engine.stages
      |> List.map (fun (s : Engine.stage) -> Printf.sprintf "%s run %d/%d done %d skip %d fail %d" s.spec.name (List.length s.running) s.jobs s.done_ s.skipped (List.length s.failed))
    in
    Printf.printf "%s items %d complete %d | %s\n%!" (Engine.time ()) eng.total eng.complete (String.concat " | " parts)
  in
  while (not (Engine.finished eng)) && not !stop_requested do
    Engine.pump eng;
    let now = Unix.gettimeofday () in
    if now -. !last_status > 30. then begin
      status ();
      last_status := now
    end;
    (try ignore (Unix.select (Engine.fds eng) [] [] 0.25) with Unix.Unix_error (Unix.EINTR, _, _) -> ())
  done;
  if !stop_requested then begin
    print_endline "stopping jobs...";
    Engine.shutdown eng
  end;
  status ();
  if Engine.failures eng > 0 then 1 else 0

let main spec_path state_dir fresh no_tui dry =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  List.iter (fun s -> Sys.set_signal s (Sys.Signal_handle (fun _ -> stop_requested := true))) [ Sys.sigterm; Sys.sighup; Sys.sigint ];
  match Spec.parse_file spec_path with
  | exception Spec.Error e ->
      Printf.eprintf "%s: %s\n" spec_path e;
      2
  | exception Sys_error e ->
      prerr_endline e;
      2
  | spec -> (
      check_hosts spec;
      let state_dir = Option.value state_dir ~default:(spec_path ^ ".state") in
      let ui = Tui.create (Filename.basename spec_path) in
      let notify msg = if no_tui then Printf.printf "%s %s\n%!" (Engine.time ()) msg else ui.message <- msg in
      match Engine.create ~dry ~spec ~state_dir ~fresh ~reach ~notify () with
      | exception Engine.Error e ->
          Printf.eprintf "%s: %s\n" spec_path e;
          2
      | eng ->
          if dry then begin
            dry_run eng;
            Engine.shutdown eng;
            0
          end
          else if no_tui then run_headless eng
          else begin
            run_tui eng ui;
            0
          end)

open Cmdliner

let spec_path = Arg.(required & pos 0 (some file) None & info [] ~docv:"SPEC" ~doc:"Pipeline spec file.")

let state_dir =
  Arg.(value & opt (some string) None & info [ "state" ] ~docv:"DIR" ~doc:"Directory for the journal and per-stage logs (default SPEC.state).")

let fresh = Arg.(value & flag & info [ "fresh" ] ~doc:"Ignore the journal from earlier runs (it is kept as journal.old).")
let no_tui = Arg.(value & flag & info [ "no-tui" ] ~doc:"Print progress lines instead of running the TUI.")
let dry = Arg.(value & flag & info [ "dry-run"; "n" ] ~doc:"Parse the spec, check hosts and routes, and print the resolved commands.")

let cmd =
  let doc = "run an ad-hoc streaming pipeline across ssh hosts" in
  Cmd.v (Cmd.info "ferry" ~doc) Term.(const main $ spec_path $ state_dir $ fresh $ no_tui $ dry)

let () = exit (Cmd.eval' cmd)
