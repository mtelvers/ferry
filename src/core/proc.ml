(* Child processes: each runs in its own session, so killing the process group
   takes everything down. Remote commands run under ssh; with [~tty:true] the
   remote side gets a pty, so it receives SIGHUP when the ssh client dies. *)

type t = {
  pid : int;
  fd : Unix.file_descr;
  partial : Buffer.t;
  mutable open_ : bool;  (* not yet at EOF *)
  mutable closed : bool;
}

let control_path = "~/.ssh/ferry-%C"

let ssh_args ~tty =
  [
    (if tty then "-tt" else "-T");
    "-o";
    "BatchMode=yes";
    "-o";
    "ControlMaster=auto";
    "-o";
    "ControlPath=" ^ control_path;
    "-o";
    "ControlPersist=10m";
    "-o";
    "LogLevel=ERROR";
  ]

let argv ~tty host script =
  match host with
  | None -> [| "sh"; "-c"; script |]
  | Some h -> Array.of_list (("ssh" :: ssh_args ~tty) @ [ h; "sh -c " ^ Filename.quote script ])

(* [stderr]: where the child's stderr goes; by default it is merged with
   stdout, which suits job logs but not commands whose stdout is data. *)
let spawn ?(tty = false) ?stderr host script =
  let argv = argv ~tty host script in
  let r, w = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  match Unix.fork () with
  | 0 -> (
      try
        ignore (Unix.setsid ());
        Sys.set_signal Sys.sigpipe Sys.Signal_default;
        Unix.dup2 devnull Unix.stdin;
        Unix.dup2 w Unix.stdout;
        Unix.dup2 (Option.value stderr ~default:w) Unix.stderr;
        Unix.execvp argv.(0) argv
      with
      | _ -> Unix._exit 127)
  | pid ->
      Unix.close w;
      Unix.close devnull;
      Unix.set_nonblock r;
      { pid; fd = r; partial = Buffer.create 256; open_ = true; closed = false }

(* Terminal-ish cleanup: drop escape sequences, keep the text after the last
   carriage return (progress bars), and make it printable. *)
let clean line =
  let line =
    let n = String.length line in
    if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
  in
  let line = match String.rindex_opt line '\r' with Some i -> String.sub line (i + 1) (String.length line - i - 1) | None -> line in
  let b = Buffer.create (String.length line) in
  let n = String.length line in
  let rec go i =
    if i >= n then ()
    else if line.[i] = '\027' then begin
      (* CSI: ESC [ params final-byte; otherwise drop ESC and next char *)
      if i + 1 < n && line.[i + 1] = '[' then begin
        let j = ref (i + 2) in
        while !j < n && not (line.[!j] >= '@' && line.[!j] <= '~') do
          incr j
        done;
        go (!j + 1)
      end
      else go (i + 2)
    end
    else begin
      (match line.[i] with
      | '\t' -> Buffer.add_string b "  "
      | c when Char.code c < 32 || Char.code c = 127 -> ()
      | c -> Buffer.add_char b c);
      go (i + 1)
    end
  in
  go 0;
  let s = Buffer.contents b in
  if String.is_valid_utf_8 s then s else String.map (fun c -> if Char.code c >= 128 then '?' else c) s

let chunk = Bytes.create 65536

(* Read whatever is available, calling [f] on each complete line. *)
let read t f =
  let rec loop () =
    match Unix.read t.fd chunk 0 (Bytes.length chunk) with
    | 0 ->
        t.open_ <- false;
        if Buffer.length t.partial > 0 then begin
          f (clean (Buffer.contents t.partial));
          Buffer.clear t.partial
        end
    | n ->
        let s = Bytes.sub_string chunk 0 n in
        let parts = String.split_on_char '\n' s in
        let rec emit = function
          | [] -> ()
          | [ last ] -> Buffer.add_string t.partial last
          | l :: rest ->
              Buffer.add_string t.partial l;
              f (clean (Buffer.contents t.partial));
              Buffer.clear t.partial;
              emit rest
        in
        emit parts;
        loop ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()
    | exception Unix.Unix_error _ -> t.open_ <- false
  in
  if t.open_ && not t.closed then loop ()

let close t =
  t.open_ <- false;
  if not t.closed then begin
    t.closed <- true;
    try Unix.close t.fd with Unix.Unix_error _ -> ()
  end

(* Some exit code if the process has finished. *)
let poll t =
  if t.pid <= 0 then Some 255 else
  match Unix.waitpid [ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, Unix.WEXITED n -> Some n
  | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> Some (128 + abs n)
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Some 255

(* never signal pid 0: kill(-0) would hit our own process group *)
let kill ?(signal = Sys.sigterm) t = if t.pid > 0 then try Unix.kill (-t.pid) signal with Unix.Unix_error _ -> ()

(* stands in for a job that never started *)
let dummy () = { pid = 0; fd = Unix.stdin; partial = Buffer.create 0; open_ = false; closed = true }

(* Run to completion, returning the exit code (for probes at startup). *)
let run_sync host script =
  let t = spawn host script in
  let rec wait () =
    read t ignore;
    match poll t with
    | Some rc ->
        close t;
        rc
    | None ->
        ignore (Unix.select [ t.fd ] [] [] 0.1);
        wait ()
  in
  wait ()
