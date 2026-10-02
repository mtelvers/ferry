(* Pipeline spec files.

   {v
   # comment
   set NAME = value                    variables, used as {NAME} anywhere after
   items: COMMAND                      run locally; one item ID per output line
   items @HOST: COMMAND                ... or on HOST

   stage NAME [@HOST] [jobs=N] [ahead=N] [retries=N] [checks=N]
     skip: COMMAND                     exit 0 => item already done, pass it on
     run: COMMAND                      the work; may continue on further
       indented lines                  (joined with newlines)
     atomic: DIR                       run writes into {out}; moved into DIR
                                       only if it succeeds

   copy NAME [HOST:]SRC -> [HOST:]DST [jobs=N] [ahead=N] ...
   move NAME [HOST:]SRC -> [HOST:]DST ...   (copy, then delete the source)
   v}

   In commands, {id} is the item and {out} the atomic output directory.
   [${...}] is left alone for the shell. *)

type host = string option

type kind =
  | Shell of {
      host : host;
      skip : string option;
      run : string;
      atomic : string option;
    }
  | Copy of {
      src : host * string;
      dst : host * string;
      move : bool;
      skip : string option;
    }

type stage = {
  name : string;
  kind : kind;
  jobs : int;
  ahead : int option;
  retries : int;
  checks : int;
}

type t = {
  items : host * string;
  stages : stage list;
}

exception Error of string

let fail line fmt = Printf.ksprintf (fun s -> raise (Error (Printf.sprintf "line %d: %s" line s))) fmt

let is_ident_start = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '_' ->
      true
  | _ -> false

let is_ident = function
  | '0' .. '9' -> true
  | c -> is_ident_start c

(* Replace {name} using [f]; [f] returns None to leave it in place. *)
let subst f s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '{' && (i = 0 || s.[i - 1] <> '$') && i + 1 < n && is_ident_start s.[i + 1] then begin
      let j = ref (i + 1) in
      while !j < n && is_ident s.[!j] do
        incr j
      done;
      if !j < n && s.[!j] = '}' then begin
        let name = String.sub s (i + 1) (!j - i - 1) in
        (match f name with
        | Some v -> Buffer.add_string b v
        | None -> Buffer.add_string b (String.sub s i (!j - i + 1)));
        go (!j + 1)
      end
      else begin
        Buffer.add_char b '{';
        go (i + 1)
      end
    end
    else begin
      Buffer.add_char b s.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents b

let runtime_vars = [ "id"; "out" ]

let expand vars line s =
  subst
    (fun name ->
      match List.assoc_opt name vars with
      | Some v -> Some v
      | None when List.mem name runtime_vars -> None
      | None -> fail line "unknown variable {%s}" name)
    s

let is_indented l = String.length l > 0 && (l.[0] = ' ' || l.[0] = '\t')
let words s = String.split_on_char ' ' s |> List.concat_map (String.split_on_char '\t') |> List.filter (( <> ) "")

let starts_with ~prefix s = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* "host:path" or "path"; a colon only counts before the first slash *)
let location s =
  match String.index_opt s ':' with
  | Some i when not (String.contains (String.sub s 0 i) '/') -> (Some (String.sub s 0 i), String.sub s (i + 1) (String.length s - i - 1))
  | _ -> (None, s)

type block = {
  header : string list;
  hline : int;
  fields : (string * Buffer.t) list ref;
  mutable current : Buffer.t option;
}

let body_keys = [ "skip"; "run"; "atomic" ]

let finish_block b =
  let field k = Option.map (fun buf -> String.trim (Buffer.contents buf)) (List.assoc_opt k !(b.fields)) in
  let line = b.hline in
  let opts, rest =
    List.partition (fun w -> String.contains w '=') (match b.header with _ :: _ :: rest -> rest | _ -> [])
  in
  let opt k default =
    match List.find_opt (fun w -> starts_with ~prefix:(k ^ "=") w) opts with
    | None -> default
    | Some w -> (
        let v = String.sub w (String.length k + 1) (String.length w - String.length k - 1) in
        match int_of_string_opt v with Some n when n >= 0 -> Some n | _ -> fail line "%s= needs a number, got %S" k v)
  in
  List.iter
    (fun w ->
      match String.index_opt w '=' with
      | Some i when List.mem (String.sub w 0 i) [ "jobs"; "ahead"; "retries"; "checks" ] -> ()
      | _ -> fail line "unknown option %S" w)
    opts;
  let name = match b.header with _ :: name :: _ -> name | _ -> fail line "missing stage name" in
  let get = function Some n -> n | None -> assert false in
  let kind =
    match b.header with
    | "stage" :: _ ->
        let host =
          match rest with
          | [] -> None
          | [ h ] when starts_with ~prefix:"@" h -> Some (String.sub h 1 (String.length h - 1))
          | _ -> fail line "expected: stage NAME [@HOST] [option=N]..."
        in
        let run = match field "run" with Some r when r <> "" -> r | _ -> fail line "stage %s has no run:" name in
        Shell { host; skip = field "skip"; run; atomic = field "atomic" }
    | (("copy" | "move") as verb) :: _ -> (
        if field "run" <> None || field "atomic" <> None then fail line "%s takes no run: or atomic:" verb;
        match rest with
        | [ src; "->"; dst ] -> Copy { src = location src; dst = location dst; move = verb = "move"; skip = field "skip" }
        | _ -> fail line "expected: %s NAME [HOST:]SRC -> [HOST:]DST [option=N]..." verb)
    | _ -> assert false
  in
  {
    name;
    kind;
    jobs = get (opt "jobs" (Some 1));
    ahead = opt "ahead" None;
    retries = get (opt "retries" (Some 2));
    checks = get (opt "checks" (Some 4));
  }

let parse_string text =
  let lines = String.split_on_char '\n' text in
  let vars = ref [] in
  let items = ref None in
  let stages = ref [] in
  let block = ref None in
  let close () =
    Option.iter (fun b -> stages := finish_block b :: !stages) !block;
    block := None
  in
  List.iteri
    (fun i raw ->
      let line = i + 1 in
      let trimmed = String.trim raw in
      if trimmed = "" || trimmed.[0] = '#' then ()
      else if is_indented raw then begin
        match !block with
        | None -> fail line "indented line outside a stage"
        | Some b -> (
            let key =
              match String.index_opt trimmed ':' with
              | Some j when List.mem (String.sub trimmed 0 j) body_keys -> Some (String.sub trimmed 0 j, j)
              | _ -> None
            in
            match key with
            | Some (k, j) ->
                if List.mem_assoc k !(b.fields) then fail line "duplicate %s:" k;
                let buf = Buffer.create 80 in
                Buffer.add_string buf (expand !vars line (String.sub trimmed (j + 1) (String.length trimmed - j - 1)));
                b.fields := (k, buf) :: !(b.fields);
                b.current <- Some buf
            | None -> (
                match b.current with
                | None -> fail line "expected skip:, run: or atomic:"
                | Some buf ->
                    Buffer.add_char buf '\n';
                    Buffer.add_string buf (expand !vars line trimmed)))
      end
      else begin
        close ();
        let l = expand !vars line trimmed in
        match words l with
        | "set" :: name :: "=" :: _ ->
            let v = String.trim (String.sub l (String.index l '=' + 1) (String.length l - String.index l '=' - 1)) in
            vars := (name, v) :: !vars
        | ("stage" | "copy" | "move") :: _ as header -> block := Some { header; hline = line; fields = ref []; current = None }
        | w :: _ when starts_with ~prefix:"items" w -> (
            let rest = String.trim (String.sub l 5 (String.length l - 5)) in
            match String.index_opt rest ':' with
            | None -> fail line "expected: items [@HOST]: COMMAND"
            | Some j ->
                let host = String.trim (String.sub rest 0 j) in
                let cmd = String.trim (String.sub rest (j + 1) (String.length rest - j - 1)) in
                let host =
                  if host = "" then None
                  else if starts_with ~prefix:"@" host then Some (String.sub host 1 (String.length host - 1))
                  else fail line "expected: items [@HOST]: COMMAND"
                in
                items := Some (host, cmd))
        | _ -> fail line "expected set, items, stage, copy or move"
      end)
    lines;
  close ();
  let stages = List.rev !stages in
  let items = match !items with Some i -> i | None -> raise (Error "no items: line") in
  if stages = [] then raise (Error "no stages");
  let names = List.map (fun s -> s.name) stages in
  List.iter
    (fun n -> if List.length (List.filter (( = ) n) names) > 1 then raise (Error (Printf.sprintf "duplicate stage name %s" n)))
    names;
  { items; stages }

let parse_file path = parse_string (In_channel.with_open_text path In_channel.input_all)
