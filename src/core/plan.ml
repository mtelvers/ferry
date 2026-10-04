(* A pipeline description: what the OCaml API produces and the engine runs.

   Strings are templates: {id} is the item's ID, {out} the temporary output
   directory of a stage that declares an output, and {NAME} a property an
   earlier stage set by printing "ferry-set NAME=VALUE". *)

type where =
  | Local
  | At of string  (** a host reached by ssh *)

(* What a stage produces for an item, on the stage's host. A trailing slash
   marks a directory. *)
type output = {
  path : string;  (** template *)
  ls : string option;  (** a command printing the IDs whose output exists; derived from [path] when [None] *)
}

type opts = {
  jobs : int;
  retries : int;
}

type step =
  | Shell of {
      name : string;
      where : where;
      cmd : string;
      out : output option;
      consume : bool;  (** delete the input - the previous output - once done *)
      opts : opts;
    }
  | Copy of {
      name : string;
      where : where;  (** destination host *)
      out : output;  (** destination; the source is the previous output *)
      move : bool;
      opts : opts;
    }
  | Gate of {
      capacity : int;  (** most items inside any one branch *)
      branches : step list list;
    }

type t = {
  items : where * string;  (** command printing one item ID per line *)
  steps : step list;
}

(* Set by [Ferry.run] when a pipeline script is evaluated. *)
let registered : t option ref = ref None

(* ---- templates ---- *)

let is_ident_start = function 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false
let is_ident c = is_ident_start c || match c with '0' .. '9' -> true | _ -> false

(* Replace each {name} by [f name]; one that [f] does not know is kept.
   [${...}] is left alone for the shell. *)
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
        (match f name with Some v -> Buffer.add_string b v | None -> Buffer.add_string b (String.sub s i (!j - i + 1)));
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

(* The variables a template mentions. *)
let vars s =
  let seen = ref [] in
  ignore
    (subst
       (fun name ->
         if not (List.mem name !seen) then seen := name :: !seen;
         None)
       s);
  List.rev !seen

let mentions name s = List.mem name (vars s)
