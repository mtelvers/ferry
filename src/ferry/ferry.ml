open Ferry_core.Plan

type host = where

let host name = At name
let local = Local

type pipeline = {
  items : (where * string) option;
  steps : step list;
}

type stage = pipeline -> pipeline

let empty = { items = None; steps = [] }
let append s p = { p with steps = p.steps @ [ s ] }
let steps_of (f : stage) = (f empty).steps
let opts ?(jobs = 1) ?(retries = 2) () = { jobs = max 0 jobs; retries = max 0 retries }
(* An output shared between items could not be listed, would be replaced by
   each item, and would be deleted by the first ~consume. *)
let output ?ls name path =
  if not (mentions "id" path) then invalid_arg (Printf.sprintf "%s: the output %S must mention {id}" name path);
  { path; ls }

let stage ?jobs ?retries ?out ?ls ?(consume = false) name where cmd =
  if out = None && ls <> None then invalid_arg (name ^ ": ~ls without ~out");
  append (Shell { name; where; cmd; out = Option.map (output ?ls name) out; consume; opts = opts ?jobs ?retries () })

type place = where * string

let at where path = (where, path)
let copy ?jobs ?retries ?ls name (where, path) = append (Copy { name; where; out = output ?ls name path; move = false; opts = opts ?jobs ?retries () })
let move ?jobs ?retries ?ls name (where, path) = append (Copy { name; where; out = output ?ls name path; move = true; opts = opts ?jobs ?retries () })
let ( >>> ) (f : stage) (g : stage) : stage = fun p -> g (f p)
let limit n f = append (Gate { capacity = max 1 n; branches = [ steps_of f ] })
let either fs = append (Gate { capacity = max_int; branches = List.map steps_of fs })
let items where cmd = { items = Some (where, cmd); steps = [] }

let run p =
  if !registered <> None then invalid_arg "Ferry.run: called more than once";
  match p.items with None -> invalid_arg "Ferry.run: the pipeline has no items (start with Ferry.items)" | Some items -> registered := Some { items; steps = p.steps }

let sprintf = Printf.sprintf
