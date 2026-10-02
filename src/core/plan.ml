(* A pipeline description: what the OCaml API and the line format both
   produce, and what the engine runs. *)

type host = {
  name : string;
  vars : (string * string) list;
}

type where =
  | Local
  | At of host
  | Here  (** wherever the item last ran *)

type opts = {
  jobs : int;
  retries : int;
  checks : int;
}

type step =
  | Shell of {
      name : string;
      where : where;
      cmd : string;
      skip : string option;
      atomic : string option;
      opts : opts;
    }
  | Copy of {
      name : string;
      src : where * string;
      dst : where * string;
      move : bool;
      skip : string option;
      opts : opts;
    }
  | Gate of {
      capacity : int;  (** most items inside any one branch *)
      branches : step list list;
    }

type source = {
  items : where * string;  (** command printing one item ID per line *)
  done_list : (where * string) list;  (** commands printing IDs already finished *)
  done_if : (where * string) option;  (** per item: exit 0 means already finished *)
}

(* How commands refer to per-item values: {id} (the line format), or
   environment variables $ID (OCaml pipelines). *)
type syntax =
  | Braces
  | Env

type t = {
  source : source;
  steps : step list;
  syntax : syntax;
}

(* Set by [Ferry.run] when a pipeline script is evaluated. *)
let registered : t option ref = ref None

let default_opts = { jobs = 1; retries = 2; checks = 4 }

(* The line format, as a plan. ahead=N on a stage becomes a gate of
   capacity N around that stage and the next one. *)
let of_spec (spec : Spec.t) =
  let where_of = function None -> Local | Some h -> At { name = h; vars = [] } in
  let step_of (s : Spec.stage) =
    let opts = { jobs = s.jobs; retries = s.retries; checks = s.checks } in
    match s.kind with
    | Spec.Shell { host; skip; run; atomic } -> Shell { name = s.name; where = where_of host; cmd = run; skip; atomic; opts }
    | Spec.Copy { src = sh, sp; dst = dh, dp; move; skip } -> Copy { name = s.name; src = (where_of sh, sp); dst = (where_of dh, dp); move; skip; opts }
  in
  let rec go = function
    | [] -> []
    | (s : Spec.stage) :: next :: rest when s.ahead <> None ->
        if next.ahead <> None then raise (Spec.Error (Printf.sprintf "%s and %s: ahead= on consecutive stages is not supported" s.name next.name));
        Gate { capacity = Option.get s.ahead; branches = [ [ step_of s; step_of next ] ] } :: go rest
    | s :: rest -> step_of s :: go rest
  in
  let host, cmd = spec.items in
  { source = { items = (where_of host, cmd); done_list = []; done_if = None }; steps = go spec.stages; syntax = Braces }
