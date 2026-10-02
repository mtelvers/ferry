(* A pipeline description: what the OCaml API and the line format both
   produce, and what the engine runs. Everything that varies per item -
   commands, skip checks, paths - is a function of the item. *)

type where =
  | Local
  | At of string  (** a host reached by ssh *)

(* What a command function sees for one job. *)
type item = {
  id : string;
  props : (string, string) Hashtbl.t;  (** set by earlier stages with "ferry-set NAME=VALUE" *)
  out : string option;  (** an atomic stage's temporary output directory *)
}

type cmd = item -> string

type opts = {
  jobs : int;
  retries : int;
  checks : int;
}

type step =
  | Shell of {
      name : string;
      where : where;
      cmd : cmd;
      skip : cmd option;  (** run on [where]; exit 0 means already done *)
      atomic : cmd option;  (** directory the output is moved into on success *)
      opts : opts;
    }
  | Copy of {
      name : string;
      src : where * cmd;
      dst : where * cmd;
      move : bool;
      skip : (where * cmd) option;
      opts : opts;
    }
  | Gate of {
      capacity : int;  (** most items inside any one branch *)
      branches : step list list;
    }

type source = {
  items : where * string;  (** command printing one item ID per line *)
  done_list : (where * string) list;  (** commands printing IDs already finished *)
  done_if : (where * cmd) option;  (** per item: exit 0 means already finished *)
}

type t = {
  source : source;
  steps : step list;
}

(* Set by [Ferry.run] when a pipeline script is evaluated. *)
let registered : t option ref = ref None

(* ---- the line format, as a plan ---- *)

(* A {name} template as a command function: {id}, {out} (atomic stages) or
   a property an earlier stage set. *)
let template s : cmd =
 fun it ->
  let missing = ref [] in
  let r =
    Spec.subst
      (fun name ->
        let v = match name with "id" -> Some it.id | "out" -> it.out | _ -> Hashtbl.find_opt it.props name in
        if v = None then missing := name :: !missing;
        v)
      s
  in
  match !missing with [] -> r | m -> failwith ("unknown variable " ^ String.concat ", " (List.map (fun n -> "{" ^ n ^ "}") m))

(* ahead=N on a stage becomes a gate of capacity N around that stage and the
   next one. A copy's skip check runs on its destination host. *)
let of_spec (spec : Spec.t) =
  let where_of = function None -> Local | Some h -> At h in
  let step_of (s : Spec.stage) =
    let opts = { jobs = s.jobs; retries = s.retries; checks = s.checks } in
    match s.kind with
    | Spec.Shell { host; skip; run; atomic } ->
        Shell { name = s.name; where = where_of host; cmd = template run; skip = Option.map template skip; atomic = Option.map template atomic; opts }
    | Spec.Copy { src = sh, sp; dst = dh, dp; move; skip } ->
        Copy
          {
            name = s.name;
            src = (where_of sh, template sp);
            dst = (where_of dh, template dp);
            move;
            skip = Option.map (fun c -> (where_of dh, template c)) skip;
            opts;
          }
  in
  let rec go = function
    | [] -> []
    | (s : Spec.stage) :: next :: rest when s.ahead <> None ->
        if next.ahead <> None then raise (Spec.Error (Printf.sprintf "%s and %s: ahead= on consecutive stages is not supported" s.name next.name));
        Gate { capacity = Option.get s.ahead; branches = [ [ step_of s; step_of next ] ] } :: go rest
    | s :: rest -> step_of s :: go rest
  in
  let host, cmd = spec.items in
  { source = { items = (where_of host, cmd); done_list = []; done_if = None }; steps = go spec.stages }
