open Ferry_core.Plan

type host = where

let host ?(vars = []) name = At { name; vars }
let local = Local
let here = Here

type pipeline = step list

let opts ?(jobs = 1) ?(retries = 2) ?(checks = 4) () = { jobs = max 0 jobs; retries = max 0 retries; checks = max 1 checks }

let shell ?jobs ?retries ?checks ?skip ?atomic name where cmd = [ Shell { name; where; cmd; skip; atomic; opts = opts ?jobs ?retries ?checks () } ]

let copy ?jobs ?retries ?checks ?skip name src dst = [ Copy { name; src; dst; move = false; skip; opts = opts ?jobs ?retries ?checks () } ]
let move ?jobs ?retries ?checks ?skip name src dst = [ Copy { name; src; dst; move = true; skip; opts = opts ?jobs ?retries ?checks () } ]
let ( >>> ) a b = a @ b
let either ?(capacity = max_int) branches = [ Gate { capacity = max 1 capacity; branches } ]
let limit n p = either ~capacity:n [ p ]

type nonrec source = source

let items ?(on = Local) cmd = { items = (on, cmd); done_list = []; done_if = None }
let done_list ?(on = Local) cmd src = { src with done_list = src.done_list @ [ (on, cmd) ] }

let done_if ?(on = Local) cmd src =
  if src.done_if <> None then invalid_arg "Ferry.done_if: only one per source";
  { src with done_if = Some (on, cmd) }

let run source steps =
  if !registered <> None then invalid_arg "Ferry.run: called more than once";
  registered := Some { source; steps; syntax = Env }
