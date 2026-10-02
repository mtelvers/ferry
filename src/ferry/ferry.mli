(** Pipelines of shell commands across ssh hosts.

    A pipeline script is an ordinary OCaml file that ends by calling {!run}.
    Commands are functions of the item, so anything that differs per host -
    paths, settings - is just an OCaml value the function closes over:

    {[
      open Ferry

      let sprintf = Printf.sprintf

      type gpu = { host : host; scratch : string; chunk : int }

      let builder = host "builder.example.org"
      let gpus = [ { host = host "gpu1.example.org"; scratch = "/scratch/work"; chunk = 1024 };
                   { host = host "gpu2.example.org"; scratch = "/data/work"; chunk = 64 } ]

      let build = shell "build" builder ~jobs:4 (fun i -> sprintf "python3 build_tile.py %s /data/%s" (id i) (id i))
      let fetch g = move "fetch" (builder, fun i -> sprintf "/data/%s/" (id i)) (g.host, fun i -> sprintf "%s/%s/" g.scratch (id i))
      let encode g = shell "encode" g.host (fun i -> sprintf "TC_CHUNK=%d encode %s/%s" g.chunk g.scratch (id i))

      let () =
        run
          (items ~on:builder "cat tiles.txt")
          (build >>> either ~capacity:8 (List.map (fun g -> fetch g >>> encode g) gpus))
    ]}

    Commands are run with [sh -c] on their host. A function may raise
    [Failure]; that fails the job, with the message as its log. *)

type host
(** Where commands run. *)

val host : string -> host
(** A host reached by ssh (anything [ssh NAME] accepts). *)

val local : host
(** The machine running ferry. *)

type item
(** One item, as seen by a command function. *)

val id : item -> string
(** The item's ID, as printed by the {!items} command. *)

val out : item -> string
(** In the command of an [~atomic] stage: the temporary directory to write
    the output to. Raises [Invalid_argument] anywhere else. *)

val prop : item -> string -> string option
(** A property an earlier stage set by printing a line [ferry-set NAME=VALUE]. *)

type pipeline
(** Stages joined with {!(>>>)}. *)

val shell : ?jobs:int -> ?retries:int -> ?checks:int -> ?skip:(item -> string) -> ?atomic:(item -> string) -> string -> host -> (item -> string) -> pipeline
(** [shell name host cmd] runs [cmd item] on [host] for each item.
    - [jobs]: how many at once (default 1).
    - [retries]: retries after a failure, with backoff (default 2).
    - [skip]: a command run first on the same host; exit 0 means this stage
      is already done for the item.
    - [atomic]: a directory; [cmd] writes its output to [out item], which is
      moved into the directory only if [cmd] succeeds. *)

val copy :
  ?jobs:int -> ?retries:int -> ?checks:int -> ?skip:host * (item -> string) -> string -> host * (item -> string) -> host * (item -> string) -> pipeline
(** [copy name (h1, src) (h2, dst)] copies with rsync, run on whichever host
    can reach the other. A destination whose last component contains the
    item's ID is filled under a temporary name and renamed into place. [skip]
    names the host its check runs on. *)

val move :
  ?jobs:int -> ?retries:int -> ?checks:int -> ?skip:host * (item -> string) -> string -> host * (item -> string) -> host * (item -> string) -> pipeline
(** As {!copy}, then removes the source. *)

val ( >>> ) : pipeline -> pipeline -> pipeline
(** One after the other. *)

val either : ?capacity:int -> pipeline list -> pipeline
(** Each item takes one branch: the least loaded one with room whose first
    stage has a free slot. At most [capacity] items are inside a branch at
    once (default: no limit). The branches rejoin afterwards, so anything
    that depends on the branch belongs inside it. *)

val limit : int -> pipeline -> pipeline
(** [limit n p] lets at most [n] items into [p] at once. Items wait to enter
    until [p]'s first stage has a free slot. *)

type source
(** Where the items come from, and which are already finished. *)

val items : ?on:host -> string -> source
(** A command printing one item ID per line (run on [on], default {!local}). *)

val done_list : ?on:host -> string -> source -> source
(** A command, run once at startup, printing IDs that are already finished;
    those items skip the whole pipeline. *)

val done_if : ?on:host -> (item -> string) -> source -> source
(** A command run for each new item; exit 0 means it is already finished and
    skips the whole pipeline. *)

val run : source -> pipeline -> unit
(** The pipeline to run. Call it once, at the end of the script. *)
