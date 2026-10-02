(** Pipelines of shell commands across ssh hosts.

    A pipeline script is an ordinary OCaml file that ends by calling {!run}:

    {[
      open Ferry

      let builder = host "builder.example.org"
      let gpus = [ host "gpu1.example.org" ~vars:[ ("CHUNK", "1024") ];
                   host "gpu2.example.org" ~vars:[ ("CHUNK", "64") ] ]

      let build = shell "build" builder ~jobs:4 "python3 build_tile.py $ID /data/$ID"
      let fetch g = move "fetch" (builder, "/data/$ID/") (g, "/scratch/$ID/")
      let encode g = shell "encode" g "TC_CHUNK=$CHUNK encode /scratch/$ID /scratch/out/$ID"

      let () =
        run
          (items ~on:builder "cat tiles.txt")
          (build >>> either ~capacity:8 (List.map (fun g -> fetch g >>> encode g) gpus))
    ]}

    Commands are shell, run with [sh -c] on their host, and see the item's
    values as environment variables:
    - [$ID]: the item;
    - [$HERE]: the host it last ran on (empty for the local machine);
    - [$OUT]: the temporary output directory of an [~atomic] stage;
    - the [~vars] of the host the command runs on;
    - any property an earlier stage set by printing [ferry-set NAME=VALUE].

    Values that are the same for every item are just OCaml: build the
    command string with [^] or [Printf.sprintf].

    Paths given to {!copy}, {!move} and [~atomic] are read by ferry itself, so
    there it expands [$NAME] and [${NAME}] for the names above. *)

type host
(** Where commands run. *)

val host : ?vars:(string * string) list -> string -> host
(** A host reached by ssh (anything [ssh NAME] accepts), with variables for
    commands run there. *)

val local : host
(** The machine running ferry. *)

val here : host
(** Wherever the item last ran (or was copied to). *)

type pipeline
(** Stages joined with {!(>>>)}. *)

val shell : ?jobs:int -> ?retries:int -> ?checks:int -> ?skip:string -> ?atomic:string -> string -> host -> string -> pipeline
(** [shell name host cmd] runs [cmd] for each item on [host].
    - [jobs]: how many at once (default 1).
    - [retries]: retries after a failure, with backoff (default 2).
    - [skip]: a command run first on the same host; exit 0 means this stage
      is already done for the item.
    - [atomic]: a directory; [cmd] writes its output to [$OUT], which is
      moved into the directory only if [cmd] succeeds. *)

val copy : ?jobs:int -> ?retries:int -> ?checks:int -> ?skip:string -> string -> host * string -> host * string -> pipeline
(** [copy name (h1, src) (h2, dst)] copies with rsync, run on whichever host
    can reach the other. A destination containing [$ID] is filled under a
    temporary name and renamed into place. Afterwards [$HERE] is [h2]. *)

val move : ?jobs:int -> ?retries:int -> ?checks:int -> ?skip:string -> string -> host * string -> host * string -> pipeline
(** As {!copy}, then removes the source. *)

val ( >>> ) : pipeline -> pipeline -> pipeline
(** One after the other. *)

val either : ?capacity:int -> pipeline list -> pipeline
(** Each item takes one branch: the least loaded one with room whose first
    stage has a free slot. At most [capacity] items are inside a branch at
    once (default: no limit). The branches rejoin afterwards. *)

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

val done_if : ?on:host -> string -> source -> source
(** A command run for each new item; exit 0 means it is already finished and
    skips the whole pipeline. *)

val run : source -> pipeline -> unit
(** The pipeline to run. Call it once, at the end of the script. *)
