(** Pipelines of shell commands across ssh hosts.

    A pipeline script is an ordinary OCaml file: a stream of item IDs piped
    through stages, ending in {!run}.

    {[
      open Ferry

      type gpu = { host : host; scratch : string; chunk : int }

      let builder = host "builder.example.org"
      let store = host "store.example.org"
      let gpus = [ { host = host "gpu1.example.org"; scratch = "/scratch/work"; chunk = 1024 };
                   { host = host "gpu2.example.org"; scratch = "/data/work"; chunk = 64 } ]

      let build = stage "build" builder ~jobs:4 ~out:"/data/{id}/" "./build.sh {id} {out}"

      let gpu g =
        limit 8
          (move "fetch" (at g.host (g.scratch ^ "/{id}/"))
           >>> stage "encode" g.host ~consume:true ~out:(g.scratch ^ "/out/{id}/")
                 (sprintf "CHUNK=%d ./encode.sh %s/{id} {out}" g.chunk g.scratch)
           >>> move "store" (at store "/srv/results/{id}/"))

      let () = items builder "cat ~/todo.txt" |> build |> either (List.map gpu gpus) |> run
    ]}

    Every string is a template: [{id}] is the item's ID, [{out}] the
    temporary output directory (see {!stage}), and [{NAME}] a property an
    earlier stage set by printing a line [ferry-set NAME=VALUE]. Commands
    run with [sh -c] on their host.

    {2 Declared outputs}

    A stage that declares [~out] says what it produces for an item, as a path
    on its host; a trailing slash means a directory. From that, ferry derives
    what the script would otherwise spell out:

    - {b Resume.} At startup each output is listed once, and an item starts
      after the furthest stage whose output exists. Work done outside ferry,
      or before a crash, is never repeated.
    - {b Atomic results.} A command that mentions [{out}] writes into a
      temporary directory; only if it succeeds is the result moved into
      place: a directory output is renamed into position, and the contents
      of [{out}] go into a file output's directory.
    - {b Copies.} {!copy} and {!move} take only a destination: the source is
      the previous output.
    - {b Cleanup.} [~consume:true] deletes the previous output once the stage
      has succeeded. *)

type host
(** Where commands run. *)

val host : string -> host
(** A host reached by ssh (anything [ssh NAME] accepts). *)

val local : host
(** The machine running ferry. *)

type pipeline
(** A stream of items and the stages it has passed through so far. *)

type stage = pipeline -> pipeline
(** One step of a pipeline; chain them with [|>] or {!(>>>)}. *)

val items : host -> string -> pipeline
(** [items host cmd]: a command printing one item ID per line. *)

val stage : ?jobs:int -> ?retries:int -> ?out:string -> ?ls:string -> ?consume:bool -> string -> host -> string -> stage
(** [stage name host cmd] runs [cmd] on [host] for each item.
    - [jobs]: how many at once (default 1).
    - [retries]: retries after a failure, with backoff (default 2).
    - [out]: the output path for an item (a trailing slash: a directory).
      It must mention [{id}]: an output shared between items could not be
      listed, would be replaced by each item and deleted by the first
      [consume]. When [cmd] mentions [{out}], it must write there; ferry
      moves the result into place only on success.
    - [ls]: a command, run once at startup on [host], printing the IDs whose
      output already exists. Needed only when the output is not a local path
      - on S3, say. For a path mentioning [{id}] it is derived.
    - [consume]: delete the previous output once this stage succeeds.

    A stage whose command prints [ferry-set NAME=VALUE] sets a property for
    the item, usable as [{NAME}] in later stages. *)

type place
(** A directory on a host. *)

val at : host -> string -> place
(** [at host dir]: a path template on [host]; it must mention [{id}]. *)

val copy : ?jobs:int -> ?retries:int -> ?ls:string -> string -> place -> stage
(** [copy name dst] copies the previous output into [dst] with rsync, run on
    whichever host can reach the other. A destination whose last component
    contains the item's ID is filled under a temporary name and renamed into
    place. [dst] is the stage's output. *)

val move : ?jobs:int -> ?retries:int -> ?ls:string -> string -> place -> stage
(** As {!copy}, then removes the source. *)

val ( >>> ) : stage -> stage -> stage
(** One after the other. *)

val limit : int -> stage -> stage
(** [limit n s] lets at most [n] items into [s] at once - a bounded queue
    before a slow stage, or a budget for a scratch disk. Items wait to enter
    until [s]'s first stage has a free slot. *)

val either : stage list -> stage
(** Each item takes one branch: the least loaded one whose first stage has a
    free slot. The branches rejoin afterwards, so anything that depends on
    the branch belongs inside it; wrap a branch in {!limit} to cap it. *)

val run : pipeline -> unit
(** Register the pipeline to run. Call it once, at the end of the script. *)

val sprintf : ('a, unit, string) format -> 'a
(** [Printf.sprintf], for commands that vary per host. *)
