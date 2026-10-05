# ferry

Ad-hoc streaming pipelines across machines you can ssh to.

You write a pipeline as a short OCaml script. ferry streams item IDs through
its stages, running each stage's shell command on its host with its own
concurrency limit, and an item moves on the moment its job finishes. A TUI
shows every stage's queue and lets you read each job's output.

Workers need only sshd and the commands you already run. ferry has the OCaml
compiler built in: there is nothing to install or build, and a type error is
reported before anything touches a remote host.

```
ferry PIPELINE.ml           # run with the TUI
ferry --no-tui PIPELINE.ml  # progress lines instead (exit 1 if anything failed)
ferry -n PIPELINE.ml        # check hosts and routes, print the resolved commands
ferry --fresh PIPELINE.ml   # ignore the journal from earlier runs (outputs still count)
```

## A pipeline

```ocaml
open Ferry

type gpu = { host : host; scratch : string; chunk : int }

let builder = host "builder.example.org"
let store = host "store.example.org"
let gpus = [ { host = host "gpu1.example.org"; scratch = "/scratch/work"; chunk = 1024 };
             { host = host "gpu2.example.org"; scratch = "/data/work"; chunk = 64 } ]

let build = stage "build" builder ~jobs:4 ~out:"/data/{id}/" "./build.sh {id} {out}"

(* one GPU's share of the work: at most 8 items on its scratch disk at a time *)
let gpu g =
  limit 8
    (move "fetch" (at g.host (g.scratch ^ "/{id}/"))
     >>> stage "encode" g.host ~consume:true ~out:(g.scratch ^ "/out/{id}/")
           (sprintf "CHUNK=%d ./encode.sh %s/{id} {out}" g.chunk g.scratch)
     >>> move "store" (at store "/srv/results/{id}/"))

let () =
  items builder "cat ~/todo.txt"
  |> build
  |> either (List.map gpu gpus)
  |> run
```

A script is a stream of item IDs piped through stages. Every string is a
template: `{id}` is the item's ID, `{out}` the stage's temporary output
directory, and `{NAME}` a property an earlier stage set by printing
`ferry-set NAME=VALUE`. Anything that differs per host is an ordinary OCaml
value, such as the `gpu` record above, so it is type-checked; `sprintf` is
there for commands that vary per host.

| | |
|---|---|
| `items host cmd` | a command printing one ID per line |
| `stage name host cmd` | run `cmd` per item on `host`. `~jobs` (default 1), `~retries` (2), `~out` (what it produces, see below), `~ls` (how to list the outputs when `~out` is not a path), `~consume:true` (delete the previous output once done) |
| `copy name (at host dir)` | rsync the previous output into `dir`, run on whichever host can reach the other; `dir` is this stage's output |
| `move ...` | copy, then delete the source |
| `a >>> b` | one after the other |
| `limit n s` | at most `n` items inside `s`: a bounded queue before a slow stage, or a budget for a scratch disk |
| `either [a; b]` | each item takes one branch: the least loaded one whose first stage has a free slot. Branches rejoin afterwards, so anything that depends on the branch belongs inside it |
| `run` | ends the pipeline |

## Declared outputs

A stage says what it produces for an item with `~out`, a path on its host
that mentions `{id}`. A trailing slash means a directory; otherwise it is a
file in a shared directory. Everything about where an item stands follows
from that, so scripts never spell out skip checks, done lists, cleanup
stages or copy sources:

- **Resume.** At startup each output is listed once (one `ls` per stage, or
  the `~ls` command for outputs on S3 and the like), and an item starts
  after the furthest stage whose output exists. Work done outside ferry,
  before a crash, or by an earlier run is never repeated. The journal in
  `PIPELINE.state/journal` adds what ferry itself finished, with the
  item's properties; an item placed by a listing alone has no properties
  from the stages it skipped.
- **Atomic results.** A command that mentions `{out}` writes into a
  temporary directory beside the output. Only if it succeeds is the result
  moved into place: a directory output is renamed into position; the
  contents of `{out}` go into a file output's directory. A command that
  ignores `{out}` writes wherever it writes, and a crash can leave a
  half-made output that looks finished, as with any shell loop.
- **Copies.** `copy` and `move` name only a destination. The source is the
  previous output, wherever the item's branch put it.
- **Cleanup.** `~consume:true` removes the previous output once the stage
  has succeeded, on whichever host holds it.

`ferry -n` prints, for every stage, the output, the listing command and the
command it would run for a sample item.

## Behaviour

- **Pools balance themselves.** `either` sends each item to a branch with
  room, so a faster GPU takes more items. Stages repeated across branches are
  named by host (`encode@gpu1`, `encode@gpu2`) and can be paused alone.
- **Stopping stops everything.** `q` in the TUI, or SIGINT/SIGTERM/SIGHUP, ends
  every job. Each job runs in its own process group, and remote commands run
  under `ssh -tt`, so they get SIGHUP when their ssh dies.
- **One ssh connection per host.** Jobs share a ControlMaster connection, and
  every host and route is checked at startup.
- **Logs:** `PIPELINE.state/<stage>.log` holds every job's output, each line
  tagged with the item ID. The TUI keeps the last 2000 lines of each job and
  shows the item's properties.

## TUI keys

| key | |
|---|---|
| ↑ ↓ | select a stage (or gate), or a job |
| Tab | switch between the stage table and the job list |
| + − | change the selected stage's jobs |
| p | pause or resume the selected stage |
| r | retry the selected stage's failed items |
| PgUp PgDn End | scroll the log |
| q | stop all jobs and quit |

## Building

notty needs OCaml < 5.4 (`.day10` pins 5.3):

```
day10 build .
```

The result is a self-contained bytecode executable,
`_build/install/default/bin/ferry`. It embeds the toplevel and the `.cmi`
files a pipeline script needs.
