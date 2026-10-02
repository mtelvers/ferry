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
ferry --fresh PIPELINE.ml   # ignore the journal from earlier runs
```

## A pipeline

```ocaml
open Ferry

let builder = host "builder.example.org"
let gpu1 = host "gpu1.example.org" ~vars:[ ("SCRATCH", "/scratch/work"); ("CHUNK", "1024") ]
let gpu2 = host "gpu2.example.org" ~vars:[ ("SCRATCH", "/data/work"); ("CHUNK", "64") ]

let build = shell "build" builder ~jobs:4 "cd ~/work && python3 build_tile.py $ID 2017 /data/work/$ID"
let fetch gpu = move "fetch" (builder, "/data/work/$ID/") (gpu, "$SCRATCH/$ID/")
let encode gpu = shell "encode" gpu ~atomic:"$SCRATCH/out/$ID" "TC_CHUNK=$CHUNK encode $SCRATCH/$ID $OUT"
let store = move "store" (here, "$SCRATCH/out/$ID/") (gpu1, "~/v2/2017/$ID/")

let () =
  run
    (items ~on:builder "cat ~/work/missing.txt"
     |> done_if ~on:gpu1 "test -f ~/v2/2017/$ID/$ID.npy")
    (build
     >>> either ~capacity:8 (List.map (fun g -> fetch g >>> encode g) [ gpu1; gpu2 ])
     >>> store)
```

**Commands** are shell, run with `sh -c` on their host. They see the item's
values as environment variables:

- `$ID`: the item;
- `$HERE`: the host it last ran on;
- `$OUT`: the temporary output directory of an `~atomic` stage;
- the `~vars` of the host the command runs on;
- anything an earlier stage set by printing `ferry-set NAME=VALUE`.

Values that are the same for every item are just OCaml: build the command
with `^` or `Printf.sprintf`. ferry adds an `export` line before each
command, so the usual shell rules apply: `$ID` expands inside double quotes
and not inside single quotes, and `awk '{print $2}'` is untouched.

| | |
|---|---|
| `shell name host cmd` | run `cmd` per item on `host`. `~jobs` (default 1), `~retries` (2), `~skip` (a check on the same host: exit 0 = already done), `~atomic:dir` (write to `$OUT`, moved to `dir` on success) |
| `copy name (h1, src) (h2, dst)` | rsync, run on whichever host can reach the other. A destination containing `$ID` is filled under a temporary name and renamed into place |
| `move ...` | copy, then delete the source |
| `here` | the host the item last ran on (or was copied to) |
| `a >>> b` | one after the other |
| `either ~capacity:n [a; b]` | each item takes one branch: the least loaded one with room whose first stage has a free slot. At most `n` items inside a branch. Branches rejoin afterwards |
| `limit n p` | at most `n` items inside `p` |
| `items ~on cmd` | a command printing one ID per line |
| `done_list ~on cmd` | run once at startup: IDs printed are finished and skip the pipeline |
| `done_if ~on cmd` | run per new item: exit 0 means finished, so it skips the pipeline |

## Behaviour

- **Restarts resume.** Each finished stage is recorded in `PIPELINE.state/journal`
  along with the item's properties, so a restart puts each item back where it
  stopped, on the same branch. `done_list`/`done_if` cover work done outside
  ferry. Between them, per-stage `~skip` checks are rarely needed.
- **Pools balance themselves.** `either` sends each item to a branch with
  room, so a faster GPU takes more items. Each pooled stage appears in the
  TUI per host (`encode@gpu1`, `encode@gpu2`) and can be paused alone.
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

## The line format

For a straight pipeline you can skip OCaml. A file not ending in `.ml` is read
as one stage per block, using `{id}`-style templates:

```
set P = builder.example.org
items @{P}: cat ~/tiles.txt

stage build @{P} jobs=4 ahead=40
  skip: test -d /data/{id}
  run: cd ~/work && python3 build.py {id} /data/{id}

move fetch {P}:/data/{id}/ -> gpu1.example.org:/scratch/{id}/ jobs=2
```

`ahead=N` is `limit N` around that stage and the next one. See
`examples/*.ferry`, and `examples/*.ml` for the same pipelines in OCaml.

## Building

notty needs OCaml < 5.4 (`.day10` pins 5.3):

```
day10 build .
```

The result is a self-contained bytecode executable,
`_build/install/default/bin/ferry`. It embeds the toplevel and the `.cmi`
files a pipeline script needs.
