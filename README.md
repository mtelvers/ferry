# ferry

Ad-hoc streaming pipelines across machines you can ssh to.

You describe a pipeline as a list of stages in a small spec file. ferry
streams item IDs through the stages, running each stage's command on its
host with its own concurrency limit. An item moves on the moment its job
finishes. A TUI shows every stage's queue and lets you read each job's output.

Workers need only sshd and the commands you already run. Nothing is installed
on them.

```
ferry SPEC              # run with the TUI
ferry --no-tui SPEC     # progress lines instead (exit 1 if anything failed)
ferry -n SPEC           # check hosts and routes, print the resolved commands
ferry --fresh SPEC      # ignore the journal from earlier runs
```

## Spec files

```
set P = builder.example.org                 # variables: {P} anywhere below
set D = gpu1.example.org

items @{P}: cat ~/tiles.txt               # one item ID per line (omit @host to run locally)

stage build @{P} jobs=4                   # run on P, 4 at a time
  skip: test -d /data/{id}                # exit 0 => already done, pass it on
  run: cd ~/work && python3 build.py {id} /data/{id}

move fetch {P}:/data/{id}/ -> {D}:/scratch/{id}/ jobs=2 ahead=8

stage encode @{D} jobs=1
  atomic: /scratch/out/{id}               # run writes to {out}; moved here on success
  run: encode /scratch/{id} {out}
    rm -rf /scratch/{id}                  # indented lines continue the script

copy ship {D}:/scratch/out/{id}/ -> {P}:/data/out/
```

- **`{id}`** is the item ID. **`{out}`** is the atomic output directory.
  `${...}` is left alone for the shell.
- **Commands are plain shell**, written exactly as you would type them. ferry
  does the quoting for ssh. A continuation line is joined with a newline, so
  end a line with `\` to continue a single command.
- **`jobs=N`** is the number of concurrent runs (default 1). You can change it
  live in the TUI.
- **`ahead=N`** stops the stage starting new work while N or more items are
  running here or are queued/running in the next stage. Use it to keep a fast
  producer from running far ahead of a slow consumer, or to cap the tiles
  staged on a nearly full disk.
- **`retries=N`** sets how many times a failed run is retried (default 2), with
  increasing backoff. **`checks=N`** sets how many skip checks run at once
  (default 4).
- **`copy`/`move SRC -> DST`** use rsync. ferry works out which host can ssh to
  the other and runs rsync there, pulling or pushing. A `move` deletes the
  source after copying. A destination containing `{id}` is filled under a
  temporary name and renamed when complete. A `skip:` on a copy runs on
  whichever host runs the rsync.

## Behaviour

- **Restarts resume.** Finished stages are recorded in `SPEC.state/journal`, so
  a restart puts each item back where it stopped. Skip checks only matter for
  work done outside ferry.
- **Stopping stops everything.** `q` in the TUI, or SIGINT/SIGTERM/SIGHUP, ends
  every job. Each job runs in its own process group, and remote commands run
  under `ssh -tt`, so they get SIGHUP when their ssh dies. Nothing keeps
  running on the remote hosts.
- **One ssh connection per host.** Jobs share a ControlMaster connection, so
  each ssh call costs milliseconds. Every host is connected at startup, so a
  bad host name or key fails immediately.
- **Logs:** `SPEC.state/<stage>.log` holds every job's output, each line tagged
  with the item ID. The TUI keeps the last 2000 lines of each job.

## TUI keys

| key | |
|---|---|
| ↑ ↓ | select a stage, or a job |
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
