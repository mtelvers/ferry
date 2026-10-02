(lang dune 3.19)

(name ferry)

(generate_opam_files true)

(authors "Mark Elvers <mark.elvers@tunbury.org>")

(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(license MIT)

(package
 (name ferry)
 (synopsis "Ad-hoc streaming pipelines across ssh hosts, with a TUI")
 (description "ferry runs a pipeline of shell stages over a stream of item IDs. Each stage runs on a host reachable by ssh, with its own concurrency limit; items move to the next stage the moment they finish. A notty TUI shows every stage's queue and the logs of each job.")
 (depends
  (ocaml (>= 5.0))
  notty
  cmdliner))
