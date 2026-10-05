(* One remote machine does the work: send each input there, process it, and
   bring the result back. Replace the host with one you can `ssh` to without
   a password.

   Inputs are directories under ~/inputs on this machine, one per item. *)

open Ferry

let worker = host "worker.example.org"

let () =
  items local "ls ~/inputs"
  (* declares where the inputs already are, so "send" knows its source. Its
     command must not mention {out}: that would make ferry write a new
     directory in the input's place. *)
  |> stage "input" local ~out:"~/inputs/{id}/" "test -d ~/inputs/{id}"
  |> copy "send" ~jobs:4 (at worker "/scratch/{id}/")
  |> stage "process" worker ~jobs:2 ~consume:true ~out:"/scratch/results/{id}/"
       "./process.sh /scratch/{id} {out}"
  |> move "fetch" (at local "~/results/{id}/")
  |> run
