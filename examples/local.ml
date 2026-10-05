(* A pipeline that runs entirely on this machine, to try ferry and its TUI:

     ferry examples/local.ml

   Each item is a number. "make" writes a file of random words for it, "count"
   counts them and removes the input, and "archive" copies the result into
   place. Run it twice: the second run finds every output already there and
   has nothing to do. *)

open Ferry

let dir = "/tmp/ferry-demo"

let () =
  items local "seq -w 1 40"
  |> stage "make" local ~jobs:4 ~out:(dir ^ "/words/{id}/")
       "awk -v seed={id} 'BEGIN { srand(seed); split(\"red green blue cyan magenta yellow black white\", w); for (i = 0; i < 200; i++) print w[int(rand() * 8) + 1] }' > {out}/words; sleep 1"
  |> stage "count" local ~jobs:2 ~consume:true ~out:(dir ^ "/counts/{id}/")
       (sprintf "sort %s/words/{id}/words | uniq -c | sort -rn > {out}/counts; sleep 2" dir)
  |> copy "archive" (at local (dir ^ "/archive/{id}/"))
  |> run
