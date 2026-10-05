(* A build host prepares inputs, a pool of GPU hosts encodes them, and the
   results are collected in one place and uploaded to S3.

   - Each GPU takes the next item whenever it has room, so a faster GPU
     does more of the work.
   - limit 8 caps how many items sit on each GPU host's scratch disk.
   - Everything that depends on which GPU an item went to (fetch, encode,
     store) stays inside its branch of `either`.
   - The upload's output is on S3, which ferry can't list from a path, so
     ~ls gives the command that prints what's already there.

   Hosts need to reach each other by ssh in at least one direction for each
   copy; ferry works out which way at startup. *)

open Ferry

type gpu = {
  host : host;
  scratch : string;  (* inputs are staged here, outputs under scratch/out *)
  chunk : int;  (* a per-host setting for the encoder *)
}

let builder = host "builder.example.org"
let store = host "store.example.org"

let gpus =
  [
    { host = host "gpu1.example.org"; scratch = "/scratch/work"; chunk = 1024 };
    { host = host "gpu2.example.org"; scratch = "/data/work"; chunk = 64 };
  ]

let bucket = "s3://my-bucket/results"

let build = stage "build" builder ~jobs:4 ~out:"/data/built/{id}/" "./build.sh {id} {out}"

let gpu g =
  limit 8
    (move "fetch" ~jobs:2 (at g.host (g.scratch ^ "/{id}/"))
    >>> stage "encode" g.host ~consume:true ~out:(g.scratch ^ "/out/{id}/")
          (sprintf "CHUNK=%d ./encode.sh %s/{id} {out}" g.chunk g.scratch)
    >>> move "store" ~jobs:2 (at store "/srv/results/{id}/"))

let upload =
  stage "upload" store ~jobs:8 ~out:(bucket ^ "/{id}/")
    ~ls:(sprintf "aws s3 ls %s/ | awk '{print $2}' | tr -d /" bucket)
    (sprintf "aws s3 sync /srv/results/{id}/ %s/{id}/ --only-show-errors" bucket)

let () = items builder "cat ~/todo.txt" |> build |> either (List.map gpu gpus) |> upload |> run
