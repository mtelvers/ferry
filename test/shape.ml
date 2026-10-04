(* The build-and-pool shape with two GPUs, as a dry run: repeated stage names must
   get distinct keys, and a copy after a pool must take its source from the
   pool's last output, not from the stage before the pool. *)
open Ferry

type gpu = { host : host; scratch : string }

let builder = host "builder.example"
let gpu1 = host "gpu1.example"
let gpus = [ { host = gpu1; scratch = "/scratch/a" }; { host = host "gpu2.example"; scratch = "/data/a" } ]
let build = stage "build" builder ~out:"/data/work/{id}/" "build {id} {out}"

let gpu g =
  limit 8
    (move "fetch" (at g.host (g.scratch ^ "/{id}/"))
    >>> stage "encode" g.host ~consume:true ~out:(g.scratch ^ "/out/{id}/") "encode {id} {out}"
    >>> move "store" (at gpu1 "~/v2/{id}/"))

let () = items builder "cat tiles" |> build |> either (List.map gpu gpus) |> copy "ship" (at builder "/data/work/out/{id}/") |> run

let () =
  let plan = Option.get !Ferry_core.Plan.registered in
  let eng = Ferry_core.Engine.create ~dry:true ~plan ~state_dir:"/nonexistent" ~fresh:false ~reach:(fun _ _ -> true) ~notify:ignore () in
  let keys = Array.to_list eng.works |> List.map (fun (w : Ferry_core.Engine.work) -> w.key) in
  assert (keys = [ "build"; "fetch@gpu1"; "encode@gpu1"; "store@gpu1"; "fetch@gpu2"; "encode@gpu2"; "store@gpu2"; "ship" ]);
  let ship =
    Ferry_core.Engine.describe eng "T"
    |> List.find_map (function `Work ((w : Ferry_core.Engine.work), _, Ok (_, s)) when w.key = "ship" -> Some s | _ -> None)
    |> Option.get
  in
  let contains s sub = let n = String.length sub in let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in go 0 in
  assert (contains ship "rsync -a gpu1.example:~/v2/T/ ");
  (* outputs shared between items are refused *)
  let refused f = match f () with exception Invalid_argument _ -> true | _ -> false in
  assert (refused (fun () -> stage "x" builder ~out:"/shared/" "true"));
  assert (refused (fun () -> copy "x" (at builder "/shared/")));
  print_endline "ok"
