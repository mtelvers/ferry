(* embed.exe OUT FILE... : write an OCaml module listing (basename, contents) *)
let () =
  let out = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  Out_channel.with_open_bin out (fun oc ->
      output_string oc "let files = [\n";
      List.iter
        (fun f -> Printf.fprintf oc "  (%S, %S);\n" (Filename.basename f) (In_channel.with_open_bin f In_channel.input_all))
        files;
      output_string oc "]\n")
