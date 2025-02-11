open Transpiler

let main () =
  let len = Array.length Sys.argv in
  if len > 3 then
    (print_string "Usage : "; print_string Sys.argv.(0); print_string " <inupt_file> [-o <output_file>]"; print_newline();)
  else
    let out =
      if len == 3 then
        Sys.argv.(2)
      else
        let last_i, _ = String.fold_left (fun (last, n) x -> if x == '.' then (n, n+1) else (last, n+1)) (-1, -1) Sys.argv.(1) in
        String.sub Sys.argv.(1) 0 last_i ^ "c"
      in
      transpile Sys.argv.(1) out