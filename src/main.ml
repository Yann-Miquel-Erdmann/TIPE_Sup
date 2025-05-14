open Det_automaton
open LL1
open Convert_to_abstract
open Environnement
open GenerateC
open Automates
open Grammar

let main () =
  let len = Array.length Sys.argv in

  if len < 2 || len > 3 then
    (print_string "Usage : "; print_string Sys.argv.(0); print_string " <inupt_file> [-o <output_file>]"; print_newline();)
  else
    let filename =
      if len == 3 then
        Sys.argv.(2)
      else
        let found = ref false in
        let stop = ref false in
        "tests/C/" ^
        String.fold_right (
          fun x acc ->
            if !stop then acc
            else if not !found then
              (if x = '.' then (found := true); acc)
            else if x = '/' then (stop := true; acc)
            else (String.make 1 x)^acc
        ) Sys.argv.(1) ""
        ^ ".c"
    in
    print_string ("Generating "^filename^"..."); print_newline();
    let l = exec_of_file syntax_automate_det Sys.argv.(1) in
    let a = analyse_LL1 grammar l in 
    let t = convert_to_abstract a in
    let s = convert_ast [t] (create_env_from_ast t) 0 in 
    let out_file = open_out filename in
    output_string out_file s;
    close_out out_file;
    print_string ("Finished generating "^filename);  print_newline()

let _ = main ()