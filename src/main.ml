open Transpileurs
open DetAutomaton
open LL1
open Automates
open ConvertToAbstract

let print_usage () =
  print_string "Usage : ";
  print_newline ();
  print_string Sys.argv.(0);
  print_string " -C <inupt_fortran_file> [-o <output_c_file>]";
  print_newline ();
  print_string Sys.argv.(0);
  print_string " -Fortran <inupt_fortran_file> [-o <output_fortran_file>]";
  print_newline ();
  print_string Sys.argv.(0);
  print_string " -stress <number>";
  print_newline ()

let output_file (len : int) : string =
  if len == 5 then
    if
      (* un nom de fichier de sortie est donné *)
      Sys.argv.(3) = "-o"
    then Sys.argv.(4)
    else (
      print_usage ();
      raise (Invalid_argument ""))
  else
    (* un nom de fichier de sortie n'est pas donné, on en donne un par défaut*)
    match Sys.argv.(1) with
    | "-Fortran" -> "out.f90"
    | "-C" -> "out.c"
    | _ ->
        print_usage ();
        raise (Invalid_argument "")

let stress_test (n : int) =
  let start = Sys.time () in
  for i = 0 to n do
    transpile_Fortran_to_C "tests/Fortran/fibonacci.f90" "/dev/null"
  done;
  print_float (Sys.time () -. start);
  print_newline ()

let main () =
  let len = Array.length Sys.argv in
  if len <> 3 && len <> 5 then print_usage ()
  else if Sys.argv.(1) = "-stress" then stress_test (int_of_string Sys.argv.(2))
  else
    let input_file_name = Sys.argv.(2) in
    let output_file_name = output_file len in
    print_string ("Generating " ^ output_file_name ^ "...");
    print_newline ();
    if Sys.argv.(1) = "-C" then
      transpile_Fortran_to_C input_file_name output_file_name
    else transpile_Fortran_to_Fortran input_file_name output_file_name;
    print_string ("Finished generating " ^ output_file_name);
    print_newline ()

let _ = main ()
