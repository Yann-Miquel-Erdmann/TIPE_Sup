open Transpileurs

let print_usage () =
  print_string "Usage : ";
  print_newline ();
  print_string Sys.argv.(0);
  print_string " -C <inupt_fortran_file> [-o <output_c_file>]";
  print_newline ();
  print_string Sys.argv.(0);
  print_string " -Fortran <inupt_fortran_file> [-o <output_fortran_file>]";
  print_newline ()

let output_file (len : int) : string =
  if len == 5 then
    (* un nom de fichier de sortie est donné *)
    if Sys.argv.(3) == "-o" then Sys.argv.(5)
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

let main () =
  let len = Array.length Sys.argv in

  if len <> 3 && len <> 5 then print_usage ()
  else
    let input_file_name = Sys.argv.(2) in
    let output_file_name = output_file len in
    print_string ("Generating " ^ output_file_name ^ "...");
    print_newline ();
    if Sys.argv.(1) = "-C" then
      transpile_Fortran_to_C input_file_name output_file_name
    else transpile_Fortran_to_C input_file_name output_file_name;
    print_string ("Finished generating " ^ output_file_name);
    print_newline ()

let _ = main ()
