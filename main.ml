open Parser_
open Create_ast

(* main function, for now paarses from a file put in the string below, not from argument *)
let main () = 
  Create_ast.create_ast (analyse (read_file ("test.f90")) [] ) 
