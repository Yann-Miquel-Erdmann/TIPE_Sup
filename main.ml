open Parser_
open Create_ast
let main () = 
  Create_ast.create_ast (Parser_.analyse (Parser_.read_file ("test.f90")) [] ) 
