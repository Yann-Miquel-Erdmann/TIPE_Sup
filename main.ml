open Parser
open Create_ast
let main () = 
  Create_ast.create_ast (Parser.analyse (Parser.read_file ("test.f90")) [] ) 
