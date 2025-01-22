open Parser2
open Create_ast

let write_to_file (filename:string) (content: string) =
  let oc = open_out filename in  
  output_string oc content;      
  close_out oc   

let file_name = "test.f90"

let token_list = Parser2.analyse (
                  Parser2.read_file file_name
                ) [] 
    
let ast =
 Create_ast.compact_ast_list ( let a, l = 
    Create_ast.create_ast (
      Create_ast.merge_syntax (
        token_list
      )
    )
    in 
    (* print_string "compact_start\n";     *)
    a
)

let env = Create_ast.type_ast ast

let res = GenerateC.convert ast env 0
let _ = write_to_file "out.c" res
    
    (* Create_ast.create_ast (Parser2.analyse (Parser2.read_file ("test.f90")) [] )  *)
    
