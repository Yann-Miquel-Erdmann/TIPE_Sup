open Parser2
open Create_ast
open Bibliotheques
open Dictionnaire

let write_to_file (filename:string) (content: string) =
  let oc = open_out filename in  
  output_string oc content;      
  close_out oc   

let file_name = "test.f90"

let token_list = 
  Parser2.exec syntax_automate_det (
    List.of_seq (String.to_seq (List.fold_left (fun acc x -> acc ^ "\n" ^ x) "" (Parser2.read_file file_name)))
  ) [] 
    
let ast =
  Create_ast.compact_ast_list ( let a, l = 
      Create_ast.create_ast (
        Create_ast.merge_syntax (
          token_list
        )
      )
      in 

      a
  )

let env = Create_ast.env_of_ast ast   
let c_of_fortran_file (input_filename: string) (output_filename: string): unit = 

  let token_list = Parser2.analyse (
                    Parser2.read_file input_filename
                  ) [] 
  in
  let ast =
  Create_ast.compact_ast_list ( let a, l = 
      Create_ast.create_ast (
        Create_ast.merge_syntax (
          token_list
        )
      )
      in 

      a
  )
  in
  let env = Create_ast.env_of_ast ast in
  let biblios = Create_ast.biblio_of_ast ast in
  let c_output = GenerateC.convert ast env biblios  in
  write_to_file output_filename c_output

let in_filename = "test.f90" 
let out_filename = "test.c" 
