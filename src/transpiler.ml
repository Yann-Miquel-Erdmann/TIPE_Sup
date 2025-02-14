
open Create_ast
open Bibliotheques
open Automates


let read_file (file_name: string):string list = 
  let rec lire file liste = 
    let line = input_line file in
      ();
    try lire file (line::liste) with End_of_file->
      close_in file;
      line::liste
  in List.rev (lire (open_in file_name) [])

let write_to_file (filename:string) (content: string) =
  let oc = open_out filename in  
  output_string oc content;      
  close_out oc   

let file_name = "test.f90"

let token_list = 
  Automates.exec syntax_automate_det (
    List.of_seq (String.to_seq (List.fold_left (fun acc  x -> acc ^ "\n" ^ x) "" (read_file file_name)))
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
let transpile (input_filename: string) (output_filename: string): unit = 

  let token_list = Automates.exec syntax_automate_det (
    List.of_seq (String.to_seq (List.fold_left (fun acc x -> acc ^ "\n" ^ x) "" (read_file file_name)))
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
