open GenerateFortran
open GenerateC
open Det_automaton
open LL1
open Convert_to_abstract
open Environnement
open GenerateC
open Automates
open Grammar

let transpile_Fortran_to_C (fortran_file_name : string) (c_file_name : string) :
    unit =
  let lexemes = exec_of_file syntax_automate_det fortran_file_name in
  let arbre_syntaxique = analyse_LL1 grammar lexemes in
  let aarbre_syntaxique_abstrait = convert_to_abstract arbre_syntaxique in
  let code_C =
    convert_ast_to_C
      [ aarbre_syntaxique_abstrait ]
      (create_env_from_ast aarbre_syntaxique_abstrait)
      0
  in
  let out_file = open_out c_file_name in
  output_string out_file code_C;
  close_out out_file

let transpile_Fortran_to_Fortran (fortran_file_name : string)
    (c_file_name : string) : unit =
  let lexemes = exec_of_file syntax_automate_det fortran_file_name in
  let arbre_syntaxique = analyse_LL1 grammar lexemes in
  let aarbre_syntaxique_abstrait = convert_to_abstract arbre_syntaxique in
  let code_C =
    convert_ast_to_Fortran
      [ aarbre_syntaxique_abstrait ]
      (create_env_from_ast aarbre_syntaxique_abstrait)
      0
  in
  let out_file = open_out c_file_name in
  output_string out_file code_C;
  close_out out_file
