open TraductionFortran
open TraductionC
open DetAutomaton
open LL1
open ConvertToAbstract
open Environnement
open Automates
open Grammar

let transpile_Fortran_to_C (fortran_file_name : string) (c_file_name : string) :
    unit =
  let lexemes = exec_of_file syntax_automate_det fortran_file_name in
  let arbre_syntaxique = analyse_LL1 Grammar.grammar lexemes in
  let tmp_ast = convert_to_abstract arbre_syntaxique in
  let env, ast = create_env_from_ast tmp_ast in
  (* print_env env; *)
  let code_C = convert_ast_to_C ast env in
  let out_file = open_out c_file_name in
  output_string out_file code_C;
  close_out out_file

let transpile_Fortran_to_Fortran (fortran_file_name : string)
    (c_file_name : string) : unit =
  let lexemes = exec_of_file syntax_automate_det fortran_file_name in
  let arbre_syntaxique = analyse_LL1 grammar lexemes in
  let tmp_ast = convert_to_abstract arbre_syntaxique in
  let env, ast = create_env_from_ast tmp_ast in

  let code_Fortran = convert_ast_to_Fortran [ ast ] env in
  let out_file = open_out c_file_name in
  output_string out_file code_Fortran;
  close_out out_file
