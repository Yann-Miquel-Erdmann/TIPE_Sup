#load "vector.cma"

#load "symbols.cmo"

#load "abstractTokens.cmo"

#load "grammar.cmo"

#load "grammarFunctions.cmo"

#load "regex.cmo"

#load "automates.cmo"

#load "LL1.cmo"

#load "convertToAbstract.cmo"

#load "environnement.cmo"

#load "detAutomaton.cmo"

#load "traduction.cmo"

#load "bibliotheques.cmo"

#load "traductionC.cmo"

#load "traductionFortran.cmo"

#load "transpileurs.cmo"

#use "automates.ml"

#use "LL1.ml"

#use "convertToAbstract.ml"

#use "detAutomaton.ml"

#use "environnement.ml"

let stress_test (n : int) =
  let start = Sys.time () in
  for i = 0 to n do
    let l = exec_of_file syntax_automate_det "tests/Fortran/fibonacci.f90" in
    let a = analyse_LL1 Grammar.grammar l in
    ignore (convert_to_abstract a)
  done;
  print_float (Sys.time () -. start);
  print_newline ()

let generate_ast (filename : string) =
  let l = exec_of_file syntax_automate_det ("tests/Fortran/" ^ filename) in
  let a = analyse_LL1 Grammar.grammar l in
  convert_to_abstract a
(*create_env_from_ast t*)
