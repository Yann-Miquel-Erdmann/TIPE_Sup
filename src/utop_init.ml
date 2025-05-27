#load "vector.cma"

#load "symbols.cmo"

#load "abstract_tokens.cmo"

#load "grammar.cmo"

#load "grammar_functions.cmo"

#load "regex.cmo"

#load "automates.cmo"

#load "LL1.cmo"

#load "convert_to_abstract.cmo"

#load "environnement.cmo"

<<<<<<< Updated upstream
#load "detAutomaton.cmo"
=======
#load "det_automaton.cmo"
>>>>>>> Stashed changes

#load "traduction.cmo"

#load "traductionC.cmo"

#load "traductionFortran.cmo"

#load "transpileurs.cmo"

#use "automates.ml"

#use "LL1.ml"

#use "convert_to_abstract.ml"

#use "det_automaton.ml"

let l = exec_of_file syntax_automate_det "tests/Fortran/test.f90"
let a = analyse_LL1 Grammar.grammar l
let t = convert_to_abstract a
