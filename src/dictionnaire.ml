open Tokens
open Regex
open Automates

let escape_uppercase (s : string) : string =
  String.fold_left (fun acc x -> if 97 <= (int_of_char x) && (int_of_char x) <= 122 then acc ^ "(" ^ Char.escaped x ^ "|" ^ String.capitalize_ascii (Char.escaped x) ^ ")" else acc ^ Char.escaped x) "" s
;;

let dico = [
  (escape_uppercase "allocatable", Syntax Allocatable);
  (escape_uppercase "allocate", Syntax Allocate);
  (escape_uppercase "assign", Syntax Assign);
  (escape_uppercase "assignment", Syntax Assignment);
  (escape_uppercase "block data", Syntax Block_data);
  (escape_uppercase "call", Syntax Call);
  (escape_uppercase "case", Syntax Case);
  (escape_uppercase "character", Syntax Character);
  (escape_uppercase "common", Syntax Common);
  (escape_uppercase "complex", Syntax Complex);
  (escape_uppercase "contains", Syntax Contains);
  (escape_uppercase "continue", Syntax Continue);
  (escape_uppercase "cycle", Syntax Cycle);
  (escape_uppercase "data", Syntax Data);
  (escape_uppercase "deallocate", Syntax Deallocate);
  (escape_uppercase "default", Syntax Default);
  (escape_uppercase "do", Syntax Do);
  (escape_uppercase "double precision", Syntax Double_precision);
  (escape_uppercase "else", Syntax Else);
  (escape_uppercase "elsewhere", Syntax Elsewhere);
  (escape_uppercase "entry", Syntax Entry);
  (escape_uppercase "equivalence", Syntax Equivalence);
  (escape_uppercase "exit", Syntax Exit);
  (escape_uppercase "external", Syntax External);
  (escape_uppercase "function", Syntax Function);
  (escape_uppercase "go to", Syntax Go_to);
  (escape_uppercase "goto", Syntax Go_to);
  (escape_uppercase "if", Syntax If);
  (escape_uppercase "implicit", Syntax Implicit);
  (escape_uppercase "in", Syntax In);
  (escape_uppercase "inout", Syntax Inout);
  (escape_uppercase "integer", Syntax Integer);
  (escape_uppercase "intent", Syntax Intent);
  (escape_uppercase "interface", Syntax Interface);
  (escape_uppercase "intrinsic", Syntax Intrinsic);
  (escape_uppercase "kind", Syntax Kind);
  (escape_uppercase "len", Syntax Len);
  (escape_uppercase "logical", Syntax Logical);
  (escape_uppercase "module", Syntax Module);
  (escape_uppercase "namelist", Syntax Namelist);
  (escape_uppercase "nullify", Syntax Nullify);
  (escape_uppercase "only", Syntax Only);
  (escape_uppercase "operator", Syntax Operator);
  (escape_uppercase "optional", Syntax Optional);
  (escape_uppercase "out", Syntax Out);
  (escape_uppercase "parameter", Syntax Parameter);
  (escape_uppercase "pause", Syntax Pause);
  (escape_uppercase "pointer", Syntax Pointer);
  (escape_uppercase "print", Syntax Print);
  (escape_uppercase "private", Syntax Private);
  (escape_uppercase "program", Syntax Program);
  (escape_uppercase "public", Syntax Public);
  (escape_uppercase "real", Syntax Real);
  (escape_uppercase "recursive", Syntax Recursive);
  (escape_uppercase "result", Syntax Result);
  (escape_uppercase "return", Syntax Return);
  (escape_uppercase "save", Syntax Save);
  (escape_uppercase "select case", Syntax Select_case);
  (escape_uppercase "stop", Syntax Stop);
  (escape_uppercase "subroutine", Syntax Subroutine);
  (escape_uppercase "target", Syntax Target);
  (escape_uppercase "then", Syntax Then);
  (escape_uppercase "type", Syntax Type);
  (escape_uppercase "use", Syntax Use);
  (escape_uppercase "where", Syntax Where);
  (escape_uppercase "while", Syntax While);
  (escape_uppercase "end", Syntax End);

  (" ", Space);
  (",", Virgule);

  ("\\*", Operateur Fois);
  ("\\+", Operateur Plus);
  ("-", Operateur Moins);
  ("/", Operateur Division);
  ("=", Operateur Assignation);
  ("\\*\\*", Operateur Puissance);

  ("<", Comparateur StrictPlusPetit);
  (">", Comparateur StrictPlusGrand);
  ("<=", Comparateur PlusPetit);
  (">=", Comparateur PlusGrand);
  ("/=", Comparateur NonEgal);
  ("==", Comparateur Egal);

  ("::", QuatrePoints);
  ("\\(", Parentheseouvrante);
  ("\\)", Parenthesefermante);

  ("[0-9]+", Integer "");
  ("[0-9]+\\.[0-9]+", Floating "");
  ("[a-zA-Z0-9_]+", Name "");
  ("\".*\"", Chaine "");
  ("'.*'", Chaine "");
  ("!.*", Commentaire "");
  ("\n", NewLine)
];;

let syntax_automate = ou_automates (List.map (fun (x, t) -> automate_gen (gen_regex x) t) dico);;

let syntax_automate_det = determinise_v2 (enleve_epsilon_trans syntax_automate);;

let texte = "program hello
  ! This is a comment line; it is ignored by the compiler
  print *, \"Hello, World!\"
end program hello" ;;
exec syntax_automate_det (List.of_seq (String.to_seq texte)) [];;