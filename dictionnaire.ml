open Tokens
open Regex

type dico = automaton list;;

(* génère les auto*)
let autoN (s:string) (t: token): automaton = N(s, -2, t, false);;
let autoC (s:string) (t: token) : automaton = C(gen_regex s, -2, t, 0, false, []);;

let dico = [
  autoN "allocatable" (Syntax Allocatable);
  autoN "allocate" (Syntax Allocate);
  autoN "assign" (Syntax Assign);
  autoN "assignment" (Syntax Assignment);
  autoN "block data" (Syntax Block_data);
  autoN "call" (Syntax Call);
  autoN "case" (Syntax Case);
  autoN "character" (Syntax Character);
  autoN "common" (Syntax Common);
  autoN "complex" (Syntax Complex);
  autoN "contains" (Syntax Contains);
  autoN "continue" (Syntax Continue);
  autoN "cycle" (Syntax Cycle);
  autoN "data" (Syntax Data);
  autoN "deallocate" (Syntax Deallocate);
  autoN "default" (Syntax Default);
  autoN "do" (Syntax Do);
  autoN "double precision" (Syntax Double_precision);
  autoN "else" (Syntax Else);
  autoN "elsewhere" (Syntax Elsewhere);
  autoN "entry" (Syntax Entry);
  autoN "equivalence" (Syntax Equivalence);
  autoN "exit" (Syntax Exit);
  autoN "external" (Syntax External);
  autoN "function" (Syntax Function);
  autoN "go to" (Syntax Go_to);
  autoN "goto" (Syntax Go_to);
  autoN "if" (Syntax If);
  autoN "implicit" (Syntax Implicit);
  autoN "in" (Syntax In);
  autoN "inout" (Syntax Inout);
  autoN "integer" (Syntax Integer);
  autoN "intent" (Syntax Intent);
  autoN "interface" (Syntax Interface);
  autoN "intrinsic" (Syntax Intrinsic);
  autoN "kind" (Syntax Kind);
  autoN "len" (Syntax Len);
  autoN "logical" (Syntax Logical);
  autoN "module" (Syntax Module);
  autoN "namelist" (Syntax Namelist);
  autoN "nullify" (Syntax Nullify);
  autoN "only" (Syntax Only);
  autoN "operator" (Syntax Operator);
  autoN "optional" (Syntax Optional);
  autoN "out" (Syntax Out);
  autoN "parameter" (Syntax Parameter);
  autoN "pause" (Syntax Pause);
  autoN "pointer" (Syntax Pointer);
  autoN "print" (Syntax Print);
  autoN "private" (Syntax Private);
  autoN "program" (Syntax Program);
  autoN "public" (Syntax Public);
  autoN "real" (Syntax Real);
  autoN "recursive" (Syntax Recursive);
  autoN "result" (Syntax Result);
  autoN "return" (Syntax Return);
  autoN "save" (Syntax Save);
  autoN "select case" (Syntax Select_case);
  autoN "stop" (Syntax Stop);
  autoN "subroutine" (Syntax Subroutine);
  autoN "target" (Syntax Target);
  autoN "then" (Syntax Then);
  autoN "type" (Syntax Type);
  autoN "use" (Syntax Use);
  autoN "where" (Syntax Where);
  autoN "while" (Syntax While);
  autoN "end" (Syntax End);

  autoN " " Space;
  autoN "," Virgule;

  autoN "*" (Operateur Fois);
  autoN "+" (Operateur Plus);
  autoN "-" (Operateur Moins);
  autoN "/" (Operateur Division);
  autoN "=" (Operateur Assignation);
  autoN "**" (Operateur Puissance);

  autoN "<" (Comparateur StrictPlusPetit);
  autoN ">" (Comparateur StrictPlusGrand);
  autoN "<=" (Comparateur PlusPetit);
  autoN ">=" (Comparateur PlusGrand);
  autoN "/=" (Comparateur NonEgal);
  autoN "==" (Comparateur Egal);

  autoN "::" QuatrePoints;
  autoN "(" Parentheseouvrante;
  autoN ")" Parenthesefermante;

  autoC "[0-9]+" (Integer []);
  autoC "[0-9]+\\.[0-9]+" (Floating []);
  autoC "[a-zA-Z0-9]+" (Name []);
  autoC "\".*\"" (Chaine []);
  autoC "'.*'" (Chaine []);
  autoC "!.*\n" (Commentaire []);

];;