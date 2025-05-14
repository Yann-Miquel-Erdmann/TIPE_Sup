open Environnement
open Abstract_tokens
open Bibliotheques
open Convert_to_abstract
open LL1

let rec tabs_to_string (n : int) : string =
  if n > 0 then "\t" ^ tabs_to_string (n - 1) else ""

let rec n_new_lines (n : int) : string =
  if n > 0 then "\n" ^ tabs_to_string (n - 1) else ""

let rec convert_ast (ast : ast list) (env : environnement_v2) (nb_tab : int) :
    string =
  match ast with
  | [] -> ""
  | [ Noeud (ProgramRoot, l) ] -> convert_ast l env nb_tab
  | Noeud (Syntax Program, Noeud (Name nom, []) :: l1) :: q ->
      "program " ^ nom ^ convert_ast l1 env (nb_tab + 1)
  | Noeud (Commentaire c, []) :: q ->
      tabs_to_string nb_tab ^ "!" ^ c ^ convert_ast q env nb_tab
  | Noeud (Syntax Print, l2) :: q ->
      tabs_to_string nb_tab ^ "print *,"
      ^ List.fold_left
          (fun acc x -> acc ^ ", " ^ convert_ast [ x ] env nb_tab)
          "" l2
      ^ convert_ast q env nb_tab
  (* définit le type des variables *)
  | Noeud (Syntax Real, l) :: q ->
      let s = convert_ast l env 0 in
      tabs_to_string nb_tab ^ "real ::" ^ s ^ convert_ast q env nb_tab
  | Noeud (Syntax Integer, l) :: q ->
      let s = convert_ast l env 0 in
      tabs_to_string nb_tab ^ "integer ::" ^ s ^ convert_ast q env nb_tab
  | Noeud (Syntax Logical, l) :: q ->
      let s = convert_ast l env 0 in
      tabs_to_string nb_tab ^ "logical ::" ^ s ^ convert_ast q env nb_tab
  | Noeud (Syntax Double_precision, l) :: q ->
      let s = convert_ast l env 0 in
      tabs_to_string nb_tab ^ "double precision ::" ^ s
      ^ convert_ast q env nb_tab
  | Noeud (Operateur Assignation, Noeud (Name s, []) :: l) :: q ->
      tabs_to_string nb_tab ^ s ^ " = " ^ convert_ast l env 0
      ^ convert_ast q env nb_tab
  | Noeud (Name s, []) :: q -> s ^ convert_ast q env nb_tab
  | Noeud (Operateur Plus, elem :: l) :: q ->
      convert_ast [ elem ] env nb_tab
      ^ " + " ^ convert_ast l env nb_tab ^ convert_ast q env nb_tab
  | Noeud (Operateur Moins, elem :: l) :: q ->
      convert_ast [ elem ] env nb_tab
      ^ " - " ^ convert_ast l env nb_tab ^ convert_ast q env nb_tab
  | Noeud (Operateur Fois, elem :: l) :: q ->
      convert_ast [ elem ] env nb_tab
      ^ " * " ^ convert_ast l env nb_tab ^ convert_ast q env nb_tab
  | Noeud (Operateur Division, elem :: l) :: q ->
      convert_ast [ elem ] env nb_tab
      ^ " / " ^ convert_ast l env nb_tab ^ convert_ast q env nb_tab
  | Noeud (Parentheseouvrante, []) :: q -> "(" ^ convert_ast q env nb_tab
  | Noeud (Parenthesefermante, []) :: q -> ")" ^ convert_ast q env nb_tab
  | Noeud (OperateurLogique NonEquivalent, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " .neqv. " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Comparateur NonEgal, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " /= " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (OperateurLogique Equivalent, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " .eqv. " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Comparateur Egal, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " == " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Comparateur StrictPlusPetit, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " < " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Comparateur PlusPetit, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " <= " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Comparateur StrictPlusGrand, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " > " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Comparateur PlusGrand, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " >= " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (OperateurLogique Et, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " && " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (OperateurLogique Ou, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " || " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (OperateurLogique Non, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ " ! " ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Operateur Puissance, [ p1; p2 ]) :: q ->
      convert_ast [ p1 ] env 0 ^ "**" ^ convert_ast [ p2 ] env 0
      ^ convert_ast q env nb_tab
  | Noeud (Syntax If, condition :: instructions)
    :: Noeud (Syntax Else_if, l)
    :: q ->
      tabs_to_string nb_tab ^ "if ("
      ^ convert_ast [ condition ] env nb_tab
      ^ ") do"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab
      ^ convert_ast (Noeud (Syntax Else_if, l) :: q) env nb_tab
  | Noeud (Syntax If, condition :: instructions) :: Noeud (Syntax Else, l) :: q
    ->
      tabs_to_string nb_tab ^ "if ("
      ^ convert_ast [ condition ] env nb_tab
      ^ ") do"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab
      ^ convert_ast (Noeud (Syntax Else, l) :: q) env nb_tab
  | Noeud (Syntax If, condition :: instructions) :: q ->
      tabs_to_string nb_tab ^ "if ("
      ^ convert_ast [ condition ] env nb_tab
      ^ ") do"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "end if" ^ convert_ast q env nb_tab
  | Noeud (Syntax Else_if, condition :: instructions) :: q ->
      tabs_to_string nb_tab ^ "else if ("
      ^ convert_ast [ condition ] env nb_tab
      ^ "){"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}" ^ convert_ast q env nb_tab
  | Noeud (Syntax Else, instructions) :: q ->
      tabs_to_string nb_tab ^ "else {"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}" ^ convert_ast q env nb_tab
  | Noeud
      ( Syntax For,
        Noeud (Operateur Assignation, [ variable; valeur ])
        :: fin
        :: Noeud (Syntax Step, [ pas ])
        :: instructions )
    :: q ->
      tabs_to_string nb_tab ^ "do ("
      ^ convert_ast
          [ Noeud (Operateur Assignation, [ variable; valeur ]) ]
          env 0
      ^ ", " ^ convert_ast [ fin ] env 0 ^ ", " ^ convert_ast [ pas ] env 0
      ^ ")\n"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "end do" ^ convert_ast q env nb_tab
  | Noeud (Syntax While, condition :: instructions) :: q ->
      tabs_to_string nb_tab ^ "do while ("
      ^ convert_ast [ condition ] env 0
      ^ ")"
      ^ convert_ast instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "end do" ^ convert_ast q env nb_tab
  | Noeud (Integer s, []) :: q -> s ^ convert_ast q env nb_tab
  | Noeud (Floating s, []) :: q -> s ^ convert_ast q env nb_tab
  | Noeud (Double s, []) :: q ->
      String.fold_left
        (fun acc x -> if x = 'd' then acc ^ "e" else acc ^ String.make 1 x)
        "" s
      ^ convert_ast q env nb_tab (* convertit les d en e de fortran *)
  | Noeud (Booleen b, []) :: q ->
      (if b then "true" else "false") ^ convert_ast q env nb_tab
  | Noeud (Chaine s, []) :: q -> s ^ convert_ast q env nb_tab
  | Noeud (NewLine, []) :: q -> "\n" ^ convert_ast q env nb_tab
  | _ -> failwith "La syntaxe donnée n'est pas encore prise en charge\n"
