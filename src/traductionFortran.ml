open Environnement
open Abstract_tokens
open Bibliotheques
open Convert_to_abstract
open LL1
open Traduction

let rec convert_ast_to_Fortran_sosl (ast : ast list) (env : environnement)
    (nb_tab : int) : string_or_string_list =
  match ast with
  | [] -> S ""
  | [ Noeud (ProgramRoot, l) ] -> convert_ast_to_Fortran_sosl l env nb_tab
  | Noeud (Syntax Program, Noeud (Name nom, []) :: l1) :: q ->
      L
        [
          S "program ";
          S nom;
          convert_ast_to_Fortran_sosl l1 env (nb_tab + 1);
          S "end program\n";
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Commentaire c, []) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "!";
          S c;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax Print, l2) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "print *";
          L
            (List.rev
               (List.fold_left
                  (fun acc x ->
                    convert_ast_to_Fortran_sosl [ x ] env nb_tab
                    :: S ", " :: acc)
                  [] l2));
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  (* définit le type des variables *)
  | Noeud (Syntax Real, l) :: q ->
      let s = convert_ast_to_Fortran_sosl l env 0 in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "real ::";
          s;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax Integer, l) :: q ->
      let s = convert_ast_to_Fortran_sosl l env 0 in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "integer ::";
          s;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax Logical, l) :: q ->
      let s = convert_ast_to_Fortran_sosl l env 0 in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "logical ::";
          s;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax Double_precision, l) :: q ->
      let s = convert_ast_to_Fortran_sosl l env 0 in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "double precision ::";
          s;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Operateur Assignation, Noeud (Name s, []) :: l) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S s;
          S " = ";
          convert_ast_to_Fortran_sosl l env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Name s, []) :: q ->
      L [ S s; convert_ast_to_Fortran_sosl q env nb_tab ]
  | Noeud (Operateur Plus, elem :: l) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ elem ] env nb_tab;
          S " + ";
          convert_ast_to_Fortran_sosl l env nb_tab;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Operateur Moins, elem :: l) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ elem ] env nb_tab;
          S " - ";
          convert_ast_to_Fortran_sosl l env nb_tab;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Operateur Fois, elem :: l) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ elem ] env nb_tab;
          S " * ";
          convert_ast_to_Fortran_sosl l env nb_tab;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Operateur Division, elem :: l) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ elem ] env nb_tab;
          S " / ";
          convert_ast_to_Fortran_sosl l env nb_tab;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Parentheseouvrante, []) :: q ->
      L [ S "("; convert_ast_to_Fortran_sosl q env nb_tab ]
  | Noeud (Parenthesefermante, []) :: q ->
      L [ S ")"; convert_ast_to_Fortran_sosl q env nb_tab ]
  | Noeud (OperateurLogique NonEquivalent, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " .neqv. ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Comparateur NonEgal, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " /= ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Equivalent, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " .eqv. ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Comparateur Egal, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " == ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Comparateur StrictPlusPetit, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " < ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Comparateur PlusPetit, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " <= ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Comparateur StrictPlusGrand, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " > ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Comparateur PlusGrand, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " >= ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Et, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " && ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Ou, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " || ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Non, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S " ! ";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Operateur Puissance, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_Fortran_sosl [ p1 ] env 0;
          S "**";
          convert_ast_to_Fortran_sosl [ p2 ] env 0;
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax If, condition :: instructions)
    :: Noeud (Syntax Else_if, l)
    :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "if (";
          convert_ast_to_Fortran_sosl [ condition ] env nb_tab;
          S ") do";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          convert_ast_to_Fortran_sosl
            (Noeud (Syntax Else_if, l) :: q)
            env nb_tab;
        ]
  | Noeud (Syntax If, condition :: instructions) :: Noeud (Syntax Else, l) :: q
    ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "if (";
          convert_ast_to_Fortran_sosl [ condition ] env nb_tab;
          S ") do";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          convert_ast_to_Fortran_sosl (Noeud (Syntax Else, l) :: q) env nb_tab;
        ]
  | Noeud (Syntax If, condition :: instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "if (";
          convert_ast_to_Fortran_sosl [ condition ] env nb_tab;
          S ") do";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "end if";
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax Else_if, condition :: instructions)
    :: Noeud (Syntax Else, l)
    :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "else if (";
          convert_ast_to_Fortran_sosl [ condition ] env nb_tab;
          S ")";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          convert_ast_to_Fortran_sosl (Noeud (Syntax Else, l) :: q) env nb_tab;
        ]
  | Noeud (Syntax Else_if, condition :: instructions)
    :: Noeud (Syntax Else_if, l)
    :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "else if (";
          convert_ast_to_Fortran_sosl [ condition ] env nb_tab;
          S ")";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          convert_ast_to_Fortran_sosl
            (Noeud (Syntax Else_if, l) :: q)
            env nb_tab;
        ]
  | Noeud (Syntax Else_if, condition :: instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "else if (";
          convert_ast_to_Fortran_sosl [ condition ] env nb_tab;
          S ")";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "end if";
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax Else, instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "else ";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "end if";
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud
      ( Syntax For,
        Noeud (Operateur Assignation, [ variable; valeur ])
        :: fin
        :: Noeud (Syntax Step, [ pas ])
        :: instructions )
    :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "do ";
          convert_ast_to_Fortran_sosl
            [ Noeud (Operateur Assignation, [ variable; valeur ]) ]
            env 0;
          S ", ";
          convert_ast_to_Fortran_sosl [ fin ] env 0;
          S ", ";
          convert_ast_to_Fortran_sosl [ pas ] env 0;
          S "\n";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "end do";
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Syntax While, condition :: instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "do while (";
          convert_ast_to_Fortran_sosl [ condition ] env 0;
          S ")";
          convert_ast_to_Fortran_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "end do";
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Integer s, []) :: q ->
      L [ S s; convert_ast_to_Fortran_sosl q env nb_tab ]
  | Noeud (Floating s, []) :: q ->
      L [ S s; convert_ast_to_Fortran_sosl q env nb_tab ]
  | Noeud (Double s, []) :: q ->
      L [ S s; convert_ast_to_Fortran_sosl q env nb_tab ]
      (* convertit les d en e de fortran *)
  | Noeud (Booleen b, []) :: q ->
      L
        [
          S (if b then "true" else "false");
          convert_ast_to_Fortran_sosl q env nb_tab;
        ]
  | Noeud (Chaine s, []) :: q ->
      L [ S s; convert_ast_to_Fortran_sosl q env nb_tab ]
  | Noeud (NewLine, []) :: q ->
      L [ S "\n"; convert_ast_to_Fortran_sosl q env nb_tab ]
  | _ -> failwith "La syntaxe donnée n'est pas encore prise en charge\n"

let convert_ast_to_Fortran (ast : ast list) (env : environnement) : string =
  Traduction.string_of_string_or_string_list
    (convert_ast_to_Fortran_sosl ast env 0)
