open Environnement
open Abstract_tokens
open Bibliotheques
open Convert_to_abstract
open LL1

let rec generate_library_imports (l : Bibliotheques.libs) : string =
  match l with
  | [] -> ""
  | name :: q -> "#include " ^ name ^ "\n" ^ generate_library_imports q

let rec generate_format_string (l : ast list) (env : environnement_v2) : string
    =
  match l with
  | [] -> ""
  | Noeud (Chaine _, []) :: q -> "%s " ^ generate_format_string q env
  | Noeud (Integer _, []) :: q -> "%d " ^ generate_format_string q env
  | Noeud (Floating _, []) :: q -> "%f " ^ generate_format_string q env
  | Noeud (Booleen _, []) :: q -> "%d " ^ generate_format_string q env
  | Noeud (Name s, []) :: q ->
      (match Hashtbl.find_opt env s with
      | Some (Syntax Real) -> "%f "
      | Some (Syntax Integer) -> "%d "
      | Some (Syntax Logical) -> "%d "
      | _ -> failwith "La variable n'est pas dans l'environnement")
      ^ generate_format_string q env
  | _ -> failwith "type non def"

let str_of_env_type (env : environnement_v2) (var : string) : string =
  match Hashtbl.find_opt env var with
  | Some (Integer _) -> "int"
  | Some (Floating _) -> "float"
  | _ -> failwith "type non supporté"

let rec generate_function_parameter_string (prams : ast list)
    (env : environnement_v2) : string =
  match prams with
  | [] -> ""
  | Noeud (Identificateur nom, []) :: [] -> str_of_env_type env nom ^ " " ^ nom
  | Noeud (Identificateur nom, []) :: Noeud (Virgule, []) :: q
  | Noeud (Identificateur nom, []) :: q ->
      str_of_env_type env nom ^ " " ^ nom ^ ", "
      ^ generate_function_parameter_string q env
  | _ -> failwith "paramètres de la fonction invalides dans la sa définition"

let string_of_data_type (t : data_type) : string =
  match t with
  | Entier s | Flottant s | Commentaire s -> s
  | Caractere s -> "\"" ^ s ^ "\""
  | Booleen b -> if b then "true" else "false"
  | Imaginaire _ -> failwith "non pris en charge"

let rec tabs_to_string (n : int) : string =
  if n > 0 then "\t" ^ tabs_to_string (n - 1) else ""

let rec n_new_lines (n : int) : string =
  if n > 0 then "\n" ^ tabs_to_string (n - 1) else ""

(* adds a semi colon at the end of the string if there is none *)
let add_semi_colon (s : string) : string =
  if String.ends_with s ~suffix:";" then s else s ^ ";"

let rec convert_ast_to_C (ast : ast list) (env : environnement_v2)
    (nb_tab : int) : string =
  match ast with
  | [] -> ""
  | [ Noeud (ProgramRoot, l) ] -> convert_ast_to_C l env nb_tab
  | Noeud (Syntax Program, Noeud (Name nom, []) :: l1) :: q ->
      tabs_to_string nb_tab ^ "// " ^ nom ^ "\n" ^ tabs_to_string nb_tab
      ^ "void main(void){"
      ^ convert_ast_to_C l1 env (nb_tab + 1)
      ^ "}"
  | Noeud (Commentaire c, []) :: q ->
      tabs_to_string nb_tab ^ "//" ^ c ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Print, l2) :: q ->
      tabs_to_string nb_tab ^ "printf(\""
      ^ generate_format_string l2 env
      ^ "\""
      ^ List.fold_left
          (fun acc x -> acc ^ ", " ^ convert_ast_to_C [ x ] env nb_tab)
          "" l2
      ^ ");"
      ^ convert_ast_to_C q env nb_tab
  (* définit le type des variables *)
  | Noeud (Syntax Real, l) :: q ->
      let s = convert_ast_to_C l env 0 in
      let s = add_semi_colon s in
      tabs_to_string nb_tab ^ "float " ^ s ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Integer, l) :: q ->
      let s = convert_ast_to_C l env 0 in
      let s = add_semi_colon s in
      tabs_to_string nb_tab ^ "int " ^ s ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Logical, l) :: q ->
      let s = convert_ast_to_C l env 0 in
      let s = add_semi_colon s in
      tabs_to_string nb_tab ^ "bool " ^ s ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Double_precision, l) :: q ->
      let s = convert_ast_to_C l env 0 in
      let s = add_semi_colon s in
      tabs_to_string nb_tab ^ "long " ^ s ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Assignation, Noeud (Name s, []) :: l) :: q ->
      tabs_to_string nb_tab ^ s ^ " = " ^ convert_ast_to_C l env 0 ^ ";"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Name s, []) :: q -> s ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Plus, elem :: l) :: q ->
      convert_ast_to_C [ elem ] env nb_tab
      ^ " + "
      ^ convert_ast_to_C l env nb_tab
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Moins, elem :: l) :: q ->
      convert_ast_to_C [ elem ] env nb_tab
      ^ " - "
      ^ convert_ast_to_C l env nb_tab
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Fois, elem :: l) :: q ->
      convert_ast_to_C [ elem ] env nb_tab
      ^ " * "
      ^ convert_ast_to_C l env nb_tab
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Division, elem :: l) :: q ->
      convert_ast_to_C [ elem ] env nb_tab
      ^ " / "
      ^ convert_ast_to_C l env nb_tab
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Parentheseouvrante, []) :: q -> "(" ^ convert_ast_to_C q env nb_tab
  | Noeud (Parenthesefermante, []) :: q -> ")" ^ convert_ast_to_C q env nb_tab
  | Noeud (OperateurLogique NonEquivalent, [ p1; p2 ]) :: q
  | Noeud (Comparateur NonEgal, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " != "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (OperateurLogique Equivalent, [ p1; p2 ]) :: q
  | Noeud (Comparateur Egal, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " == "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Comparateur StrictPlusPetit, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " < "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Comparateur PlusPetit, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " <= "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Comparateur StrictPlusGrand, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " > "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Comparateur PlusGrand, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " >= "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (OperateurLogique Et, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " && "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (OperateurLogique Ou, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " || "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (OperateurLogique Non, [ p1; p2 ]) :: q ->
      convert_ast_to_C [ p1 ] env 0
      ^ " ! "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Puissance, [ p1; p2 ]) :: q ->
      "pow((long)"
      ^ convert_ast_to_C [ p1 ] env 0
      ^ ", "
      ^ convert_ast_to_C [ p2 ] env 0
      ^ "(long))"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax If, condition :: instructions) :: q ->
      tabs_to_string nb_tab ^ "if ("
      ^ convert_ast_to_C [ condition ] env nb_tab
      ^ "){"
      ^ convert_ast_to_C instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Else_if, condition :: instructions) :: q ->
      tabs_to_string nb_tab ^ "else if ("
      ^ convert_ast_to_C [ condition ] env nb_tab
      ^ "){"
      ^ convert_ast_to_C instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Else, instructions) :: q ->
      tabs_to_string nb_tab ^ "else {"
      ^ convert_ast_to_C instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}"
      ^ convert_ast_to_C q env nb_tab
  | Noeud
      ( Syntax For,
        Noeud (Operateur Assignation, [ variable; valeur ])
        :: fin
        :: Noeud (Syntax Step, [ pas ])
        :: instructions )
    :: q ->
      tabs_to_string nb_tab ^ "for ("
      ^ convert_ast_to_C
          [ Noeud (Operateur Assignation, [ variable; valeur ]) ]
          env 0
      ^ " "
      ^ convert_ast_to_C
          [ Noeud (Comparateur StrictPlusPetit, [ variable; fin ]) ]
          env 0
      ^ "; "
      ^ convert_ast_to_C [ variable ] env 0
      ^ "="
      ^ convert_ast_to_C [ Noeud (Operateur Plus, [ variable; pas ]) ] env 0
      ^ ") {"
      ^ convert_ast_to_C instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax While, condition :: instructions) :: q ->
      tabs_to_string nb_tab ^ "while ("
      ^ convert_ast_to_C [ condition ] env 0
      ^ "){"
      ^ convert_ast_to_C instructions env (nb_tab + 1)
      ^ tabs_to_string nb_tab ^ "}"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Integer s, []) :: q -> s ^ convert_ast_to_C q env nb_tab
  | Noeud (Floating s, []) :: q -> s ^ convert_ast_to_C q env nb_tab
  | Noeud (Double s, []) :: q ->
      String.fold_left
        (fun acc x -> if x = 'd' then acc ^ "e" else acc ^ String.make 1 x)
        "" s
      ^ convert_ast_to_C q env nb_tab (* convertit les d en e de fortran *)
  | Noeud (Booleen b, []) :: q ->
      (if b then "true" else "false") ^ convert_ast_to_C q env nb_tab
  | Noeud (Chaine s, []) :: q -> s ^ convert_ast_to_C q env nb_tab
  | Noeud (NewLine, []) :: q -> "\n" ^ convert_ast_to_C q env nb_tab
  | _ -> failwith "La syntaxe donnée n'est pas encore prise en charge\n"

let convert (ast : ast list) (env : environnement_v2)
    (biblios : Bibliotheques.libs) : string =
  generate_library_imports biblios ^ convert_ast_to_C ast env 0

(*
<--------------done---------------> 
  Syntax Program
  ProgramRoot
  Commentaire s
  Syntax Print
  Syntax Integer
  Syntax Logical
  Operateur Assignation
  Name s
  Parentheseouvrante
  Parenthesefermante
  Operateur Plus
  Operateur Moins
  Operateur Fois
  Operateur Division
  Comparateur Egal
  Comparateur NonEgal
  Comparateur StrictPlusPetit
  Comparateur PlusPetit
  Comparateur StrictPlusGrand
  Comparateur PlusGrand
  OperateurLogique Equivalent
  OperateurLogique NonEquivalent
  OperateurLogique Ou
  OperateurLogique Et
  OperateurLogique Non
  Syntax If
  Syntax Else_if
  Syntax Else
  Syntax While
  Syntax For
  Syntax Step
  Integer s
  Floating s
  Chaine s
  Booleen b
  Syntax Double_precision
  Newline
  Operateur Puissance

<-----------non existent---------->
 (non implémentés pour le moment)
  Syntax Complex
  IntrinsicFunction Any (linked with char[] and char* (char[n] actually whith n the size given) => not implemented yet)

<--------make env for it---------->
  (might not make it)
  IntrinsicFunction Size

<--------------TODO--------------->
*)
