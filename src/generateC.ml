open Environnement
open Abstract_tokens
open Bibliotheques
open Convert_to_abstract
open LL1

(* TODO add libraries *)

(** Renvoie les imports nécessaires contenus dans [l] *)
let rec generate_library_imports (l : libs) : string =
  match l with
  | [] -> ""
  | name :: q -> "#include <" ^ name ^ ".h>\n" ^ generate_library_imports q

(** Renvoie le format de string dans les printf pour afficher les éléments de
    [l] en s'aidant des types dans [env] *)
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

(** renvoie la chaîne associée au type de [var] dans [env] *)
let str_of_env_type (env : environnement_v2) (var : string) : string =
  match Hashtbl.find_opt env var with
  | Some (Integer _) -> "int"
  | Some (Floating _) -> "float"
  | _ -> failwith "type non supporté"

(** renvoie la chaîne associée à chaque variable de [params] dans [env] *)
let rec generate_function_parameter_string (params : ast list)
    (env : environnement_v2) : string =
  match params with
  | [] -> ""
  | Noeud (Identificateur nom, []) :: [] -> str_of_env_type env nom ^ " " ^ nom
  | Noeud (Identificateur nom, []) :: Noeud (Virgule, []) :: q
  | Noeud (Identificateur nom, []) :: q ->
      str_of_env_type env nom ^ " " ^ nom ^ ", "
      ^ generate_function_parameter_string q env
  | _ -> failwith "paramètres de la fonction invalides dans la sa définition"

(** convertis en chaîne la donnée constante [t] *)
let string_of_data_type (t : data_type) : string =
  match t with
  | Entier s | Flottant s | Commentaire s -> s
  | Caractere s -> "\"" ^ s ^ "\""
  | Booleen b -> if b then "true" else "false"
  | Imaginaire _ -> failwith "non pris en charge"

(** crée une chaîne de [n] tabulations *)
let tabs_to_string (n : int) : string = String.make n '\t'

(** crée une chaîne de [n] retours à la ligne *)
let rec n_new_lines (n : int) : string = String.make n '\n'

(** ajoute un point-virgule à la fin de [s] si elle n'en a pas déjà *)
let add_semi_colon (s : string) : string =
  if String.ends_with s ~suffix:";" then s else s ^ ";"

(** convertis l'arbre de syntaxe abstrait [ast] à l'aide de l'environnement
    [env] et l'indente de [n_tab] *)
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
  | Noeud (Syntax Character, Noeud (Syntax Constant, []) :: l) :: q ->
      let s = convert_ast_to_C l env 0 in
      let s = add_semi_colon s in
      tabs_to_string nb_tab ^ "const char " ^ s ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Character, l) :: q ->
      let s = convert_ast_to_C l env 0 in
      let s = add_semi_colon s in
      tabs_to_string nb_tab ^ "char " ^ s ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Assignation, Noeud (Name s, []) :: l) :: q ->
      tabs_to_string nb_tab ^ s ^ " = " ^ convert_ast_to_C l env 0 ^ ";"
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Syntax Size, l) :: q ->
      tabs_to_string nb_tab ^ "[" ^ convert_ast_to_C l env 0 ^ "] "
      ^ convert_ast_to_C q env nb_tab
  | Noeud (Operateur Assignation, Noeud (Syntax Size, l1) :: l) :: q ->
      tabs_to_string nb_tab
      ^ convert_ast_to_C [ Noeud (Syntax Size, l1) ] env 0
      ^ convert_ast_to_C (Noeud (Operateur Assignation, l) :: q) env nb_tab
  | Noeud (Syntax Any, []) :: q -> convert_ast_to_C q env 0
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

(** convertis l'arbre de syntaxe abstrait [ast] et ajoute les librairies
    nécessaires depuis [biblios] grace à [env] *)
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
  Syntax Any (linked with char[] and char* (char[n] actually whith n the size given) => not implemented yet)

<--------make env for it---------->
  (might not make it)
  Syntax Size

<--------------TODO--------------->
*)
