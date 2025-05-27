open Environnement
open AbstractTokens
open Bibliotheques
open ConvertToAbstract
open LL1
open Traduction

(** Renvoie les imports nécessaires contenus dans [l] *)
let rec generate_library_imports (l : libs) : string =
  match l with
  | [] -> ""
  | name :: q -> "#include <" ^ name ^ ".h>\n" ^ generate_library_imports q

(** Renvoie le format de string dans les printf pour afficher la variable ou la
    fonction [n] à l'aide de [env] *)
let generate_format_string_of_name (n : string) (env : environnement) : string =
  match Hashtbl.find_opt env n with
  | Some (Syntax Real) -> "%f "
  | Some (Syntax Integer) -> "%d "
  | Some (Syntax Logical) -> "%d "
  | _ -> failwith "La variable n'est pas dans l'environnement"

(** Renvoie le format de string dans les printf pour afficher les éléments de
    [l] en s'aidant des types dans [env] *)
let rec generate_format_string (l : ast list) (env : environnement) : string =
  match l with
  | [] -> ""
  | Noeud (Chaine _, []) :: q -> "%s " ^ generate_format_string q env
  | Noeud (Integer _, []) :: q -> "%d " ^ generate_format_string q env
  | Noeud (Floating _, []) :: q -> "%f " ^ generate_format_string q env
  | Noeud (Booleen _, []) :: q -> "%d " ^ generate_format_string q env
  | Noeud (Name n, []) :: q ->
      generate_format_string_of_name n env ^ generate_format_string q env
  | Noeud (Syntax Call, Noeud (Name n, []) :: _) :: _ ->
      generate_format_string_of_name n env
  | Noeud (s, _) :: _ ->
      print_token s;
      failwith " type non def"

(** renvoie la chaîne associée au type de [var] dans [env] *)
let str_of_env_type (env : environnement) (var : string) : string =
  match Hashtbl.find_opt env var with
  | Some (Syntax Integer) -> "int"
  | Some (Syntax Real) -> "float"
  | Some t ->
      print_token t;
      failwith "type non supporté"
  | None -> failwith "var not found in environnement"

(** Applique la fonction [f] au dernier élément de la liste [l] *)
let rec map_to_last (f : 'a -> 'a) (l : 'a list) : 'a list =
  match l with
  | [] -> failwith "impossible to map to last"
  | e :: [] -> if e = L [] then failwith "e= []" else [ f e ]
  | e :: q -> e :: map_to_last f q

(** ajoute un point-virgule à la fin de [s] si elle n'en a pas déjà *)
let rec add_semi_colon (s : string_or_string_list) : string_or_string_list =
  match s with
  | S s -> if String.ends_with s ~suffix:";" then S s else S (s ^ ";")
  | L l -> L (map_to_last add_semi_colon l)

(** Renvoie le type de la fonction d'ast [a] à l'aide de l'environnement [env]
*)
let get_function_return_type (a : ast) (env : environnement) :
    string_or_string_list =
  match a with
  | Noeud (Syntax Function, l) -> (
      let ret = last_of_list l in
      match ret with
      | Noeud (Syntax Return, [ Noeud (Name var_name, []) ]) ->
          S (str_of_env_type env var_name)
      | _ -> failwith "type de retour non pris en charge")
  | _ -> failwith "Cette fonction prend en paramètre un Noeud(Function)"

(** parcourt la liste des fils pour trouver les définitions des paramètres *)
let rec get_function_param_list (l : ast list) (env : environnement) :
    string_or_string_list * ast list =
  match l with
  | Noeud (Name nom, []) :: Noeud (Name n, []) :: q ->
      let sosl, q2 = get_function_param_list (Noeud (Name n, []) :: q) env in
      (L [ S (str_of_env_type env nom); S " "; S nom; S ", "; sosl ], q)
  | Noeud (Name nom, []) :: q ->
      (L [ S (str_of_env_type env nom); S " "; S nom ], q)
  | l1 -> (S "", l)

(** convertit l'arbre de syntaxe abstrait [ast] à l'aide de l'environnement
    [env] et l'indente de [n_tab] en un string_or_string_list*)
let rec convert_ast_to_C_sosl (ast : ast list) (env : environnement)
    (nb_tab : int) : string_or_string_list =
  match ast with
  | [] -> S ""
  | [ Noeud (ProgramRoot, l) ] -> convert_ast_to_C_sosl l env nb_tab
  | Noeud (Syntax Program, Noeud (Name nom, []) :: l1) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "// ";
          S nom;
          S "\n";
          Traduction.tabs_to_string nb_tab;
          S "void main(void){";
          convert_ast_to_C_sosl l1 env (nb_tab + 1);
          S "}";
        ]
  | Noeud (Commentaire c, []) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "//";
          S c;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Print, l2) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "printf(\"";
          S (generate_format_string l2 env);
          S "\"";
          L
            (List.rev
               (List.fold_left
                  (fun acc x ->
                    convert_ast_to_C_sosl [ x ] env nb_tab :: S ", " :: acc)
                  [] l2));
          S ");";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  (* définit le type des variables *)
  | Noeud (Syntax Real, l) :: q ->
      let s = convert_ast_to_C_sosl l env 0 in
      let s = add_semi_colon s in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "float ";
          s;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Integer, l) :: q ->
      let s = convert_ast_to_C_sosl l env 0 in
      let s = add_semi_colon s in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "int ";
          s;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Logical, l) :: q ->
      let s = convert_ast_to_C_sosl l env 0 in
      let s = add_semi_colon s in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "bool ";
          s;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Double_precision, l) :: q ->
      let s = convert_ast_to_C_sosl l env 0 in
      let s = add_semi_colon s in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "long ";
          s;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Character, Noeud (Syntax Constant, []) :: l) :: q ->
      let s = convert_ast_to_C_sosl l env 0 in
      let s = add_semi_colon s in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "const char ";
          s;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Character, l) :: q ->
      let s = convert_ast_to_C_sosl l env 0 in
      let s = add_semi_colon s in
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "char ";
          s;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Operateur Assignation, Noeud (Name s, []) :: l) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S s;
          S " = ";
          convert_ast_to_C_sosl l env 0;
          S ";";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Size, l) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "[";
          convert_ast_to_C_sosl l env 0;
          S "] ";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Operateur Assignation, Noeud (Syntax Size, l1) :: l) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          convert_ast_to_C_sosl [ Noeud (Syntax Size, l1) ] env 0;
          convert_ast_to_C_sosl
            (Noeud (Operateur Assignation, l) :: q)
            env nb_tab;
        ]
  | Noeud (Syntax Any, []) :: q -> convert_ast_to_C_sosl q env 0
  | Noeud (Name s, []) :: q -> L [ S s; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (Operateur Plus, elem :: l) :: q ->
      L
        [
          convert_ast_to_C_sosl [ elem ] env nb_tab;
          S " + ";
          convert_ast_to_C_sosl l env nb_tab;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  (* opérateur unaire *)
  | Noeud (Operateur Moins, elem :: []) :: q ->
      L
        [
          S " -";
          convert_ast_to_C_sosl [ elem ] env nb_tab;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Operateur Moins, elem :: l) :: q ->
      L
        [
          convert_ast_to_C_sosl [ elem ] env nb_tab;
          S " - ";
          convert_ast_to_C_sosl l env nb_tab;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Operateur Fois, elem :: l) :: q ->
      L
        [
          convert_ast_to_C_sosl [ elem ] env nb_tab;
          S " * ";
          convert_ast_to_C_sosl l env nb_tab;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Operateur Division, elem :: l) :: q ->
      L
        [
          convert_ast_to_C_sosl [ elem ] env nb_tab;
          S " / ";
          convert_ast_to_C_sosl l env nb_tab;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Parentheseouvrante, []) :: q ->
      L [ S "("; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (Parenthesefermante, []) :: q ->
      L [ S ")"; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (OperateurLogique NonEquivalent, [ p1; p2 ]) :: q
  | Noeud (Comparateur NonEgal, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " != ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Equivalent, [ p1; p2 ]) :: q
  | Noeud (Comparateur Egal, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " == ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Comparateur StrictPlusPetit, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " < ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Comparateur PlusPetit, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " <= ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Comparateur StrictPlusGrand, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " > ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Comparateur PlusGrand, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " >= ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Et, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " && ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Ou, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " || ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (OperateurLogique Non, [ p1; p2 ]) :: q ->
      L
        [
          convert_ast_to_C_sosl [ p1 ] env 0;
          S " ! ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Operateur Puissance, [ p1; p2 ]) :: q ->
      L
        [
          S "pow((long)";
          convert_ast_to_C_sosl [ p1 ] env 0;
          S ", ";
          convert_ast_to_C_sosl [ p2 ] env 0;
          S "(long))";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax If, condition :: instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "if (";
          convert_ast_to_C_sosl [ condition ] env nb_tab;
          S "){";
          convert_ast_to_C_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "}";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Else_if, condition :: instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "else if (";
          convert_ast_to_C_sosl [ condition ] env nb_tab;
          S "){";
          convert_ast_to_C_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "}";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Else, instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "else {";
          convert_ast_to_C_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "}";
          convert_ast_to_C_sosl q env nb_tab;
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
          S "for (";
          convert_ast_to_C_sosl
            [ Noeud (Operateur Assignation, [ variable; valeur ]) ]
            env 0;
          S " ";
          convert_ast_to_C_sosl
            [ Noeud (Comparateur StrictPlusPetit, [ variable; fin ]) ]
            env 0;
          S "; ";
          convert_ast_to_C_sosl [ variable ] env 0;
          S "=";
          convert_ast_to_C_sosl
            [ Noeud (Operateur Plus, [ variable; pas ]) ]
            env 0;
          S ") {";
          convert_ast_to_C_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "}";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax While, condition :: instructions) :: q ->
      L
        [
          Traduction.tabs_to_string nb_tab;
          S "while (";
          convert_ast_to_C_sosl [ condition ] env 0;
          S "){";
          convert_ast_to_C_sosl instructions env (nb_tab + 1);
          Traduction.tabs_to_string nb_tab;
          S "}";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Integer s, []) :: q -> L [ S s; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (Floating s, []) :: q -> L [ S s; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (Double s, []) :: q ->
      let d =
        String.fold_left
          (fun acc x -> if x = 'd' then acc ^ "e" else acc ^ String.make 1 x)
          "" s
      in
      L [ S d; convert_ast_to_C_sosl q env nb_tab ]
      (* convertit les d en e de fortran *)
  | Noeud (Booleen b, []) :: q ->
      L
        [
          S (if b then "true" else "false"); convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Chaine s, []) :: q -> L [ S s; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (Syntax Function, Noeud (Name n, []) :: l) :: q ->
      let sosl, l2 = get_function_param_list l env in
      L
        [
          tabs_to_string nb_tab;
          get_function_return_type
            (Noeud (Syntax Function, Noeud (Name n, []) :: l))
            env;
          S " ";
          S n;
          S "(";
          sosl;
          S "){\n";
          convert_ast_to_C_sosl l2 env (nb_tab + 1);
          tabs_to_string nb_tab;
          S "}\n";
        ]
  | Noeud (Syntax Call, Noeud (Name n, []) :: l) :: q ->
      L
        [
          S n;
          S "(";
          convert_ast_to_C_sosl l env 0;
          S ")";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (Syntax Return, l) :: q ->
      L
        [
          tabs_to_string nb_tab;
          S "return ";
          convert_ast_to_C_sosl l env 0;
          S ";\n";
          convert_ast_to_C_sosl q env nb_tab;
        ]
  | Noeud (NewLine, []) :: q -> L [ S "\n"; convert_ast_to_C_sosl q env nb_tab ]
  | Noeud (t, _) :: q ->
      print_token t;
      failwith "La syntaxe donnée n'est pas encore prise en charge\n"

(** convertit l'arbre de syntaxe abstrait [ast] et ajoute les librairies
    nécessaires depuis [biblios] grace à [env] *)
let convert_ast_to_C (ast : ast list) (env : environnement)
    (biblios : Bibliotheques.libs) : string =
  generate_library_imports biblios
  ^ Traduction.string_of_string_or_string_list (convert_ast_to_C_sosl ast env 0)
