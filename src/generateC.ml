open Environnement
open Abstract_tokens
open Bibliotheques

let rec generate_library_imports (l: Bibliotheques.libs): string =
  match l with
  | [] -> ""
  | name::q -> "#include " ^ name ^ "\n" ^ generate_library_imports q

  

let rec generate_format_string (l:Create_ast.ast list) (env: Environnement.environnement): string =
  match l with
  | [] -> ""
  
  | Noeud(DataType Caractere _, [], [])::q -> "%s "^generate_format_string q env
  | Noeud(DataType Entier _, [], [])::q  -> "%d "^generate_format_string q env
  | Noeud(DataType Flottant _, [], [])::q -> "%f "^generate_format_string q env
  | Noeud(DataType Booleen _, [], [])::q  -> "%d "^generate_format_string q env
  | Noeud(DataType Imaginaire _, [], [])::q  -> "%d+i%d "^generate_format_string    q env

  | Noeud(Syntax Real, [], [])::q  -> "%f "^generate_format_string q env
  | Noeud(Syntax Integer, [], [])::q  -> "%i "^generate_format_string q env
  | Noeud(Syntax Logical, [], [])::q  -> "%i "^generate_format_string q env
  | Noeud(Syntax Character, [], [])::q  -> "%c "^generate_format_string q env
  
  | Noeud(Virgule, [], [])::q -> generate_format_string q env

  | Noeud(Identificateur s, [], [])::q ->(
      match get_type env s with
      | Real -> "%f "
      | Integer -> "%d "
      | _ -> failwith "type de l'identificateur inconnu" 
    ) ^ generate_format_string q env


  | _ -> failwith "type non def"

let str_of_env_type (env: environnement) (var:string) : string = 
  match List.assoc var env with 
  | Integer -> "int" 
  | Real -> "float" 
  | _ -> failwith "type non supporté"

let rec generate_function_parameter_string (prams: Create_ast.ast list) (env: environnement): string = 
  match prams with
  | [] -> ""
  | Noeud(Identificateur(nom), [],[]):: [] -> str_of_env_type env nom ^ " " ^ nom
  | Noeud(Identificateur(nom), [],[])::Noeud(Virgule, [], [])::q   
  | Noeud(Identificateur(nom), [],[])::q -> str_of_env_type env nom ^ " " ^ nom ^  ", " ^ generate_function_parameter_string q env
  | _ -> failwith "paramètres de la fonction invalides dans la sa définition"

let string_of_data_type (t: data_type): string = 
  match t with
  | Entier s | Flottant s | Commentaire s  -> s
  | Caractere s -> "\"" ^ s ^ "\"" 
  | Booleen b -> if b then "true" else "false"
  | Imaginaire _ -> failwith "non pris en charge"


let rec n_tabs (n: int): string = 
  if n > 0 then
    "\t"^n_tabs (n-1)
  else
    ""

let rec n_new_lines (n: int): string = 
  if n > 0 then
    "\n"^n_tabs (n-1)
  else
    ""

let rec convert_ast (ast: Create_ast.ast list) (env: Environnement.environnement) (tab: int) (new_lines: int): string = 
  match ast with
  | Noeud(Syntax Program, [], (Noeud(Identificateur nom, [],[]))::l1)::q ->
    n_tabs tab ^ "// " ^ nom ^ "\n"^ n_tabs tab ^
    "void main(void){\n"^ (convert_ast l1 env (tab +1) 1) ^"}"^ n_new_lines new_lines ^ convert_ast q env tab 1
  
  | Noeud(Syntax Function, [Noeud(Syntax t, [],[]);Noeud(Identificateur nom, [],[]); Noeud(Parentheseouvrante, [],l1)],instructions)::q -> 
    "void " ^ nom ^ "(" ^ generate_function_parameter_string l1 env ^ ") {\n" ^ 
    convert_ast instructions env (tab+1) 1 ^ "}\n"
    ^convert_ast q env tab 1

  | Noeud(DataType (Commentaire c), [] , [])::q ->n_tabs tab ^ "//"^c ^ n_new_lines new_lines^ convert_ast q env tab 1
  
  | Noeud(Syntax Print, [], (Noeud(Operateur Fois, [], []) )::l2)::q ->n_tabs tab ^ "printf(\""^generate_format_string l2 env^"\\n\" " ^ convert_ast l2 env tab 1 ^ ");" ^ n_new_lines new_lines ^convert_ast q env tab 1
  | Noeud(Virgule, [], [])::q->"," ^convert_ast q env tab 1
  | Noeud(PointVirgule, [], [])::q ->  ";" ^ convert_ast q env tab 1

  | Noeud(DataType t, [], [])::q ->string_of_data_type t^convert_ast q env tab 1

  (* ne transforme pas le implicit none en C car il n'a pas d'equivalent *)
  | Noeud (Syntax Implicit, [], [])::Noeud (Identificateur "none", [], [])::q ->convert_ast q env tab 1
  
  | Noeud (Identificateur i, [], [])::q -> i^convert_ast q env tab 1

  (* définit le type des variables *)
  | Noeud (Syntax Real, [] ,Noeud( QuatrePoints, [], [])::l)::q ->n_tabs tab ^ "float "^convert_ast l env tab 0^";" ^ n_new_lines new_lines^convert_ast q env tab 1
  | Noeud (Syntax Integer, [] ,Noeud( QuatrePoints, [], [])::l)::q ->n_tabs tab ^ "int "^convert_ast l env tab 0^";" ^ n_new_lines new_lines^convert_ast q env tab 1

  | Noeud (Parentheseouvrante, [], l)::q ->  "("^convert_ast l env tab 0^")"^convert_ast q env tab 1

  | Noeud (Operateur Assignation, [], Noeud(Identificateur i, [], [])::l)::q ->n_tabs tab ^  i ^ " = " ^ convert_ast l env tab 0^ ";" ^ n_new_lines new_lines ^ convert_ast q env tab 1
  
  | Noeud (Operateur Plus, [], elem::l)::q ->  (convert_ast [elem] env tab 0) ^ " + " ^ convert_ast l env tab 0 ^ convert_ast q env tab 1
  | Noeud (Operateur Moins, [], elem::l)::q ->  (convert_ast [elem] env tab 0) ^ " - " ^ convert_ast l env tab 0^ convert_ast q env tab 1
  | Noeud (Operateur Fois, [], elem::l)::q ->  (convert_ast [elem] env tab 0) ^ " * " ^ convert_ast l env tab 0^ convert_ast q env tab 1
  | Noeud (Operateur Division, [], elem::l)::q ->  (convert_ast [elem] env tab 0) ^ " / " ^ convert_ast l env tab 0^ convert_ast q env tab 1

  | Noeud (Syntax If, condition, instructions)::Noeud(Syntax Else, [] , instructions2)::q ->convert_ast [Noeud (Syntax If, condition, instructions)] env tab 0 ^ convert_ast (Noeud(Syntax Else, [] , instructions2)::q) env tab 1
  | Noeud (Syntax If, condition, instructions)::q ->n_tabs tab ^  
      "if " ^ convert_ast condition env tab 0 ^ "{\n" ^ 
        (convert_ast instructions env (tab+1) 1) ^ n_tabs tab ^ 
      "}" ^ 
      n_new_lines new_lines ^ convert_ast q env tab 1
  | Noeud (Syntax Else, [], instructions)::q ->  
      " else {\n" ^ 
        (convert_ast instructions env (tab+1) 1) ^ n_tabs tab ^ 
      "}" ^ 
      n_new_lines new_lines ^ convert_ast q env tab 1
  
  | Noeud(Syntax Do, Noeud(Operateur Assignation, [], [variable; valeur])::fin::pas::[] , instructions)::q->n_tabs tab ^ 
      "for (" ^ convert_ast [Noeud(Operateur Assignation, [], [variable; valeur])] env 0 0 ^ 
        convert_ast [Noeud(Comparateur StrictPlusPetit, [], [variable; fin])] env 0 0 ^ "; " ^ 
        convert_ast [variable] env 0 0^ "=" ^ convert_ast [Noeud(Operateur Plus, [], [variable; pas])] env 0 0 ^ ") {\n" ^ 
        convert_ast instructions env (tab+1) 1 ^ 
      n_tabs tab ^ "}" ^ 
      n_new_lines new_lines^ convert_ast q env tab 1

  | Noeud(Syntax Do, Noeud(Operateur Assignation, [], [variable; valeur])::fin::[] , instructions)::q->n_tabs tab ^ 
      "for (" ^ convert_ast [Noeud(Operateur Assignation, [], [variable; valeur])] env 0 0 ^ 
        convert_ast [Noeud(Comparateur StrictPlusPetit, [], [variable; fin])] env 0 0 ^ "; " ^ 
        convert_ast [variable] env 0 0^"=" ^convert_ast [Noeud(Operateur Plus, [], [variable; Noeud(DataType(Entier("1")), [], [])])] env 0 0 ^ ") {\n" ^ 
        convert_ast instructions env (tab+1) 1 ^ 
      n_tabs tab ^ "}" ^ 
      n_new_lines new_lines^ convert_ast q env tab 1
 

  | Noeud(Syntax While, condition, instructions)::q-> n_tabs tab ^
      "while " ^ convert_ast condition env 0 0 ^ "{\n" ^
        convert_ast instructions env (tab+1) 1 ^ 
      n_tabs tab ^ "}" ^ 
      n_new_lines new_lines^ convert_ast q env tab 1

  | Noeud (Comparateur Egal, [], elem::l)::q ->  (convert_ast [elem] env tab 0) ^ " == " ^ convert_ast l env tab 0^ convert_ast q env tab 1
  | Noeud(Comparateur StrictPlusPetit, [], [p1;p2])::q -> convert_ast [p1] env 0 0 ^ " < " ^ convert_ast [p2] env 0 0 ^ (convert_ast q env tab 1)
  | Noeud(Comparateur PlusPetit, [], [p1;p2])::q -> convert_ast [p1] env 0 0 ^ " <= " ^ convert_ast [p2] env 0 0 ^ (convert_ast q env tab 1)
  | Noeud(Comparateur StrictPlusGrand, [], [p1;p2])::q -> convert_ast [p1] env 0 0 ^ " > " ^ convert_ast [p2] env 0 0 ^ (convert_ast q env tab 1)
  | Noeud(Comparateur PlusGrand, [], [p1;p2])::q -> convert_ast [p1] env 0 0 ^ " >= " ^ convert_ast [p2] env 0 0 ^ (convert_ast q env tab 1)


  | Noeud (Commentaire _, [], [])::q -> (convert_ast q env tab 1)
  | _ ->  print_string "La syntaxe donnée n'est pas encore prise en charge\n"; ""


let convert (ast: Create_ast.ast list) (env: Environnement.environnement) (biblios: Bibliotheques.libs): string = 
  generate_library_imports biblios ^ convert_ast ast env 0 0