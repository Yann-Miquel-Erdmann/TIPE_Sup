(* open Create_ast *)
open Environnement
open Tokens

let rec generate_format_string (l:Create_ast.ast list) (env: Environnement.environnement): string =
  match l with
  | [] -> ""
  | Noeud(DataType Caractere _, [], [])::q -> "%s "^generate_format_string q env
  | Noeud(DataType Entier _, [], [])::q  -> "%d "^generate_format_string q env
  | Noeud(DataType Flottant _, [], [])::q -> "%f "^generate_format_string q env
  | Noeud(DataType Booleen _, [], [])::q  -> "%d "^generate_format_string q env
  | Noeud(DataType Imaginaire _, [], [])::q  -> "%d+i%d "^generate_format_string q env

  | Noeud(Syntax Real, [], [])::q  -> "%s "^generate_format_string q env
  | Noeud(Syntax Integer, [], [])::q  -> "%i "^generate_format_string q env
  | Noeud(Syntax Logical, [], [])::q  -> "%i "^generate_format_string q env
  | Noeud(Syntax Character, [], [])::q  -> "%c "^generate_format_string q env
  
  | Noeud(Virgule, [], [])::q -> generate_format_string q env

  | Noeud(Identificateur s, [], [])::q -> generate_format_string (Noeud(Tokens.Syntax (get_type env s) ,[],[])::q) env

  | _ -> failwith "type non def"


let string_of_data_type (t: data_type): string = 
  match t with
  | Entier s | Flottant s | Caractere s | Commentaire s -> s
  | Booleen b -> if b then "true" else "false"
  | Imaginaire _ -> failwith "non pris en charge"


let rec n_tabs (n: int): string = 
  if n > 0 then
    "\t"^n_tabs (n-1)
  else
    ""

let rec convert (ast: Create_ast.ast list) (env: Environnement.environnement) (tab: int): string = 
  match ast with
  | Noeud(Syntax Program, [], (Noeud(Identificateur nom, [],[]))::l1)::q ->n_tabs tab ^"void " ^ nom  ^ "(void){\n"^ (convert l1 env (tab +1)) ^"}\n" ^ convert q env tab
  
  | Noeud(DataType (Commentaire c), [] , [])::q ->n_tabs tab ^ "//"^c^"\n"^ convert q env tab
  
  | Noeud(Syntax Print, [], (Noeud(Operateur Fois, [], []) )::l2)::q ->n_tabs tab ^ "printf(\""^generate_format_string l2 env^"\" " ^ convert l2 env tab ^");\n"^convert q env tab
  | Noeud(Virgule, [], [])::q->","^convert q env tab
  | Noeud(DataType t, [], [])::q ->string_of_data_type t^convert q env tab

  (* ne transforme pas le implicit none en C car il n'a pas d'equivalent *)
  | Noeud (Syntax Implicit, [], [])::Noeud (Identificateur "none", [], [])::q ->convert q env tab
  
  | Noeud (Identificateur i, [], [])::q -> i^convert q env tab 

  (* définit le type des variables *)
  | Noeud (Syntax Real, [] ,Noeud( QuatrePoints, [], [])::l)::q ->n_tabs tab ^ "float "^convert l env tab^";\n"^convert q env tab
  | Noeud (Syntax Integer, [] ,Noeud( QuatrePoints, [], [])::l)::q ->n_tabs tab ^ "int "^convert l env tab^";\n"^convert q env tab

  | Noeud (Parentheseouvrante, [], l)::q ->  "("^convert l env tab^")"^convert q env tab

  | Noeud (Operateur Assignation, [], Noeud(Identificateur i, [], [])::l)::q ->n_tabs tab ^  i ^ " = " ^ convert l env tab^ ";\n" ^ convert q env tab
  
  | Noeud (Comparateur Egal, [], elem::l)::q ->  (convert [elem] env tab) ^ " == " ^ convert l env tab^ convert q env tab
  | Noeud (Operateur Plus, [], elem::l)::q ->  (convert [elem] env tab) ^ " + " ^ convert l env tab  ^ convert q env tab
  | Noeud (Operateur Moins, [], elem::l)::q ->  (convert [elem] env tab) ^ " - " ^ convert l env tab ^ convert q env tab
  | Noeud (Operateur Fois, [], elem::l)::q ->  (convert [elem] env tab) ^ " * " ^ convert l env tab ^ convert q env tab
  | Noeud (Operateur Division, [], elem::l)::q ->  (convert [elem] env tab) ^ " / " ^ convert l env tab ^ convert q env tab

  | Noeud (Syntax If, condition, instructions)::q ->n_tabs tab ^  "if " ^ convert condition env tab ^ "{\n" ^ (convert instructions env (tab+1)) ^ n_tabs tab ^ "}\n" ^ (convert q env tab)

  | Noeud (Commentaire _, [], [])::q -> convert q env tab
  | _ ->  print_string "La syntaxe donnée n'est pas encore prise en charge\n"; ""


