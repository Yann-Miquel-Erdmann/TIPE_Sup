open Tokens
open Parser
open Environnement

type ast = Noeud of Tokens.token * (ast list) * (ast list)

(* met les syntaxes en plusieurs mots en un seul (ex: End Program -> End_program) *)
let rec merge_syntax (l:Tokens.token list): Tokens.token list=
  match l with
  | [] -> []
  | (Syntax End)::Space::(Syntax Do)::q -> Syntax End_do::merge_syntax q
  | (Syntax End)::Space::(Syntax Function)::q -> Syntax End_function::merge_syntax q
  | (Syntax End)::Space::(Syntax Interface)::q -> Syntax End_interface::merge_syntax q
  | (Syntax End)::Space::(Syntax Module)::q -> Syntax End_module::merge_syntax q
  | (Syntax End)::Space::(Syntax Program)::q -> Syntax End_program::merge_syntax q
  | (Syntax End)::Space::(Syntax Select)::q -> Syntax End_select::merge_syntax q
  | (Syntax End)::Space::(Syntax Subroutine)::q -> Syntax End_subroutine::merge_syntax q
  | (Syntax End)::Space::(Syntax Type)::q -> Syntax End_type::merge_syntax q
  | (Syntax End)::Space::(Syntax Where)::q -> Syntax End_where::merge_syntax q
  | x::q -> x::merge_syntax q


let rec is_in_list(l: Tokens.token list) (t: Tokens.token): bool =
  match l with
  | [] -> false
  | x::q -> if x == t then true else is_in_list q t

(* prend tous les tokens jusqu'a un Tokens.token  *)
let rec get_up_to (l: Tokens.token list) (stop_token: Tokens.token list) (l2: Tokens.token list): Tokens.token list* Tokens.token list = 
  match l with
  | [] -> (List.rev l2, l) 
  | x::q -> if (is_in_list stop_token x) then (List.rev l2, l) else get_up_to q stop_token (x::l2)


let rec compact_ast_list (l:ast list): ast list=
  
  let rec compact_on_token_2 (l:ast list) (t: Tokens.token) : ast list =
    match l with
    | [] -> []
    | Noeud(x1,x2,x3)::Noeud(y1,[],[])::Noeud(z1,z2,z3)::q when y1 = t-> compact_on_token_2 (Noeud(y1,[],Noeud(x1,x2,x3)::Noeud(z1,z2,z3)::[])::q) t
    | Noeud(x1, x2, x3)::q -> Noeud(x1, compact_ast_list x2, compact_ast_list x3) :: compact_on_token_2 q t
  
  in let rec compact_ast_list_inner (l:ast list) (tokens: Tokens.token list): ast list=
  match tokens with
  | [] -> l
  | x::q -> compact_ast_list_inner (compact_on_token_2 l x) q
  in compact_ast_list_inner l [OperateurLogique Non; Operateur Puissance; Operateur Fois; Operateur Division; Operateur Moins; Operateur Plus;OperateurLogique Et; OperateurLogique Ou; OperateurLogique Equivalent; OperateurLogique NonEquivalent; Comparateur PlusPetit; Comparateur PlusGrand; Comparateur StrictPlusPetit; Comparateur StrictPlusGrand; Comparateur Egal; Comparateur NonEgal]
  (* in compact_ast_list_inner l [Operateur Plus] *)

let rec create_ast (l: Tokens.token list): ast list = 
  match l with
  | [] -> []
  | (Identificateur(x))::q -> Noeud(Identificateur(x), [], []) :: create_ast q
  | (Operateur(x))::q -> Noeud(Operateur(x), [], []) :: create_ast q
  | (OperateurLogique(x))::q -> Noeud(OperateurLogique(x), [], []) :: create_ast q
  | (Comparateur(x))::q -> Noeud(Comparateur(x), [], []) :: create_ast q
  

  | (DataType(x))::q ->let l2,q2 = get_up_to q [NewLine] [] in Noeud(DataType(x), create_ast l2, []) :: create_ast q2

  (* fin de programme *)
  | (Syntax End_program)::q ->let l2,q2 = get_up_to q [NewLine] [] in  Noeud(Syntax End,[],[]):: Noeud(Syntax Program ,[], create_ast l2) ::create_ast q2
  (* création de programe *)
  | (Syntax (Program) ):: q -> let l2,q2 = get_up_to q [Syntax(End_program)] [] in  Noeud(Syntax Program,[], create_ast l2) ::create_ast q2 
  (* définitions de variables *)
  | (Syntax x)::q when x = Real || x = Integer ->let l2,q2 = get_up_to q [NewLine] [] in  Noeud(Syntax Real,[], create_ast l2) ::create_ast q2
  (* print *)
  | (Syntax(Print))::q  -> let l2,q2 = get_up_to q [NewLine] [] in  Noeud(Syntax Real,[], create_ast l2) ::create_ast q2

  | NewLine::q | Space::q-> create_ast q
  | c::q -> Noeud(c, [],[])::create_ast q 
  

let rec set_variable_list_to_env (var_list: ast list) (env: environnement) (t: Tokens.data_type): environnement = 
  match var_list with
  | [] -> env
  | Noeud(Identificateur(nom),[],[])::q -> let env2 = set_type env nom t in set_variable_list_to_env q env2 t
  | _::q -> set_variable_list_to_env q env t
  

let rec type_ast (ast: ast list) (env: environnement): environnement =
  match ast with
  | [] -> env
  | Noeud(DataType(x),l1,l2)::q -> env
  | Noeud(_,l1,l2)::q -> let env2 = type_ast l1 env in let env3 = type_ast l2 env2 in type_ast q env3


let test (file_name: string) = 
  compact_ast_list (
    create_ast (
      merge_syntax (
        Parser.analyse (
          Parser.read_file file_name
        ) []
      )
    )
  )
