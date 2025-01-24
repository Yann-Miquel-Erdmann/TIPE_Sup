open Tokens
open Parser2
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
  | (Syntax End)::Space::(Syntax If)::q -> Syntax End_if::merge_syntax q

  | Space::Space::q -> merge_syntax (Space::q)
  | Space::NewLine::q -> merge_syntax (NewLine::q)
  | NewLine::Space::q -> merge_syntax (NewLine::q)
  | NewLine::NewLine::q -> merge_syntax (NewLine::q)
  | x::q -> x::merge_syntax q


let rec is_in_list(l: Tokens.token list) (t: Tokens.token): bool =
  match l with
  | [] -> false
  | x::q -> if x == t then true else is_in_list q t

(* prend tous les tokens jusqu'a un Tokens.token  *)
let get_up_to (l: Tokens.token list) (stop_token: Tokens.token list): Tokens.token list* Tokens.token list = 
  let rec aux (l1: Tokens.token list)  (l2: Tokens.token list): Tokens.token list* Tokens.token list = 
    match l1 with
    | [] -> (List.rev l2, l1)
    | x::q -> if (is_in_list stop_token x) then (List.rev l2, l1) else aux q (x::l2)
  in aux l []

let rec compact_ast_list (l:ast list): ast list=
  let rec compact_on_token (l1:ast list) (t: Tokens.token) : ast list =
    match l1 with
    | [] -> [] 
    | Noeud(x1,x2,x3)::Noeud(y1,[],[])::Noeud(z1,z2,z3)::q when y1 = t-> 
        compact_on_token (Noeud(y1,[],[Noeud(x1, compact_on_token x2 t, compact_on_token x3 t);Noeud(z1, compact_on_token z2 t, compact_on_token z3 t)])::q) t

    | Noeud(x1, x2, x3)::q -> Noeud(x1, compact_on_token x2 t, compact_on_token x3 t) :: compact_on_token q t
    
  in List.fold_left compact_on_token l [Operateur Puissance; Operateur Fois; Operateur Division; Operateur Moins; Operateur Plus;OperateurLogique Et; OperateurLogique Ou; OperateurLogique Equivalent; OperateurLogique NonEquivalent; Comparateur PlusPetit; Comparateur PlusGrand; Comparateur StrictPlusPetit; Comparateur StrictPlusGrand; Comparateur Egal; Comparateur NonEgal;Operateur Assignation]



let rec create_ast (l: Tokens.token list): (ast list) * (Tokens.token list) = 
  match l with
  | [] -> [], []
  | Identificateur(x)::q1 ->let a2, q2 = create_ast q1 in Noeud(Identificateur(x), [], []) :: a2, q2
  | Operateur(x)::q1 -> let a2, q2 = create_ast q1 in Noeud(Operateur(x), [], []) :: a2, q2
  | OperateurLogique(x)::q1 ->let a2, q2 = create_ast q1 in Noeud(OperateurLogique(x), [], []) :: a2, q2
  | Comparateur(x)::q1 ->let a2, q2 = create_ast q1 in Noeud(Comparateur(x), [], []) :: a2,q2
  | (Syntax x)::q when x == Real || x == Integer ->let l2,q2 = get_up_to q [NewLine; PointVirgule] in
                                                  let a3, q3 = create_ast l2 in
                                                  let a4, q4 = create_ast q2 in
                                                  Noeud(Syntax x, [], a3)::a4, q4

  | Syntax Program:: q1 -> let a2, q2 = create_ast q1 in 
                          let a3, q3 = create_ast q2 in 
                          Noeud(Syntax Program,[], a2) :: a3 , q3
  | (Syntax End_program)::Space::Identificateur(_)::q1 -> [], q1
  | Syntax End_program::q1 -> [], q1
  
  | Syntax If::q1 -> let a2, q2 = create_ast q1 in (* s'arrête à then *)
                      let a3, q3 = create_ast q2 in (* s'arrête à end if *)
                      let a4, q4 = create_ast q3 in 
                      Noeud(Syntax If, a2, a3)::a4, q4
  | Syntax Then ::q1-> [], q1
  | Syntax End_if ::q1-> [], q1

  | Parentheseouvrante::q1 -> let a2, q2 = create_ast q1 in
                              let a3, q3 = create_ast q2 in 
                              Noeud(Parentheseouvrante, [], a2 )::a3 , q3
  | Parenthesefermante::q1-> [], q1

  | (Syntax(Print))::q1 -> let l2,q2 = get_up_to q1 [NewLine; PointVirgule] in
                          let a3, q3 = create_ast l2 in
                          let a4, q4 = create_ast q2 in
                          Noeud(Syntax Print, [], a3)::a4, q4



  | NewLine::q1 -> create_ast q1
  | Space::q1-> create_ast q1
  
  (* default *)

  | c::q1 -> let a2, q2 = create_ast q1 in  Noeud(c, [],[])::a2, q2 

let rec set_variable_list_to_env (var_list: ast list) (env: Environnement.environnement) (t: Tokens.syntax): Environnement.environnement = 
  match var_list with
  | [] -> env
  | Noeud(Identificateur(nom),[],[])::q -> let env2 = Environnement.set_type env nom t in set_variable_list_to_env q env2 t
  | _::q -> set_variable_list_to_env q env t


(* crée l'environnement de l'ast*)
let type_ast (ast1:ast list): Environnement.environnement = 
  let rec type_ast_inner (ast2: ast list) (env: Environnement.environnement): Environnement.environnement =
    match ast2 with
    | [] -> env
    | Noeud(Syntax x,l1,l2)::q when x = Tokens.Real || x = Tokens.Integer || x = Tokens.Logical || x = Tokens.Character -> set_variable_list_to_env l2 env x
    | Noeud(_,l1,l2)::q -> let env2 = type_ast_inner l1 env in let env3 = type_ast_inner l2 env2 in type_ast_inner q env3
  in type_ast_inner ast1 []


let test (file_name: string) = 
  compact_ast_list ( let a_l, l2 = 
    create_ast (
      merge_syntax (
        Parser2.exec Dictionnaire.syntax_automate_det (
          List.of_seq (String.to_seq (List.fold_left (fun acc x -> acc ^ "\n" ^ x) "" (Parser2.read_file file_name)))
        ) []
      )
    )
    in a_l
  )

