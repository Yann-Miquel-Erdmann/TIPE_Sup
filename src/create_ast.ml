open Abstract_tokens
open Environnement


type ast = Noeud of token * (ast list) * (ast list)

(* enlève les définitions des types des paramètres de la fonction et remplace les nom_de_fct = par des returns
let format_function_instructions (nom: string) (params: ast list) (l: ast  list): ast list =
  match l with
  | [] -> []
  | Noeud(Operateur Assignation, [],Noeud(Identificateur n, [],[])::v )::q when n = nom -> Noeud(Syntax Return,v,[])::
  | _ -> failwith "type non supporté" *)



(* met les syntaxes en plusieurs mots en un seul (ex: End Program -> End_program) *)
let rec merge_syntax (l:token list):token list=
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


let rec is_in_list(l:token list) (t:token): bool =
  match l with
  | [] -> false
  | x::q -> if x == t then true else is_in_list q t

(* prend tous les tokens jusqu'a untoken  *)
let get_up_to (l:token list) (stop_token:token list):token list*token list = 
  let rec aux (l1:token list)  (l2:token list):token list*token list = 
    match l1 with
    | [] -> (List.rev l2, l1)
    | x::q -> if (is_in_list stop_token x) then (List.rev l2, l1) else aux q (x::l2)
  in aux l []

let rec compact_ast_list (l:ast list): ast list=
  let rec compact_on_token (l1:ast list) (t:token) : ast list =
    match l1 with
    | [] -> [] 
    | Noeud(x1,x2,x3)::Noeud(y1,[],[])::Noeud(z1,z2,z3)::q when y1 = t-> 
        compact_on_token (Noeud(y1,[Noeud(x1, compact_on_token x2 t, compact_on_token x3 t);Noeud(z1, compact_on_token z2 t, compact_on_token z3 t)],[])::q) t

    | Noeud(x1, x2, x3)::q -> Noeud(x1, compact_on_token x2 t, compact_on_token x3 t) :: compact_on_token q t
    
  in List.fold_left compact_on_token l [Operateur Puissance; Operateur Fois; Operateur Division; Operateur Moins; Operateur Plus;OperateurLogique Et; OperateurLogique Ou; OperateurLogique Equivalent; OperateurLogique NonEquivalent; Comparateur PlusPetit; Comparateur PlusGrand; Comparateur StrictPlusPetit; Comparateur StrictPlusGrand; Comparateur Egal; Comparateur NonEgal;Operateur Assignation]



let rec create_ast (l:token list): (ast list) * (token list) = 
  match l with
  | [] -> [], []

  | Syntax Program:: q1 -> let a2, q2 = create_ast q1 in 
                          let a3, q3 = create_ast q2 in 
                          Noeud(Syntax Program,[], a2) :: a3 , q3
  | (Syntax End_program)::Space::Identificateur(_)::q1 -> [], q1
  | Syntax End_program::q1 -> [], q1


 
  (* | Syntax t :: Space :: Syntax Function ::Space:: Identificateur nom :: q1 -> (
    let a2, q2 = create_ast q1 in 
    match a2 with
    | Noeud(Parentheseouvrante, [], l1) :: a3 -> (
      let a4, q4 = create_ast q2 in 
      Noeud(Syntax Function,[Noeud(Syntax t, [],[]);Noeud(Identificateur nom, [], []);Noeud(Parentheseouvrante, [], l1)], ) :: a4 , q4
    )
    | _ -> failwith "la fonction n'a pas de parenthèses"
  )
  | (Syntax End_function)::Space::Identificateur(_)::q1 -> [], q1
  | Syntax End_function::q1 -> [], q1 *)


  | Identificateur(x)::q1 ->let a2, q2 = create_ast q1 in Noeud(Identificateur(x), [], []) :: a2, q2
  | Operateur(x)::q1 -> let a2, q2 = create_ast q1 in Noeud(Operateur(x), [], []) :: a2, q2
  | OperateurLogique(x)::q1 ->let a2, q2 = create_ast q1 in Noeud(OperateurLogique(x), [], []) :: a2, q2
  | Comparateur(x)::q1 ->let a2, q2 = create_ast q1 in Noeud(Comparateur(x), [], []) :: a2,q2
  | (Syntax x)::q when x =Real || x =Integer || x =Logical || x =Character  ->
      let l2,q2 = get_up_to q [NewLine; PointVirgule] in
      let a3, q3 = create_ast l2 in
      let a4, q4 = create_ast q2 in
      Noeud(Syntax x, [], a3)::a4, q4
  
  | Syntax If::q1 ->  let a2, q2 = create_ast q1 in (* s'arrête à then *)
                      let a3, q3 = create_ast q2 in (* s'arrête à else ou end if *)
                      (* q3 contient else ou end_if en premier elem  *)
                      (
                        match q3 with
                        | Syntax End_if::q4 -> ( 
                          let a5, q5 = create_ast q4 in 
                          Noeud(Syntax If, a2, a3)::a5, q5
                        )
                        | (Syntax Else) :: q4 -> (
                          let a5, q5 = create_ast q4 in  (* s’arrête à end_if *)
                          match q5 with

                          | Syntax End_if::q6 -> ( print_endline "else end if";
                            let a7, q7 = create_ast q6 in  
                            Noeud(Syntax If, a2, a3)::Noeud(Syntax Else, [], a5)::a7, q7 
                          )
                          | _ -> failwith "erreur de syntaxe dans le else"
                        )

                        | _ -> failwith "erreur de syntaxe dans le si"
                      )

  | Syntax Then ::q1-> [], q1
  | Syntax Else::q1-> [], Syntax Else::q1
  | Syntax End_if::q1-> [], Syntax End_if::q1


  | Syntax Do::Space::Syntax While :: q1 -> let a2,q2 = create_ast q1 in
                                            let a3,q3 = (fun a->
                                              match a with
                                              | [] -> failwith "il n'y a pas de paramètres dans le while"
                                              | condition :: q -> [condition], q
                                            ) a2 in
                                            let a4, q4 = create_ast q2 in 
                                            
                                            Noeud (Syntax While, a3, q3)::a4, q4

  | Syntax Do :: q1 -> let a2,q2 = create_ast q1 in
                       let a3, q3 = create_ast q2 in 
                       let a2 = compact_ast_list a2 in
                       (* sépare les paramètres de la boucle des expressions à l'intérieur de la boucle *)
                       let a4,a5 = (fun a ->
                          match a with
                          (* cas avec début, fin, pas *)
                          | debut::Noeud(Virgule, [], [])::fin::Noeud(Virgule, [], [])::pas::q -> [debut;fin;pas] ,q
                          (* cas avec début, fin *)
                          | debut::Noeud(Virgule, [], [])::fin::q -> [debut;fin] ,q
                          | _ -> failwith "il n'y a pas de paramètres dans pour la boucle"
                        ) a2 in
                          
                        Noeud(Syntax Do, a4, a5) :: a3 , q3

  | Syntax End_do:: q1 -> [], q1

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

let rec set_variable_list_to_env (var_list: ast list) (env: environnement) (t:token): environnement = 
  match var_list with
  | [] -> env
  | Noeud(Identificateur(nom),[],[])::q -> let env2 = set_type env nom t in 
                                          set_variable_list_to_env q env2 t
  | _::q -> set_variable_list_to_env q env t


(* crée l'environnement de l'ast*)
let env_of_ast (ast1:ast list): environnement = 
  let rec env_of_ast_inner (ast2: ast list) (env: environnement): environnement =
    match ast2 with
    | [] -> env
    | Noeud(Syntax x,[],l2)::q when x =Real || x =Integer || x =Logical || x =Character -> 
        let env2 = set_variable_list_to_env l2 env x in env_of_ast_inner q env2

    | Noeud(_,l1,l2)::q ->
        let env2 = env_of_ast_inner l1 env in 
        let env3 = env_of_ast_inner l2 env2 in 
        env_of_ast_inner q env3
  in env_of_ast_inner ast1 []


let biblio_of_ast (ast1:ast list): Bibliotheques.libs = 
  let rec biblio_of_ast_inner (ast2: ast list) (biblio: Bibliotheques.libs): Bibliotheques.libs =
    match ast2 with
    | [] -> biblio
    | Noeud(Syntax Print, _,_)::q -> let biblio2 = Bibliotheques.add_lib biblio "<stdio.h>" in biblio_of_ast_inner q biblio2
    | Noeud(_,l1,l2)::q ->
        let biblio2 = biblio_of_ast_inner l1 biblio in 
        let biblio3 = biblio_of_ast_inner l2 biblio2 in 
        biblio_of_ast_inner q biblio3

  in biblio_of_ast_inner ast1 []

