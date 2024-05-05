open Tokens

type ast = Noeud of token * (ast list) * (ast list)

let rec is_in_list(l: token list) (t: token): bool =
  match l with
  | [] -> false
  | x::q -> if x == t then true else is_in_list q t

(* prend tous les tokens jusqu'a un token  *)
let rec get_up_to (l: token list) (stop_token: token list) (l2: token list): token list* token list = 
  match l with
  | [] -> (List.rev l2, l) 
  | x::q -> if (is_in_list stop_token x) then (List.rev l2, l) else get_up_to q stop_token (x::l2)

let rec compact_on_token_2 (l:ast list) (t: token) : ast list =
  match l with
  | [] -> []
  | Noeud(x1,x2,x3)::Noeud(y1,[],[])::Noeud(z1,z2,z3)::q-> compact_on_token_2 (Noeud(y1,[],Noeud(x1,x2,x3)::Noeud(z1,z2,z3)::[])::q) t
  | Noeud(x1, x2, x3)::q -> Noeud(x1, x2, x3) :: compact_on_token_2 q t

let compact_ast_list (l:ast list): ast list=
  let rec compact_ast_list_inner (l:ast list) (tokens: token list): ast list=
  match tokens with
  | [] -> l
  | x::q -> compact_ast_list_inner (compact_on_token_2 l x) q
  in compact_ast_list_inner l [OperateurLogique Non; Operateur Puissance; Operateur Fois; Operateur Division; Operateur Moins; Operateur Plus;OperateurLogique Et; OperateurLogique Ou; OperateurLogique Equivalent; OperateurLogique NonEquivalent; Comparateur PlusPetit; Comparateur PlusGrand; Comparateur StrictPlusPetit; Comparateur StrictPlusGrand; Comparateur Egal; Comparateur NonEgal]

let rec create_ast (l: token list): ast list = 
  match l with
  | [] -> []
  | (DataType(x))::q -> Noeud(DataType(x), [], []) :: create_ast q
  | (Identificateur(x))::q -> Noeud(Identificateur(x), [], []) :: create_ast q
  | (Operateur(x))::q -> Noeud(Operateur(x), [], []) :: create_ast q
  | (OperateurLogique(x))::q -> Noeud(OperateurLogique(x), [], []) :: create_ast q
  | (Comparateur(x))::q -> Noeud(Comparateur(x), [], []) :: create_ast q
  | _ -> failwith "type non implémenté"


