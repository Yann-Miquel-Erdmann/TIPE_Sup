open Tokens

(* 
  syntaxe actuelle:
    * [] ✔️ ✔️
    * |  ✔️ ✔️
    * .  ✔️ ✔️
    * *  ✔️ ✔️
    * +  ✔️ ✔️
    * a  ✔️ ✔️
    * \  ✔️

  probablement à ajouter:
    * () ✔️
    * #  ✔️
    * ?  ✔️
*)

type regex =
(* cas de base *)
| Epsilon
| Caractere of char
| AllChars
| Range of char*char
(* opérations sur les regex*)
| Concat of regex*regex
| Ou of regex*regex
| UnPlus of regex
| ZeroPlus of regex
| Vide
| Facultatif of regex
;;

type automaton =
  (* teste un par un les cractères donnés à ceux de la chaine de caractère, garde l'index dans l'entier *)
| N of string*int*token*bool 
  (* teste un par un les caractères donnés sur ceux du regex, garde l'index dans l'entier 1er entier
     stocke l'ensemble des cacarères dans la liste de chaine de caractères
     le dernier entier sert à savoir quel est l'index du regex a tester
  *)
| C of regex*int*token*int*bool*char list
;;

(* l'entier sert à savoir quel est l'index du caractère dans le mot
   le booléen sert pour savoir si l'on est à la fin de la ligne
*)
type search = int*bool;;

(* renvoie l'élément de la liste l à l'indice i *)
let rec index_list (l: 'a list) (i: int) : 'a =
  match l with
  | [] -> failwith "Invalid index"
  | x::q -> if i == 0 then x else index_list q (i-1)
;;

(* remplace l'élément de la liste l à l'indice i par l'élément e *)
let replace_index (l: 'a list) (i: int) (e:'a) : 'a list =
  let rec replace_aux (l: 'a list) (i: int) (e:'a) (out:'a list) =
    match l, i with
    | [], -1 -> List.rev out
    | [], _ -> failwith "invalid index"
    | x::q, 0 -> replace_aux q (i-1) e (e::out)
    | x::q, -1 -> replace_aux q i e (x::out)
    | x::q, _ -> replace_aux q (i-1) e (x::out)
  in replace_aux l i e []
;;

(* affiche le contenu de la liste c *)
let print_list (c: char list) : unit =
  print_char '[';
  let rec print_list_aux (c:char list) =
    match c with
    | [] -> ()
    | x::[] -> print_char x;
    | x::q -> print_char x; print_char ','; print_list_aux q;
  in print_list_aux c;
  print_char ']';
  print_newline ()
;;

let print_reg_list (c: regex list) : unit =
  let rec print_list_aux (r:regex list) : unit =
    match r with
    | []-> ()
    | [Vide] -> print_char '_';
    | Vide::q -> print_char '_'; print_char ' '; print_list_aux q

    | [Epsilon] -> print_char '#';
    | Epsilon::q -> print_char '#'; print_char ' '; print_list_aux q

    | [Caractere c] -> print_char c;
    | Caractere c ::q -> print_char c; print_char ' '; print_list_aux q

    | [AllChars] -> print_char '.';
    | AllChars::q -> print_char '.'; print_char ' '; print_list_aux q
    
    | [Range (s, e)] -> print_char '['; print_char s; print_char '-'; print_char e; print_char ']';
    | (Range (s, e))::q -> print_char '['; print_char s; print_char '-'; print_char e; print_char ']'; print_char ' '; print_list_aux q

    | [Concat (e1, e2)] -> print_list_aux [e1]; print_list_aux [e2];
    | (Concat (e1, e2))::q -> print_list_aux [e1]; print_list_aux [e2]; print_char ' '; print_list_aux q
    
    | [Ou (e1, e2)] -> print_char '('; print_list_aux [e1]; print_char '|'; print_list_aux [e2]; print_char ')';
    | (Ou (e1, e2))::q -> print_char '('; print_list_aux [e1]; print_char '|'; print_list_aux [e2]; print_char ')'; print_char ' '; print_list_aux q
    
    | [UnPlus e] -> print_char '('; print_list_aux [e]; print_string ")+";
    | (UnPlus e)::q -> print_char '('; print_list_aux [e]; print_string ")+"; print_char ' '; print_list_aux q
    
    | [ZeroPlus e] -> print_char '('; print_list_aux [e]; print_string ")*";
    | (ZeroPlus e)::q -> print_char '('; print_list_aux [e]; print_string ")*"; print_char ' '; print_list_aux q

    | [Facultatif e] -> print_char '('; print_list_aux [e]; print_string ")?";
    | (Facultatif e)::q -> print_char '('; print_list_aux [e]; print_string ")?"; print_char ' '; print_list_aux q
  in if List.length c == 0 then print_string "[]" else print_list_aux c;
;;

(* converti la chaine de cacatère s à partir de l'index index et l'ajoute à la liste c *)
let rec string_to_char_2 (s:string) (c : char list) (index: int): char list =
  if index == String.length s then
    List.rev c
  else
    string_to_char_2 s (s.[index]::c) (index + 1)
;;

exception Invalid_syntax;;
exception Empty_pile;;

let is_empty(l : 'a list ref) : bool =
  List.length !l == 0
;;

let pop (l : 'a list ref) : 'a =
  match !l with
  | [] -> raise Empty_pile
  | x::q -> l := q; x
;;

let bool_of_int (n: int) : bool =
  if n == 0 then false
  else true
;;

let or_reg (l : regex list) : regex =
  let rec or_reg_aux (l : regex list) (out : regex): regex =
  match l, out with
  | [], _ -> out
  | x::q, Epsilon -> or_reg_aux q x
  | x::q, _ -> or_reg_aux q (Ou(x, out))
  in
  or_reg_aux l Epsilon
;;

let concat_reg (l : regex list) : regex =
  let rec concat_reg_aux (l : regex list) (out : regex): regex =
  match l, out with
  | [], _ -> out
  | x::q, Epsilon -> concat_reg_aux q x
  | x::q, _ -> concat_reg_aux q (Concat(x, out))
  in
  concat_reg_aux l Epsilon
;;

let rec gen_regex (s : string) : regex =
  let caracters = ref (List.of_seq(String.to_seq s)) in
  let rec gen_regex_2 (pile : regex list) (ignore : bool) : regex =
    if is_empty(caracters) then
      concat_reg pile
    else
    let c = pop(caracters) in
    if ignore then gen_regex_2 (Caractere c::pile) false
    else
    match c with
    | '\\' -> gen_regex_2 pile true
    | '(' -> 
      begin
        let reg =
          let l = ref [] in
          try
            let count = ref 0 in
            let c = ref (pop(caracters)) in
            let i = ref false in
            while not (!c == ')' && !count == 0 && not !i) do
              if not !i then
                begin
                  if !c == '\\' then
                    i := true;
                  if !c == '(' then
                    count := !count + 1;
                  if !c == ')' then 
                    count := !count -1;
                end
              else 
                i := false;
              l := !c::!l;
              c := pop(caracters)
            done;
            let s1 = (String.of_seq (List.to_seq (List.rev !l))) in
            gen_regex s1
          with Empty_pile -> raise Invalid_syntax
      in 
      gen_regex_2 (reg::pile) false
      end
    | '[' -> 
      begin
        let reg =
          let l = ref [] in
          try
            let c = ref (pop(caracters)) in
            let i = ref 0 in
            let pile1 = ref [] in
            while not (!c == ']' && not (bool_of_int !i)) do
              if not (bool_of_int !i) && !c == '\\' then
                i := 2;
              if !i <> 2 then 
                match !pile1 with
                | [] -> pile1 := [Caractere !c]

                | [Caractere '-'; Caractere x] -> pile1 := [Range(x, !c)]
                | Caractere '-'::Caractere x::q -> pile1 := Range(x, !c)::q

                | Caractere x::q -> pile1 := Caractere !c::!pile1
                | Range (x, y)::q -> pile1 := Caractere !c::!pile1
                
                | _ -> failwith "impossible";
              ;
              if !i <> 2 then
                begin
                  l := !c::!l;
                end;
              c := pop(caracters);
              if !i > 0 then i := !i-1;
            done;
            or_reg !pile1
          with Empty_pile -> raise Invalid_syntax
      in 
      gen_regex_2 (reg::pile) false
      end
    | ')' | ']' ->
      raise Invalid_syntax
    | '|' -> 
      begin

        let left = gen_regex_2 [] false in
        match pile, left with
        | [], _ -> raise Invalid_syntax

        | [Caractere _], AllChars | [AllChars], Caractere _ |[Range _], AllChars->  gen_regex_2 [AllChars] false
        | Caractere _::q, AllChars | AllChars::q, Caractere _ | Range _::q, AllChars -> gen_regex_2 (AllChars::q) false

        | [x], _ -> gen_regex_2 [Ou(x, left)] false
        | x::q, _  -> gen_regex_2 (Ou(x, left)::q) false
      end
    | '#' -> 
      begin
        match pile with
        | [] -> gen_regex_2 [Epsilon] false
        | Concat(_, _)::_ | Caractere _::_ | Ou(_,_)::_ | Range(_, _)::_ -> gen_regex_2 pile false
        | _ -> gen_regex_2 (Epsilon::pile) false
      end
    | '+' ->
      begin
        match pile with
        | [] -> raise Invalid_syntax;
        | Epsilon::q ->  gen_regex_2 pile false
        | [x] -> gen_regex_2 [UnPlus x] false
        | x::q -> gen_regex_2 (UnPlus x::q) false
      end
    | '*' ->
      begin
        match pile with
        | [] -> raise Invalid_syntax;
        | Epsilon::q ->  gen_regex_2 pile false
        | [x] -> gen_regex_2 [ZeroPlus x] false
        | x::q -> gen_regex_2 (ZeroPlus x::q) false
      end
    | '.' -> gen_regex_2 (AllChars::pile) false
    | '?' ->
      begin
        match pile with
        | [] -> raise Invalid_syntax
        | Epsilon::q -> gen_regex_2 pile false
        | [x] -> gen_regex_2 [Facultatif x] false
        | x::q -> gen_regex_2 (Facultatif x::q) false
      end
    | _ -> gen_regex_2 (Caractere c::pile) false
  in let reg = gen_regex_2 [] false in if not (is_empty caracters) then raise Invalid_syntax else reg 
;;

let r1 = gen_regex "[a-zA-Z0-9]+";;
let r2 = gen_regex "[0-9]+\\.[0-9]+";;
let r3 = gen_regex "\".*\"";;
let r4 = gen_regex "'.*'";;
let r5 = gen_regex "[0-9]+";;
let r6 = gen_regex "!.*\n";;
let r7 = gen_regex "\".*\"";;
let r8 = gen_regex "abc";;

(*
let num = match_regex (C(num_reg, -2, Integer [], 0, false, [])) (0, false) '"'
let num = match_regex num (1, false) 'a'

let num = match_regex num (2, false) 'a'

let num = match_regex num (3, false) 'a'

let num = match_regex num (4, false) '\''

let num = match_regex num (5, false) '"'
*)