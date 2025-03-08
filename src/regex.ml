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
    * ~  ✔️
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
| AllBut of bool array
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

    | [AllBut _] -> print_string "~(..."; print_string ")";
    | (AllBut _)::q -> print_string "~(..."; print_string ")"; print_char ' '; print_list_aux q
  in if List.length c = 0 then print_string "[]" else print_list_aux c;
;;

(* convertit la chaine de cacatère s à partir de l'index index et l'ajoute à la liste c *)
let rec string_to_char_2 (s:string) (c : char list) (index: int): char list =
  if index = String.length s then
    List.rev c
  else
    string_to_char_2 s (s.[index]::c) (index + 1)
;;

exception Invalid_syntax;;
exception Empty_pile;;

let is_empty(l : 'a list ref) : bool =
  List.length !l = 0
;;

let pop (l : 'a list ref) : 'a =
  match !l with
  | [] -> raise Empty_pile
  | x::q -> l := q; x
;;

let bool_of_int (n: int) : bool =
  n <> 0
;;

(* renvoie le ou de toutes les expressions régulières dans l *)
let or_reg (l : regex list) : regex =
  let rec or_reg_aux (l : regex list) (out : regex): regex =
  match l, out with
  | [], _ -> out
  | x::q, Epsilon -> or_reg_aux q x
  | x::q, _ -> or_reg_aux q (Ou(x, out))
  in
  or_reg_aux l Epsilon
;;

(* renvoie la concaténation de toutes les expressions régulières de l*)
let concat_reg (l : regex list) : regex =
  let rec concat_reg_aux (l : regex list) (out : regex): regex =
  match l, out with
  | [], _ -> out
  | x::q, Epsilon -> concat_reg_aux q x
  | x::q, _ -> concat_reg_aux q (Concat(x, out))
  in
  concat_reg_aux l Epsilon
;;

(* fonction qui transforme, si c'est possible, la chaine de caractères s en une expression régulière (regex) *)
let rec gen_regex (s : string) : regex =
  let caracters = ref (List.of_seq(String.to_seq s)) in

  (* fonction auxiliaire qui permet de générer le regex entre parenthèses *)
  let parenthesis () =
    let l = ref [] in
    try
      let count = ref 0 in
      let c = ref (pop(caracters)) in
      let i = ref false in
      while not (!c = ')' && !count = 0 && not !i) do
        if not !i then
          begin
            if !c = '\\' then
              i := true
            else if !c = '(' then
              count := !count + 1
            else if !c = ')' then 
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

  (* fonction auxiliaire qui permet de générer le regex entre crochets *)
  let crochet () =
    let l = ref [] in
    try
      let c = ref (pop(caracters)) in
      let i = ref 0 in
      let pile1 = ref [] in
      while not (!c = ']' && not (bool_of_int !i)) do
        if not (bool_of_int !i) && !c = '\\' then
          i := 2;
        if !i <> 2 then 
          begin
            match !pile1 with
            | [Caractere '-'; Caractere x] -> pile1 := [Range(x, !c)]
            | Caractere '-'::Caractere x::q -> pile1 := Range(x, !c)::q

            | _ ->if !i = 1 && !c = 'n' then pile1 := Caractere '\n'::!pile1 else pile1 := Caractere !c::!pile1;
            l := !c::!l
          end;
        c := pop(caracters);
        if !i > 0 then i := !i-1
      done;
      or_reg !pile1
    with Empty_pile -> raise Invalid_syntax
  in

  (* fonction auxiliaire générale qui génère le regex à partir de la chaine, qui stocke au fur et à mesure dans la pile
  et qui ignore les caractères à effets lorsque ignore est à vrai *)
  let rec gen_regex_2 (pile : regex list) (ignore : bool) : regex =
    if is_empty(caracters) then
      concat_reg pile
    else
    let c = pop(caracters) in
    if ignore then if c = 'n' then gen_regex_2 (Caractere '\n'::pile) false else gen_regex_2 (Caractere c::pile) false
    else
    match c with
    | '\\' -> gen_regex_2 pile true
    | '(' -> gen_regex_2 (parenthesis()::pile) false
    | '[' -> gen_regex_2 (crochet()::pile) false
    | ')' | ']' ->
      raise Invalid_syntax
    | '|' -> 
      begin

        let left = gen_regex_2 [] false in
        match pile, left with
        | [], _ -> raise Invalid_syntax

        | [Caractere c], AllChars | [AllChars], Caractere c -> if c = '\n' then gen_regex_2 [Ou(Caractere c, AllChars)] false else gen_regex_2 [AllChars] false
        | Caractere c::q, AllChars | AllChars::q, Caractere c -> if c = '\n' then gen_regex_2 (Ou(Caractere c, AllChars)::q) false else gen_regex_2 (AllChars::q) false
        
        | [Range (d, f)], AllChars | [AllChars], Range (d, f)  -> if int_of_char d < 32 || int_of_char f < 32 then gen_regex_2 [Ou(Range (d, f), AllChars)] false else gen_regex_2 [AllChars] false
        | Range (d, f)::q, AllChars | AllChars::q, Range (d, f)-> if int_of_char d < 32 || int_of_char f < 32 then gen_regex_2 (Ou(Range (d, f), AllChars)::q) false else gen_regex_2 (AllChars::q) false

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
        | Epsilon::q -> gen_regex_2 pile false
        | [x] -> gen_regex_2 [UnPlus x] false
        | x::q -> gen_regex_2 (UnPlus x::q) false
      end
    | '*' ->
      begin
        match pile with
        | [] -> raise Invalid_syntax;
        | Epsilon::q -> gen_regex_2 pile false
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
    | '~' ->
      begin
        let c = ref (pop(caracters)) in
        match !c with
        | '[' ->
          (let reg = crochet () in
          let rec recon_crochet e =
            match e with
            | Caractere c -> gen_regex_2 (AllBut (Array.init 128 ((<>) (int_of_char c)))::pile) false
            | Range (b1, b2) -> gen_regex_2 (AllBut (Array.init 128 (fun i -> i < int_of_char b1 || i > int_of_char b2))::pile) false
            | Ou (e1, e2) ->
              (match recon_crochet e1, recon_crochet e2 with
              | AllBut l1, AllBut l2 -> gen_regex_2 (AllBut (Array.map2 (&&) l1 l2)::pile) false
              | _ -> failwith "impossible")
            | _ -> raise Invalid_syntax
          in recon_crochet reg)
        | '(' ->
          (let reg = parenthesis () in
          let rec recon_parntethis e =
            match e with
            | Caractere c -> gen_regex_2 (AllBut (Array.init 128 ((<>) (int_of_char c)))::pile) false
            | Range (b1, b2) -> gen_regex_2 (AllBut (Array.init 128 (fun i -> i < int_of_char b1 || i > int_of_char b2))::pile) false
            | AllChars -> gen_regex_2 (AllBut (Array.init 128 ((<) 32))::pile) false
            | Ou (e1, e2) ->
              (match recon_parntethis e1, recon_parntethis e2 with
              | AllBut l1, AllBut l2 -> gen_regex_2 (AllBut (Array.map2 (&&) l1 l2)::pile) false
              | _ -> failwith "impossible")
            | _ -> raise Invalid_syntax
          in
          recon_parntethis reg)
        | '\\' -> gen_regex_2 (AllBut (Array.init 128 ((<>) (int_of_char (pop(caracters)))))::pile) false
        | ')' | ']' -> raise Invalid_syntax
        | c1 -> gen_regex_2 (AllBut (Array.init 128 ((<>) (int_of_char c1)))::pile) false
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