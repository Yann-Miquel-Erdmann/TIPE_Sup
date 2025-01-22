open Regex2
open Tokens
open Dictionnaire (* la liste des automates *)

(* lis le fichier nommé file_name et renvoie une liste des lignes *)
let read_file (file_name: string):string list = 
  let rec lire file liste = 
  let line = input_line file in
    ();
  try lire file (line::liste) with End_of_file->
    close_in file;
    line::liste
  in List.rev (lire (open_in file_name) [])
;;

(* teste si l'un des automates de la liste d'automates d est en vie
   i.e. si il peut encore accepter des caractères *)
let rec is_one_alive (d: dico) : bool =
  match d with
  | [] -> false
  | N(s, -2, _, _)::q -> true
  | C(l, -2, _, _, _, _)::q -> true
  | _::q -> is_one_alive q
;;

(* teste si l'un des caractères de la liste l correspond au caractère c *)
let rec search_list (c:'a) (l: 'a list) : bool =
  match l with
  | [] -> false
  | x::q -> if x == c then true else search_list c q
;;


let search (d:dico) (s:search) (c: char): dico =
  let rec sub_search d s c d2 = 
    match d, s with
    | [], _ -> List.rev d2
    | N(s1, -2, t, false)::q, (index, _)->
      begin
        if (s1.[index] != c && int_of_char s1.[index] != int_of_char c+32) then
          sub_search q s c (N(s1, index-1, t, false)::d2)
        else 
          if ((index + 1) == String.length s1) then 
            sub_search q s c (N(s1, index+1, t, true)::d2)
          else
              sub_search q s c (N(s1, -2, t, false)::d2)
      end
    | C(r, -2, t, i, b, l)::q, (index, b1) -> 
      sub_search q s c ((match_regex (C(r, -2, t, i, b, l)) s c)::d2)
    | x::q, _ -> sub_search q s c (x::d2)
  in sub_search d s c []
;;

(* convertis la chaine s en une liste de caractères qui la composent *)
let string_to_char (s:string) : char list =
  let rec string_to_char_aux (s:string) (c : char list) (index: int): char list =
    if index == String.length s then
        List.rev c
    else
      if (List.length c == 0) && (int_of_char (s.[index]) == 32) then
        string_to_char_aux s c (index+1)
      else
        begin
          string_to_char_aux s (s.[index]::c) (index + 1)
        end
  in string_to_char_aux s [] 0
;;

(* remet l'ensemble des automates à leur état initial *)
let rec reset_dico (d:dico) (out:dico) : dico =
  match d with
  | [] -> List.rev out
  | N(s, _, t, _)::q -> reset_dico q (N(s, -2, t, false)::out)
  | C(r, _, t, _, _, _)::q -> reset_dico q (C(reset_regex r, -2, t, 0, false, [])::out)
;;

(* ne renvoie pas s pour le moment mais besoin pour plusieurs mots *)
let rec test (str:char list) (d:dico) (s:search) : dico*search =
  match str, s with
  | [], _ -> (d, s)
  | x::[], (i1, b) ->
    begin
      if is_one_alive d then
        let d2 = search d (i1, true) x in 
          test [] d2 (i1+1, b)
      else
        (d, s)
      end
  | x::q, (i1, _) ->
    begin
      if is_one_alive d then
        let d2 = search d s x in
          test q d2 (i1+1, false)
      else
        (d, s)
    end
;;

(* checks for the automaton that died last *)
let last_alive (d:dico) : Tokens.token option * int =
  let rec last_alive_aux (d:dico) (t_max:Tokens.token) (max:int) : Tokens.token option * int =
    match d with
    | [] -> if max < 0 then (None,0) else (Some t_max, max)
    | N(s, _, t, true)::q -> if (String.length s) > max then last_alive_aux q t (String.length s) else last_alive_aux q t_max max
    | C(r, _, Name _, _, true, l)::q -> if List.length l > max then last_alive_aux q (Name (List.rev l)) (List.length l) else last_alive_aux q t_max max
    | C(r, _, Floating _, _, true, l)::q -> if List.length l > max then last_alive_aux q (Floating (List.rev l)) (List.length l) else last_alive_aux q t_max max
    | C(r, _, Integer _, _, true, l)::q -> if List.length l > max then last_alive_aux q (Integer (List.rev l)) (List.length l) else last_alive_aux q t_max max
    | C(r, _, Chaine _, _, true, l)::q -> if List.length l > max then last_alive_aux q (Chaine (List.rev l)) (List.length l) else last_alive_aux q t_max max
    | C(r, _, Commentaire _, _, true, l)::q -> if List.length l > max then last_alive_aux q (Commentaire (List.rev l)) (List.length l) else last_alive_aux q t_max max
    | C(r, _, t, _, true, l)::q -> if List.length l > max then last_alive_aux q t (List.length l) else last_alive_aux q t_max max
    | _::q -> last_alive_aux q t_max max
  in last_alive_aux d NewLine 0
;;

(* supprime les n premiers éléments de la liste l *)
let rec clear_list (l : 'a list) (n:  int) : 'a list =
  match l with
  | [] -> l
  | x::q -> if n > 0 then clear_list q (n-1) else l
;;

(* applique la fin de ligne aux différents automates de d:
      * si l'automate est un regex, *)
let rec end_of_text (d:dico) (endIndex: int) (out:dico): dico =
  match d with
  | [] -> List.rev out
  | C(r, -2, t, i, bool, l)::q ->
    let b = 
      begin
        let i = if i >= List.length r then (List.length r)-1 else i in
        match (index_list r i) with 
        | ZeroPlus (Litteral _, _) | UnPlus (Litteral _, _) -> false
        | ZeroPlus _ -> true
        | UnPlus (_, i) -> if i > 0 then true else false
        | _ -> false
      end
    in end_of_text q endIndex ((C(r, endIndex, t, i, b||bool, l))::out)
  | x:: q -> end_of_text q endIndex (x::out)
;;

let rec analyse_ligne (c: char list) (d: dico) (s:search) (t:Tokens.token list): Tokens.token list =
  match c with
  | [] -> t
  | c ->
    let (d, (i1, b)) = test c dico s in
    let d = if i1 == List.length c then end_of_text d i1 [] else d in
    let tok = last_alive d in
      match tok with
      | None, _ -> print_list c; failwith "word not recognized\n"
      | Some x, max -> let t = (x::t) in
    if max == 0 then
      begin
        print_char '\'';
        print_char (index_list c 0);
        print_string "' is not recognised\n";
        raise(Failure "")
      end
    else
      let s = (0, false) in
      let d = reset_dico d [] in
      let c = clear_list c (max) in
      analyse_ligne c d s t
;;


(* outputs the token list of the tokens *)
let rec analyse (s: string list) (t: token list): token list =
  match s with
  | [] -> 
    begin
      let rec reverse l out =
      match l with
      | [] -> out
      | (Integer x)::q -> reverse q (DataType (Entier (String.of_seq (List.to_seq x)))::out)
      | (Floating x)::q -> reverse q (DataType (Flotant (String.of_seq (List.to_seq x)))::out)
      | (Name x)::q -> reverse q (Identificateur (String.of_seq (List.to_seq x))::out)
      | (Chaine x)::q -> reverse q (DataType (Caractere (String.of_seq (List.to_seq x)))::out)
      | (Commentaire x)::q -> reverse q (DataType (Commentaire (String.of_seq (List.to_seq x)))::out)
      | x::q -> reverse q (x::out)
    in reverse t []
    end
  | x::q ->
    let c = string_to_char x in
      let t = (NewLine::analyse_ligne c dico (0, false) t) in
        analyse q t
;;

