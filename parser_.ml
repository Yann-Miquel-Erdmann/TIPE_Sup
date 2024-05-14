open Regex
open Tokens

module Parser_ = struct 

type dico = automaton list;;

let file = open_in "test.f90";;
let read_file (file_name: string):string list = 
  let rec lire file liste = 
  let line = input_line file in
    ();
  try lire file (line::liste) with End_of_file->
    close_in file;
    line::liste
  in List.rev (lire (open_in file_name) [])
;;

let rec is_one_alive (d: dico) : bool =
  match d with
  | [] -> false
  | N(s, -2, _, _)::q -> true
  | C(l, -2, _, _, _, _)::q -> true
  | _::q -> is_one_alive q
;;

let print_bool b =
  if b then print_string "true"
  else print_string "false"
;;

let rec search_list (c:'a) (l: 'a list) : bool =
  match l with
  | [] -> false
  | x::q -> if x == c then true else search_list c q
;;

let rec print_alive (d:dico) : unit =
  match d with
  | [] -> ();
  | N(s, -2, _, _)::q -> print_string s; print_string " is alive"; print_newline(); print_alive q;
  | C(l, -2, _, _, _, _)::q -> print_string "automaton is alive"; print_newline(); print_alive q;
  | _::q -> print_alive q;
;;

let search (d:dico) (s:search) (c: char): dico =
  let rec sub_search d s c d2 = 
    match d, s with
    | [], _ -> List.rev d2
    | N(s1, -2, t, false)::q, (_, index, _)->
      begin
        if (s1.[index] != c) then
          sub_search q s c (N(s1, index-1, t, false)::d2)
        else 
          if ((index + 1) == String.length s1) then 
            sub_search q s c (N(s1, index+1, t, true)::d2)
          else
              sub_search q s c (N(s1, -2, t, false)::d2)
      end
    | C(r, -2, t, i, b, l)::q, (_, index, b1) -> 
      sub_search q s c ((match_regex (C(r, -2, t, i, b, l)) s c)::d2)
    | x::q, _ -> sub_search q s c (x::d2)
  in sub_search d s c []
;;


let autoN (s:string) (t: token): automaton = N(s, -2, t, false);;
let autoC (s:string) (t: token) : automaton = C(gen_regex s, -2, t, 0, false, []);;

let rec string_to_char (s:string) (c : char list) (index: int): char list =
  if index == String.length s then
      List.rev c
  else
    if (List.length c == 0) && (int_of_char (s.[index]) == 32) then
      string_to_char s c (index+1)
    else
      begin
        (*print_char s.[index];*)
        if (int_of_char s.[index]) >= 65 && (int_of_char s.[index]) <= 90 then
          string_to_char s ((char_of_int((int_of_char(s.[index]))+32))::c) (index+1)
        else string_to_char s (s.[index]::c) (index + 1)
      end
;;

let dico = [
  autoN "allocatable" (Syntax Allocatable);
  autoN "allocate" (Syntax Allocate);
  autoN "assign" (Syntax Assign);
  autoN "assignment" (Syntax Assignment);
  autoN "block data" (Syntax Block_data);
  autoN "call" (Syntax Call);
  autoN "case" (Syntax Case);
  autoN "character" (Syntax Character);
  autoN "common" (Syntax Common);
  autoN "complex" (Syntax Complex);
  autoN "contains" (Syntax Contains);
  autoN "continue" (Syntax Continue);
  autoN "cycle" (Syntax Cycle);
  autoN "data" (Syntax Data);
  autoN "deallocate" (Syntax Deallocate);
  autoN "default" (Syntax Default);
  autoN "do" (Syntax Do);
  autoN "double precision" (Syntax Double_precision);
  autoN "else" (Syntax Else);
  autoN "elsewhere" (Syntax Elsewhere);
  autoN "entry" (Syntax Entry);
  autoN "equivalence" (Syntax Equivalence);
  autoN "exit" (Syntax Exit);
  autoN "external" (Syntax External);
  autoN "function" (Syntax Function);
  autoN "go to" (Syntax Go_to);
  autoN "goto" (Syntax Go_to);
  autoN "if" (Syntax If);
  autoN "implicit" (Syntax Implicit);
  autoN "in" (Syntax In);
  autoN "inout" (Syntax Inout);
  autoN "integer" (Syntax Integer);
  autoN "intent" (Syntax Intent);
  autoN "interface" (Syntax Interface);
  autoN "intrinsic" (Syntax Intrinsic);
  autoN "kind" (Syntax Kind);
  autoN "len" (Syntax Len);
  autoN "logical" (Syntax Logical);
  autoN "module" (Syntax Module);
  autoN "namelist" (Syntax Namelist);
  autoN "nullify" (Syntax Nullify);
  autoN "only" (Syntax Only);
  autoN "operator" (Syntax Operator);
  autoN "optional" (Syntax Optional);
  autoN "out" (Syntax Out);
  autoN "parameter" (Syntax Parameter);
  autoN "pause" (Syntax Pause);
  autoN "pointer" (Syntax Pointer);
  autoN "print" (Syntax Print);
  autoN "private" (Syntax Private);
  autoN "program" (Syntax Program);
  autoN "public" (Syntax Public);
  autoN "real" (Syntax Real);
  autoN "recursive" (Syntax Recursive);
  autoN "result" (Syntax Result);
  autoN "return" (Syntax Return);
  autoN "save" (Syntax Save);
  autoN "select case" (Syntax Select_case);
  autoN "stop" (Syntax Stop);
  autoN "subroutine" (Syntax Subroutine);
  autoN "target" (Syntax Target);
  autoN "then" (Syntax Then);
  autoN "type" (Syntax Type);
  autoN "use" (Syntax Use);
  autoN "where" (Syntax Where);
  autoN "while" (Syntax While);
  autoN "end" (Syntax End);
  autoN " " Space;
  autoN "," Virgule;
  autoN "*" (Operateur Fois);
  autoN "+" (Operateur Plus);
  autoN "=" (Operateur Assignation);
  autoN "<" (Comparateur StrictPlusPetit);
  autoN ">" (Comparateur StrictPlusGrand);
  autoN "<=" (Comparateur PlusPetit);
  autoN ">=" (Comparateur PlusGrand);
  autoN "::" QuatrePoints;
  autoN "(" Parentheseouvrante;
  autoN ")" Parenthesefermante;
  autoN "!" Commentaire;

  autoC "[0-9]+" (Integer []);
  autoC "[0-9]+\\.[0-9]+" (Floating []);
  autoC "[a-zA-Z0-9]+" (Name []);
  autoC "\".*\"" (Chaine []);
  autoC "'.*'" (Chaine []);

];;

let rec print_char_list (c: char list) : unit =
  match c with
  | [] -> ();
  | x::q -> print_char x; print_char_list q
;;
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
  | x::[], (i1, i2, b) ->
    begin
      if is_one_alive d then
        let d2 = search d (i1, i2, true) x in 
          test [] d2 (i1+1, i2+1, b)
      else
        (d, s)
      end
  | x::q, (i1, i2, _) ->
    begin
      if is_one_alive d then
        let d2 = search d s x in
          test q d2 (i1+1, i2+1, false)
      else
        (d, s)
    end
;;

(* checks for the automaton that died last *)
let rec last_alive (d:dico) (t_max:Tokens.token) (max:int) : Tokens.token option * int =
  match d with
  | [] -> if max < 0 then (None,0) else (Some t_max, max)
  | N(s, _, t, true)::q -> begin print_int (String.length s); print_char ' '; print_string s; print_newline(); if (String.length s) > max then last_alive q t (String.length s) else last_alive q t_max max end
  | C(r, _, Name _, _, true, l)::q -> if List.length l > max then last_alive q (Name (List.rev l)) (List.length l) else last_alive q t_max max
  | C(r, _, Floating _, _, true, l)::q -> if List.length l > max then last_alive q (Floating (List.rev l)) (List.length l) else last_alive q t_max max
  | C(r, _, Integer _, _, true, l)::q -> if List.length l > max then last_alive q (Integer (List.rev l)) (List.length l) else last_alive q t_max max
  | C(r, _, Chaine _, _, true, l)::q -> if List.length l > max then last_alive q (Chaine (List.rev l)) (List.length l) else last_alive q t_max max
  | C(r, _, t, _, true, l)::q -> if List.length l > max then last_alive q t (List.length l) else last_alive q t_max max
  | _::q -> last_alive q t_max max
;;

let rec clear_list (l : 'a list) (i:  int) : 'a list =
  match l with
  | [] -> print_string "end"; print_newline(); l
  | x::q -> if i > 0 then begin print_char x; print_string " cleared"; print_newline(); clear_list q (i-1) end else begin print_string "end"; print_newline(); l end 
;;

let rec end_of_text (d:dico) (endIndex: int) (out:dico): dico =
  match d with
  | [] -> List.rev out
  | C(r, -2, t, i, bool, l)::q -> end_of_text q endIndex ((C(r, endIndex, t, i, true, l))::out)
  | C(r, e, t, i, false, l)::q -> end_of_text q endIndex ((C(r, e, t, i, true, l))::out)
  | x:: q -> end_of_text q endIndex (x::out)
;;

let rec analyse_ligne (c: char list) (d: dico) (s:search) (t:Tokens.token list): Tokens.token list =
  let (i01, i02, b0) = s in
  match c with
  | [] -> t
  | c ->
    let (d, (i1, i2, b)) = test c dico s in
    let d = if i2 == List.length c then end_of_text d i2 [] else d in
    let tok = last_alive d NewLine (0) in
      match tok with
      | None, _ -> print_list c; failwith "word not recognized"
      | Some x, max -> let t = (x::t) in
    (*print_char (index_list c i2);*)
    print_int i1;print_char ':';print_int i2;print_string "->";print_int (List.length c);
    print_char '_';print_char (index_list c 0);print_int max; print_newline();
    let s = (i01+max, 0, false) in
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
      | x::q -> reverse q (x::out)
    in reverse t []
    end
  | x::q ->
    let c = string_to_char x [] 0 in
      let t = (NewLine::analyse_ligne c dico (0, 0, false) t) in
        analyse q t
;;

end 

