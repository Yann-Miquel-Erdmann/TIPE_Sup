open Regex
open Tokens
module Parser = struct 

type automaton =
| N of string*int*Tokens.token*bool (* N for normal, searches for the string *)
| C of (char list)*int*Tokens.token*bool (* C for complex, searches the regular expression ex : (1|2|3|4|5|6|7|8|9|0) *)
;;

type dico = automaton list;;

let file = open_in "test.f90";;
let read_file (file_name: string):string list = 
  let rec lire file liste = 
  let line = input_line file in
    ();
  try lire file (line::liste) with End_of_file->
    close_in file;
    line::liste
;;

let rec is_one_alive (d: dico) : bool =
  match d with
  | [] -> false
  | N(s, -2, _, _)::q -> true
  | C(l, -2, _, _, _)::q -> true
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
  | C(l, -2, _, _, _)::q -> print_string "automaton is alive"; print_newline(); print_alive q;
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
    | C(l, -2, t, _)::q, (_, index, b1) -> 
      begin
        if search_list c l then
          if b1 then 
            match t with
            | Name x -> sub_search q s c (C(l, index, (Name (List.rev (c::x))), true)::d2)
            | Integer x -> sub_search q s c (C(l, index, (Integer (List.rev (c::x))), true)::d2)
            | Floating x -> sub_search q s c (C(l, index, (Floating (List.rev (c::x))), true)::d2)
            | _ -> sub_search q s c (C(l, index, t, true)::d2)
          else 
            match t with
            | Name x -> sub_search q s c (C(l, -2, (Name (c::x)), true)::d2)
            | Integer x -> sub_search q s c (C(l, -2, (Integer (c::x)), true)::d2)
            | Floating x -> sub_search q s c (C(l, -2, (Floating (c::x)), true)::d2)
            | _ -> sub_search q s c (C(l, -2, t, true)::d2)
        else
          match t with
            | Name x -> begin sub_search q s c (C(l, index-1, (Name (List.rev x)), true)::d2) end
            | Integer x -> begin sub_search q s c (C(l, index-1, (Name (List.rev x)), true)::d2) end
            | Floating x -> begin sub_search q s c (C(l, index-1, (Name (List.rev x)), true)::d2) end
            | _ -> sub_search q s c (C(l, index-1, t, true)::d2)
      end
    | x::q, _ -> sub_search q s c (x::d2)
  in sub_search d s c []
;;


let autoN (s:string) (t: token): automaton = N(s, -2, t, false);;
let autoC (s:string) (t: token) : automaton = C(gen_regex s, -2, t, 0, false);;

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
  autoN "\"" StringDelimiter1;
  autoN "'" StringDelimiter2;
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



  
  autoC "0-9" (Integer []);
  autoC ".0-9" (Floating []);
  autoC "a-zA-Z0-9" (Name []);

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
  | C(s, _, Name _, _, _)::q -> reset_dico q (C(s, -2, Name [], 0, false)::out)
  | C(s, _, Integer _, _, _)::q -> reset_dico q (C(s, -2, Integer [], 0, false)::out)
  | C(s, _, Floating _, _, _)::q -> reset_dico q (C(s, -2, Floating [], 0, false)::out)
  | C(s, _, t, _, _)::q -> reset_dico q (C(s, -2, t, 0, false)::out)
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
  | C(_, _, Name x, true)::q -> if (List.length x) > max then begin print_int (List.length x); print_char ' '; print_int max; print_newline(); last_alive q (Name x) (List.length x) end else last_alive q t_max max
  | C(_, _, Integer x, true)::q -> if (List.length x) > max then last_alive q (Integer x) (List.length x) else last_alive q t_max max
  | C(_, _, Floating x, true)::q -> if (List.length x) > max then begin print_int (List.length x); print_char ' '; print_int max; last_alive q (Floating x) (List.length x) end else last_alive q t_max max
  | C(l, i, t, true)::q -> if i > max then last_alive q t i else last_alive q t_max max
  | _::q -> last_alive q t_max max
;;

let rec clear_list (l : 'a list) (i:  int) : 'a list =
  match l with
  | [] -> print_string "end"; print_newline(); l
  | x::q -> if i > 0 then begin print_char x; print_string " cleared"; print_newline(); clear_list q (i-1) end else begin print_string "end"; print_newline(); l end 
;;


let rec analyse_ligne (c: char list) (d: dico) (s:search) (t:Tokens.token list): Tokens.token list =
  let (i01, i02, b0) = s in
  match c with
  | [] -> t
  | c ->
    let (d, (i1, i2, b)) = test c dico s in
    let tok = last_alive d NewLine (0) in
      match tok with
      | None, max -> print_list c; failwith "word not recognized"
      | Some x, max -> let t = (x::t) in
    (*print_char (index_list c i2);*)
    print_char '_';print_int max; print_newline();
    let s = (i01+max, 0, false) in
    let d = reset_dico d [] in
    let c = clear_list c (max) in
    analyse_ligne c d s t
;;


(* outputs the token list of the tokens *)
let rec analyse (s: string list) (t: Tokens.token list): Tokens.token list =
  match s with
  | [] -> 
    begin
      let rec reverse l out=
      match l with
      | [] -> out
      | (Integer x)::q -> reverse q (DataType (Entier (String.of_seq (List.to_seq x)))::out)
      | (Floating x)::q -> reverse q (DataType (Flotant (String.of_seq (List.to_seq x)))::out)
      | (Name x)::q -> reverse q (Identificateur (String.of_seq (List.to_seq x))::out)
      | x::q -> reverse q (x::out)
    in reverse t []
    end
  | x::q ->
    let c = string_to_char x [] 0 in
      let t = (NewLine::analyse_ligne c dico (0, 0, false) t) in
        analyse q t
;;

end 

