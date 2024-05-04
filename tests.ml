let a = "a-z0-9";;

let rec string_to_char_2 (s:string) (c : char list) (index: int): char list =
  if index == String.length s then
    begin
      print_newline ();
      List.rev c
    end
  else
    if (List.length c == 0) && (int_of_char (s.[index]) == 32) then
      string_to_char_2 s c (index+1)
    else
      begin
        string_to_char_2 s (s.[index]::c) (index + 1)
      end
;;

let rec sub_gen_list (i1: int) (i2: int) (out:char list): char list =
  if i1 > i2 then
    out
  else
    begin
      print_int i1;
      print_newline ();
      sub_gen_list (i1+1) i2 ((char_of_int i1)::out)  
    end
;;

let rec gen_list (s: char list) (out: char list) ((i1, b):char*bool): char list =
  match s with
  | [] -> out
  | '-'::q ->
    begin
      if i1 <> '-' && not b then
        gen_list q out (i1, true) 
      else 
        failwith "error"
      end
  | x::q ->
    if b then
      gen_list q (sub_gen_list (int_of_char i1) (int_of_char x) out) ('-', false)
    else 
      gen_list q out (x, false)
;;

gen_list (string_to_char_2 a [] 0) [] ('-', false);;