open Regex
open Automates

(** Returns the list of all the lines in the file named [file_name]. *)
let read_file (file_name: string) : string list = 
  let rec lire file liste = 
    let line = input_line file in
      ();
    try lire file (line::liste) with End_of_file->
      close_in file;
      line::liste
  in List.rev (lire (open_in file_name) [])
;;

(** Removes duplicates in the list [l]. *)
let remove_duplicates (l : 'a list) : 'a list =
  let storage = Hashtbl.create (List.length l) in 
  (** Removes the duplicates in the list [l] and stores the result in [out]. *)
  let rec remove_duplicates_aux (l : 'a list) (out : 'a list) : 'a list =
    match l with
    | [] -> out
    | x::q ->
      if Hashtbl.mem storage x then
        remove_duplicates_aux q out
      else
        (Hashtbl.add storage x ();
        remove_duplicates_aux q (x::out))
  in remove_duplicates_aux l []
;;

(** Removes the occurences of all the elements of [to_remove] in [l]. *)
let remove_form_list (l : 'a list) (to_remove : 'a list) : 'a list=
  let storage = Hashtbl.create (List.length l) in
  List.iter (fun x -> Hashtbl.add storage x ()) to_remove;
  List.filter (fun x -> not (Hashtbl.mem storage x)) l
;;

(** [define_lexemes file] returns the list of the tokens using regex found in the file [file]
and the safe token, i.e. the one used for names that can be overwritten by regular tokens *)
let define_lexemes (file : string) : (string * string) list * string =
  let safe_token = ref "" in
  let get_safe = ref false in
  let valid_char(c : char) : bool =
    let x = int_of_char c in
    (x >= 65 && x <= 90) || (x >= 48 && x <= 57) || (x >= 97 && x <= 122)
  in

  let rec get_regex (s : char list) (started : bool) (out : string * string) : string * string =
    match s, out with
    | [], _ -> out
    | '\\'::'\''::q, (s, t) -> get_regex q started (s^(String.make 1 '\''), t)
    | 's'::'\''::q, (s, t) -> if started then (s^(String.make 1 's'), t) else (get_safe := true; get_regex q true out)
    | '\''::q, (_, t) -> if started then (if !get_safe then safe_token := t; out) else get_regex q true out
    | x::q, (s, t) -> if started then get_regex q started (s^(String.make 1 x), t) else if valid_char x then get_regex q started (s, t^(String.make 1 x)) else get_regex q started out
  in

  (** Returns all the token names in [s] if they respect the regex rule [[A-Za-z0-9]]. Then it is stored in [out].
    [in_string] helps knowing if it is in string to not match there. *)
  (*let rec get_names (s : char list) (out : (string * bool) list) (in_string : bool): (string * bool) list =
    match s with
    | [] -> out
    | '\\'::'\''::q -> get_names q out in_string
    | 'r'::'\''::q ->
      if in_string then
        match out with
        | [] -> failwith "Syntaxe Invalide"
        | (x, b)::q1 -> get_names q ((x ^ String.make 1 'r', b)::q1) (not in_string)
      else
        (List.iter (fun (s, _) ->print_char '"'; print_string s; print_char '"'; print_newline()) out;
        get_names q (("", true)::out) (not in_string))
    | 's'::'\''::q ->
      if in_string then
        match out with
        | [] -> failwith "Syntaxe Invalide"
        | (x, b)::q1 -> get_names q ((x ^ String.make 1 's', b)::q1) (not in_string)
      else
        (get_safe := true;
        List.iter (fun (s, _) ->print_char '"'; print_string s; print_char '"'; print_newline()) out;
        get_names q (("", true)::out) (not in_string))
    | '\''::q ->
      if in_string then
        (if !get_safe then
          (print_int 1;
            get_safe := false;
            (*List.iter (fun (s, _) ->print_char '"'; print_string s; print_char '"'; print_newline()) out;*)
          match out with
          | (s, _)::_ -> print_string s; safe_token := s
          | _ -> failwith "impossible")
        else
          print_int 0
        ;
        get_names q out (not in_string))
      else
        get_names q (("", false)::out) (not in_string)
    | x::q ->
      if in_string then 
        get_names q out in_string
      else
        if valid_char x then
          match out with
          | [] -> get_names q [(String.make 1 x, false)] in_string
          | (x1, b)::q1 -> get_names q ((x1 ^ String.make 1 x, b)::q1) in_string
        else 
          match out with
          | ("", _)::_ -> get_names q out in_string
          | _ -> get_names q (("", false)::out) in_string
  in*)

  let lines = read_file file in
  let reg_l = List.filter (fun (x, t) -> x <> "") (List.map (fun x -> get_regex (List.of_seq (String.to_seq x)) false ("", "")) lines) in
  
  (*let names_1, names_2 = List.fold_left (fun (l1, l2) x -> List.fold_left (fun (lg, ld) (s, b) -> if b then (s::lg, ld) else (lg, s::ld)) ([], []) (get_names (List.of_seq (String.to_seq x)) [] false)) ([], []) lines in 
  let names_1 = remove_duplicates names_1 in
  let names_2 = remove_duplicates names_2 in
  let names_2 = remove_form_list names_2 names_1 in
  names_1, names_2,*)
  reg_l, !safe_token
;;

(** [generate_tokens_from_lists tok reg] generates the [tokens2.ml] file with the list [tok] of the tokens and the list [reg]
of regex expressions and the deterministic automata from [reg]. *)
let generate_tokens_from_lists (tok : string list) (reg : (string * string) list) (safe_token : string): unit =
  let file = open_out "tokens3.ml" in
  output_string file "open Regex\nopen Automates\n";
  output_string file "type token_reg =\n";
  (* removes from the list as the same time as putting in the file the tokens *)
  let tok = remove_form_list tok (List.map (fun (_, t) -> output_string file ("  | "^t^"\n"); t) reg) in

  output_string file "\ntype token =\n";
  List.iter (fun x -> output_string file ("  | "^x^"\n")) tok;
  output_string file "\nlet syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [";
  List.iter (fun (s, t) -> output_string file ("(\""^s^"\", "^t^"); ")) reg;
  output_string file "])))";
  flush file;
  close_out file
;;

(*let generate_tokens (file : string) : unit =
  let l1, l2, l3, safe_token = define_lexemes file in
  let l4 = List.map (fun (a, b) -> b) l3 in 
  generate_tokens_from_lists l4 l2
;;*)

