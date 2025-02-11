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

(** Returns the list of the tokens found in the file [file]
and the list of all the regex with its matching token. *)
let define_lexemes (file : string) : string list * string list * (string * string) list=
  let valid_char(c : char) : bool =
    let x = int_of_char c in
    (x >= 65 && x <= 90) || (x >= 48 && x <= 57) || (x >= 97 && x <= 122)
  in

  let rec get_regex (s : char list) (started : bool) (out : string * string) : string * string =
    match s, out with
    | [], _ -> out
    | '\\'::'\''::q, (s, t) -> get_regex q started (s^(String.make 1 '\''), t)
    | '\''::q, _ -> if started then out else get_regex q true out
    | x::q, (s, t) -> if started then get_regex q started (s^(String.make 1 x), t) else if valid_char x then get_regex q started (s, t^(String.make 1 x)) else get_regex q started out
  in

  (** Returns all the token names in [s] if they respect the regex rule [[A-Za-z0-9]]. Then it is stored in [out].
    [in_string] helps knowing if it is in string to not match there. *)
  let rec get_names (s : char list) (out : (string * bool) list) (in_string : bool): (string * bool) list =
    match s with
    | [] -> out
    | '\\'::'\''::q -> get_names q out in_string
    | 'r'::'\''::q ->
      if in_string then
        match out with
        | [] -> failwith "Syntaxe Invalide"
        | (x, b)::q1 -> get_names q ((x ^ String.make 1 'r', b)::q1) (not in_string)
      else
        get_names q (("", true)::out) (not in_string)
    | '\''::q ->
      if in_string then
        get_names q out (not in_string)
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
          | [] -> get_names q [] in_string
          | ("", _)::_ -> get_names q out in_string
          | _::_ -> get_names q (("", false)::out) in_string
  in

  (** Flattens the list [l] and stores the result in [out]. *)
  let rec flatten (l : 'a list list) (out : 'a list): 'a list =
    match l with
    | [] -> out
    | x::q -> flatten q (List.fold_left (fun acc x -> x::acc) out x)
  in
  let lines = read_file file in
  let reg_l = List.filter (fun (x, t) -> x <> "") (List.map (fun x -> get_regex (List.of_seq (String.to_seq x)) false ("", "")) lines) in
  
  let names_1, names_2 = List.fold_left (fun (l1, l2) x -> List.fold_left (fun (lg, ld) (s, b) -> if b then (s::lg, ld) else (lg, s::ld)) ([], []) (get_names (List.of_seq (String.to_seq x)) [] false)) ([], []) lines in 
  let names_1 = remove_duplicates names_1 in
  let names_2 = remove_duplicates names_2 in
  let names_2 = remove_form_list names_2 names_1 in 
  names_1, names_2, reg_l
;;

(** Generates the [tokens.ml] file with the list [l_string] of the tokens taking an argument  and the list [l_no_string]
not taking any argument. *)
let generate_tokens_from_lists (l_string : string list) (l_no_string : string list) : unit =
  let file = open_out "tokens2.ml" in
  output_string file "type token =\n";
  List.iter (fun x -> output_string file ("  | "^x^" of string\n")) l_string;
  List.iter (fun x -> output_string file ("  | "^x^"\n")) l_no_string;
  flush file;
  close_out file
;;

let generate_tokens (file : string) : unit =
  let l1, l2, l3 = define_lexemes file in
  let l4 = List.map (fun (a, b) -> b) l3 in 
  generate_tokens_from_lists l4 l2
;;