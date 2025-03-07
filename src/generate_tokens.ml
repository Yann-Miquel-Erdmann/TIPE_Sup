open Grammar

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
and the safe token, i.e. the one used for names that can be overwritten by regular tokens
and the unparsed token, which are used by the automaton but is useless during syntax analysis *)
let define_lexemes (file : string) : (string * string) list * string * string list=
  let safe_token = ref "" in
  let get_safe = ref false in
  let unparsable  = ref false in 
  let unparsed_tokens = ref [] in 

  let valid_char(c : char) : bool =
    let x = int_of_char c in
    (x >= 65 && x <= 90) || (x >= 48 && x <= 57) || (x >= 97 && x <= 122)
  in

  let rec get_regex (s : char list) (started : bool) (out : string * string) : string * string =
    match s, out with
    | [], _ -> out
    | '\\'::'\''::q, (s, t) -> get_regex q started (s^(String.make 1 '\''), t)
    | 's'::'\''::q, (s, t) -> if started then (s^(String.make 1 's'), t) else (get_safe := true; get_regex q true out)
    | '_'::'\''::q, (s, t) -> if started then (s^(String.make 1 '_'), t) else (unparsable := true; get_regex q true out)
    | '\''::q, (_, t) -> if started then (if !get_safe then safe_token := t; if !unparsable then (unparsable := false; unparsed_tokens := t::!unparsed_tokens); get_safe := false; out) else get_regex q true out
    | x::q, (s, t) -> if started then get_regex q started (s^(String.make 1 x), t) else if valid_char x then get_regex q started (s, t^(String.make 1 x)) else get_regex q started out
  in

  let lines = read_file file in
  let reg_l = List.filter (fun (x, t) -> x <> "") (List.map (fun x -> get_regex (List.of_seq (String.to_seq x)) false ("", "")) lines) in

  reg_l, !safe_token, !unparsed_tokens
;;

(** [generate_tokens_from_lists tok reg] generates the [tokens2.ml] file with the list [tok] of the tokens and the list [reg]
of regex expressions and the deterministic automata from [reg]. *)
let generate_tokens_from_lists (tok : string list) (reg : (string * string) list) (safe_token : string) (unparsed_tokens : string list) : unit =
  let file = open_out "tokens.ml" in
  output_string file "type token_reg =\n";
  (* removes from the list as the same time as putting in the file the tokens *)
  let tok = remove_form_list tok (List.map (fun (_, t) -> output_string file ("  | "^t^"\n"); t) reg) in

  output_string file "\ntype token =\n";
  List.iter (fun x -> output_string file ("  | "^x^"\n")) tok;

  output_string file ("\nlet safe_token = "^safe_token^"\n");

  output_string file "let unparsed_tokens = [";
  List.iter (fun x -> output_string file (x^"; ")) unparsed_tokens;
  output_string file "]\n\n";

  output_string file "let assoc_tok (t : token_reg) : string =\n  match t with\n";
  List.iter (fun (s, t) -> output_string file ("  | "^t^" -> \""^(String.escaped s)^"\"\n")) reg;

  flush file;
  close_out file;

  let file = open_out "lexer.ml" in
  output_string file "open Regex\nopen Automates\n";
  output_string file "\nlet syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [";
  List.iter (fun (s, t) -> output_string file ("(\""^(String.escaped s)^"\", "^t^"); ")) reg;
  output_string file "])))";
  flush file;
  close_out file
;;

let generate_tokens (file : string) : unit =
  let g = get_grammar file in
  let l, safe_token, unparsed_tokens = define_lexemes file in
  generate_tokens_from_lists (List.map (fun (s, _) -> s) g) l safe_token unparsed_tokens;
;;

let _ = (
  print_endline "Generating the tokens.ml file...";
  Sys.chdir "src/";
  generate_tokens "../grammar/Our_Grammar.txt";
  print_endline "Finised generating, exiting.")