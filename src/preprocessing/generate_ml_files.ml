open Generate_grammar

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


(** [generate_tokens_from_lists tok reg] generates the [tokens2.ml] file with the list [tok] of the tokens and the list [reg]
of regex expressions and the deterministic automata from [reg]. *)
let generate_tokens_from_lists (tok : string list) (reg : (string * string) list) (safe_token : string) (unparsed_tokens : string list) : unit =
  let file = open_out "lexer.ml" in
  output_string file "open Regex\nopen Automates\n";
  output_string file "\nlet syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [";
  List.iter (fun (s, t) -> output_string file ("(\""^(String.escaped s)^"\", "^t^"); ")) reg;
  output_string file "])))";
  flush file;
  close_out file


let generate_tokens (file : string) : unit =
  let g = get_grammar file in
  let l, safe_token, unparsed_tokens = define_lexemes file in
  generate_tokens_from_lists (List.map (fun (s, _) -> s) g) l safe_token unparsed_tokens


let string_of_terminal (_,p: rule): string = (List.nth (List.nth p 0) 0)

(* removes the leading chars up to ' and the last ' *)
let printable_string_of_terminal (r: rule): string = 
  let s = string_of_terminal r in
  if s.[0] = '\'' then
    String.sub s 1 ((String.length s) - 2)
  else 
    String.sub s 2 ((String.length s) - 3)


let get_safe_symbol (term: rule list): string = fst (List.nth (List.filter (fun (r : rule) -> (string_of_terminal (r)).[0] = 's') term) 0)
let get_unparsed_symbol (term: rule list): string list = List.map fst (List.filter (fun (r : rule) -> (string_of_terminal (r)).[0] = '_') term)

let generate_file_lexer (g: grammar): unit = 
  let file = open_out "lexer.ml" in
  let t = terminals g in 
  print_endline "Generating the lexer.ml file...";
  output_string file "open Regex\nopen Automates\n";
  output_string file "\nlet syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [";
  List.iter (fun (s, p) -> output_string file ("(\""^String.escaped (printable_string_of_terminal (s,p))^"\", "^s^"); ")) t;
  output_string file "])))";
  flush file;
  close_out file

let generate_file_symbols (g: grammar): unit =  

  let t = ("EOF", [["'End of File'"]])::("E", [["'Epsilon'"]]):: (terminals g) in
  let nt = non_terminals g in
  
  print_endline "Generating the symbols.ml file...";
  let output_file = open_out "symbols.ml" in
  output_string output_file "type terminal =\n";
  List.iter (fun (symbol,_) -> output_string output_file ("  | "^symbol^"\n")) t;

  output_string output_file "\ntype non_terminal =\n";
  List.iter (fun (symbol,_) -> output_string output_file ("  | "^symbol^"\n")) nt;


  output_string output_file ("type symbol = | Terminal of terminal | NonTerminal of non_terminal");

  print_endline "get_safe_symbol";
  output_string output_file ("\nlet safe_token = "^get_safe_symbol t^"\n");
  print_endline "got_safe_symbol";

  output_string output_file "let unparsed_tokens = [";
  List.iter (fun x -> output_string output_file (x^"; ")) (get_unparsed_symbol t);
  output_string output_file "]\n\n";

  output_string output_file "let string_of_terminal (t : terminal) : string =\n  match t with\n";
  List.iter (fun (s, p) -> output_string output_file ("  | "^s^" -> \""^(String.escaped (printable_string_of_terminal (s,p)))^"\"\n")) t;
  
  output_string output_file "let string_of_non_terminal (nt : non_terminal) : string =\n  match nt with\n";
  List.iter (fun (s, p) -> output_string output_file ("  | "^s^" -> \""^s^"\"\n")) nt;

  flush output_file;
  close_out output_file

let generate_file_grammar (g: grammar) : unit = 
  let nt = non_terminals g in
  let non_terminals_set = StringSet.of_list (List.map fst nt) in 
  let type_string_of_symbol (s: string): string = if StringSet.mem s non_terminals_set  then "NonTerminal" else "Terminal" in 
  
  print_endline "Generating the grammar.ml file...";
  let output_file = open_out "grammar.ml" in
  output_string output_file "open Grammar_functions\nopen Symbols\n";


  output_string output_file "let g = ";
  output_string output_file "[";
  List.iter (fun (s,p: rule) ->  output_string output_file ("("^type_string_of_symbol s^" "^s^",["); List.iter (fun pattern -> output_string output_file "[";(List.iter (fun s -> output_string output_file (type_string_of_symbol s^" "^s ^ ";")) pattern ); output_string output_file "];") p; output_string output_file "]);\n") nt;
  output_string output_file "]";
  
   

  flush output_file;
  close_out output_file

let generate_files (g_path: string): unit = 
  let g = get_grammar g_path in
  Printf.printf "Grammar length %d\n" (List.length g);
  generate_file_symbols g;
  (* generate_file_lexer g; *)
  generate_file_grammar g

let _ = (
  Sys.chdir "src/";
  generate_files "../grammar/Our_Grammar.txt";
  print_endline "Finished generating, exiting.")