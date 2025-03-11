open Symbols

type pattern = symbol list
type rule = symbol*(pattern list)
type grammar = rule list
type hashed_grammar = (symbol , pattern list) Hashtbl.t 


let string_of_symbol (s: symbol): string = 
  match s with
  | Terminal(t) -> string_of_terminal t
  | NonTerminal(nt) -> string_of_non_terminal nt

let print_symbol (s: symbol): unit = print_string (string_of_symbol s)

let print_patterns (patterns: pattern list): unit = 
  List.iter (fun pattern -> (List.iter (fun s -> print_string ((string_of_symbol s) ^ " ")) pattern ); print_newline()) patterns

let print_grammar (g: grammar): unit = 
  List.iter (fun (s, patterns) -> print_string (string_of_symbol s ^ " -> \n"); print_patterns patterns; print_newline ();) g
  
  
let is_terminal_symbol (s: symbol): bool = 
  match s with
  | Terminal _ -> true
  | NonTerminal _ -> false

let is_non_terminal_symbol (s: symbol): bool = not (is_terminal_symbol s)

let is_terminal (s,_: rule): bool = is_terminal_symbol s

let is_non_terminal (r: rule): bool = not (is_terminal r)

(* returns the list of the terminal symbols *)
let terminals (g: grammar): grammar = List.filter is_terminal g
let non_terminals (g: grammar): grammar = List.filter is_non_terminal  g

let hashed_grammar_of_grammar (g: grammar): hashed_grammar = Hashtbl.of_seq (List.to_seq g)


let rule_of_symbol (hg: hashed_grammar) (s: symbol): rule = 
  if Hashtbl.mem hg s then 
    (s, Hashtbl.find hg s )
  else (
    failwith ("the hashed grammar hg does not contain " ^ string_of_symbol s ^" as a key\n")
  ) 



