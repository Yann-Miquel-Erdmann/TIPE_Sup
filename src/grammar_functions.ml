open Symbols

type pattern = symbol list
type rule = symbol * pattern list
type rule_hashtable = (symbol, pattern list) Hashtbl.t
type grammar = { rules_htbl : rule_hashtable; start_symbol : Symbols.symbol }

(** pour un symbole [s], renvoie sa représentation sous forme de chaîne *)
let string_of_symbol (s : symbol) : string =
  match s with
  | Terminal t -> repr_of_terminal t
  | NonTerminal nt -> string_of_non_terminal nt

(** affiche le symbole [s] *)
let print_symbol (s : symbol) : unit = print_string (string_of_symbol s)

(** affiche chaque pattern de [patterns] *)
let print_patterns (patterns : pattern list) : unit =
  List.iter
    (fun pattern ->
      List.iter (fun s -> print_string (string_of_symbol s ^ " ")) pattern;
      print_newline ())
    patterns

(** affiche chaque règle de [r] *)
let print_rule_list (r : rule_hashtable) : unit =
  Hashtbl.iter
    (fun s patterns ->
      print_string (string_of_symbol s ^ " -> \n");
      print_patterns patterns;
      print_newline ())
    r

(** affiche la grammaire [g] *)
let print_grammar (g : grammar) =
  print_string "{\nrules: ";
  print_rule_list g.rules_htbl;
  print_string "\nstart_symbol: ";
  print_symbol g.start_symbol;
  print_string "\n}\n"

(** teste si le symbole [s] est terminal *)
let is_terminal_symbol (s : symbol) : bool =
  match s with Terminal _ -> true | NonTerminal _ -> false

(** teste si le symbole [s] est non terminal *)
let is_non_terminal_symbol (s : symbol) : bool = not (is_terminal_symbol s)

(** teste si la règle associe un symbole [s] est terminal *)
let is_terminal ((s, _) : rule) : bool = is_terminal_symbol s

(** teste si la règle [r] est non terminale*)
let is_non_terminal (r : rule) : bool = not (is_terminal r)

(** renvoie la liste des terminaux de [r] *)
let terminals (r : rule list) : rule list = List.filter is_terminal r

(** renvoie la liste des non terminaux de [r] *)
let non_terminals (r : rule list) : rule list = List.filter is_non_terminal r

(** renvoie la règle associée au symbole [s] dans la grammaire [g] *)
let rule_of_symbol (g : grammar) (s : symbol) : rule =
  if Hashtbl.mem g.rules_htbl s then (s, Hashtbl.find g.rules_htbl s)
  else
    failwith
      ("the hashed grammar does not contain " ^ string_of_symbol s
     ^ " as a key\n")
