open Grammar_functions
open Symbols

module SymbolSet = Set.Make(struct 
  type t = symbol
  let compare = compare
end )


type symbol_SS_Htbl = (symbol,SymbolSet.t) Hashtbl.t


let print_SymbolSet (ss: SymbolSet.t): unit = 
  print_endline "SymbolSet(\n" ;
  SymbolSet.iter (fun s -> print_symbol s; print_newline ()) ss;
  print_endline ")\n"

let print_SymbolSet_Hastable (h: symbol_SS_Htbl): unit =
  Hashtbl.iter (fun s ss -> print_string (string_of_symbol s ^ " -> "); print_SymbolSet ss; print_newline ()) h


(* returns a hashtable containing the first set of every non terminal *)
let first (g: grammar): symbol_SS_Htbl = 
  
  (* prog dyn, filling first_h *)
  let first_h = Hashtbl.create (List.length g) in
  let h_grammar = hashed_grammar_of_grammar g in


  let rec first_of_rule (s,patterns: rule): unit =
    if is_terminal (s,patterns) then 
      Hashtbl.add first_h s (SymbolSet.singleton s)
    else
    (    (* checking if already computed *)
      if not (Hashtbl.mem first_h s) then 
        Hashtbl.add first_h s (List.fold_left (fun acc d -> let f = first_of_pattern s d  in if SymbolSet.disjoint f acc then SymbolSet.union f acc else failwith "the first set is not disjoined") SymbolSet.empty patterns)
        
      else ()
    )
  
  and first_of_pattern (s: symbol) (d: pattern): SymbolSet.t  =

    match d with
    | [] -> failwith "empty pattern"
    | first_string::q -> (
        if first_string = (Terminal E) then
          SymbolSet.singleton (Terminal E)
        else
          (first_of_rule (first_string,(Hashtbl.find h_grammar first_string)); Hashtbl.find first_h first_string )
      )
    
  in 
  (List.iter (fun (name, pattern) -> first_of_rule (name, pattern)) g);
  first_h



(* returns a hashtable containing the follow set of every non terminal (all the terminals that can occur after a rule) *)
let follow (g:grammar):  symbol_SS_Htbl  = 

  (* contains the terminals following the key in the hashtable *)
  let follow_non_terminal =  Hashtbl.of_seq (Seq.map (fun (name, pattern) -> (name, SymbolSet.empty )) (List.to_seq g)) in
  (* contains the rule names in which the key appears in at the end *)
  let parent_on_end =  Hashtbl.of_seq (Seq.map (fun (name, pattern) -> (name, SymbolSet.empty)) (List.to_seq g)) in

  (* fills the follow_non_terminal and parent_on_end *)
  let rec pattern_follow (s: symbol) (d: pattern) : unit = 
    (match d with
    | name1::name2::q -> Hashtbl.replace follow_non_terminal name1 (SymbolSet.add name2 (Hashtbl.find follow_non_terminal name1)) ; pattern_follow s (name2::q)
    | name1::[] ->
      if (name1 <> s) && (name1 <> (Terminal E)) then  
        Hashtbl.replace parent_on_end name1 (SymbolSet.add s (Hashtbl.find parent_on_end name1)) 
      else ()
    | [] -> ())
    ;
  in 

  let patterns_follow (s,patterns: rule): unit = 
    List.iter (pattern_follow s) patterns 
  in
  List.iter patterns_follow (non_terminals g);



  let first_h = first g in 
  (* contains the terminals that can appear after a key in the grammar*)
  let follow_h =  Hashtbl.create (Hashtbl.length first_h) in
  

  let rec union_and_next_on_epsilon (next_hashtable: symbol_SS_Htbl) (s: symbol)  (acc:SymbolSet.t) : SymbolSet.t = 
    

    if SymbolSet.mem (Terminal E) (Hashtbl.find next_hashtable s ) then(
      follow_of_rule (s,[]);
      SymbolSet.remove (Terminal E) (SymbolSet.union acc (SymbolSet.union (Hashtbl.find follow_h s) (Hashtbl.find next_hashtable s)))
    ) else (
      SymbolSet.union acc (Hashtbl.find next_hashtable s)
    )
    

  and

  follow_of_rule (s,_: rule): unit =

    (* checking if already computed *)
    if not (Hashtbl.mem follow_h s) then (
      
      let follow_non_terminal_set = SymbolSet.fold (union_and_next_on_epsilon first_h)  (Hashtbl.find follow_non_terminal s) (SymbolSet.singleton (Terminal EOF)) in
      
      let follow_parent_on_end_set = SymbolSet.fold (fun rn -> follow_of_rule (rn,[]); union_and_next_on_epsilon follow_h rn) (Hashtbl.find parent_on_end s) (SymbolSet.singleton (Terminal EOF)) in
      
      Hashtbl.replace follow_h s (SymbolSet.union follow_non_terminal_set follow_parent_on_end_set)

    )else (
      
    );

  in 
  List.iter follow_of_rule g;
  follow_h



(* arbre de syntaxe (non abstraite) *)
type at = Noeud of symbol * (at list)

(* let analyse_LL1 (g: grammar) (texte: token list): at = 
  let hg = hashed_grammar_of_grammar g in
  let analyse_LL1_aux (texte: token list) (rule_name: string): at*(token list) = 
    if is_terminal (rule_of_rulename  rule_name) then(
      match texte with
      | [] -> failwith "no text to match in analyse_LL1, text is empty"
      | t1::q ->if t1 = rule_name Noeud (t1, []), q
    )else(
      failwith "the expected terminal does not match the text"
    ) *)
