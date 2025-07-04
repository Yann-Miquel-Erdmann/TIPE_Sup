open GrammarFunctions
open Symbols

module SymbolSet = Set.Make (struct
  type t = symbol

  let compare = compare
end)

(* arbre de syntaxe (non abstraite) *)
type at = Noeud of (symbol * string) * at list
type symbol_SS_Htbl = (symbol, SymbolSet.t) Hashtbl.t

let print_SymbolSet (ss : SymbolSet.t) : unit =
  print_endline "SymbolSet(";
  SymbolSet.iter
    (fun s ->
      print_symbol s;
      print_newline ())
    ss;
  print_endline ")\n"

let print_SymbolSet_Hastable (h : symbol_SS_Htbl) : unit =
  Hashtbl.iter
    (fun s ss ->
      print_string (string_of_symbol s ^ " -> ");
      print_SymbolSet ss;
      print_newline ())
    h

(* returns a hashtable containing the first set of every non terminal *)
let first (g : grammar) : symbol_SS_Htbl =
  (* prog dyn, filling first_h *)
  let first_h = Hashtbl.create (Hashtbl.length g.rules_htbl) in

  let rec first_of_rule ((s, patterns) : rule) : unit =
    if is_terminal (s, patterns) then
      Hashtbl.add first_h s (SymbolSet.singleton s)
    else if
      (* checking if already computed *)
      not (Hashtbl.mem first_h s)
    then
      Hashtbl.add first_h s
        (List.fold_left
           (fun acc d ->
             let f = first_of_pattern s d in
             if SymbolSet.disjoint f acc then SymbolSet.union f acc
             else (
               print_symbol s;
               print_newline ();
               print_SymbolSet f;
               prerr_newline ();
               print_SymbolSet acc;
               print_newline ();
               failwith "the first set is not disjoined"))
           SymbolSet.empty patterns)
    else ()
  and first_of_pattern (s : symbol) (d : pattern) : SymbolSet.t =
    match d with
    | [] -> failwith "empty pattern"
    | Terminal t :: q -> SymbolSet.singleton (Terminal t)
    | fst_symbol :: q ->
        first_of_rule (fst_symbol, Hashtbl.find g.rules_htbl fst_symbol);
        if
          SymbolSet.mem (Terminal E) (Hashtbl.find first_h fst_symbol)
          && q <> []
        then
          SymbolSet.union (first_of_pattern s q)
            (SymbolSet.remove (Terminal E) (Hashtbl.find first_h fst_symbol))
        else Hashtbl.find first_h fst_symbol
  in
  Hashtbl.iter (fun name pattern -> first_of_rule (name, pattern)) g.rules_htbl;
  first_h

let rec first_of_pattern (fst_ss_htbl : symbol_SS_Htbl) (p : pattern) :
    SymbolSet.t =
  (* print_endline ("first_of_pattern "); print_patterns [p];print_newline(); *)
  match p with
  | [] -> failwith "empty pattern in first_of_pattern"
  | Terminal t :: _ -> SymbolSet.singleton (Terminal t)
  | NonTerminal nt :: q ->
      if
        SymbolSet.mem (Terminal E) (Hashtbl.find fst_ss_htbl (NonTerminal nt))
        && q <> []
      then
        SymbolSet.union
          (first_of_pattern fst_ss_htbl q)
          (SymbolSet.remove (Terminal E)
             (Hashtbl.find fst_ss_htbl (NonTerminal nt)))
      else Hashtbl.find fst_ss_htbl (NonTerminal nt)

(* returns a hashtable containing the follow set of every non terminal (all the terminals that can occur after a rule) *)
let follow (g : grammar) : symbol_SS_Htbl =
  (* contains the terminals following the key in the hashtable *)
  let follow_non_terminal =
    Hashtbl.of_seq
      (Seq.map
         (fun (name, pattern) -> (name, SymbolSet.empty))
         (Hashtbl.to_seq g.rules_htbl))
  in

  (* contains the rule names in which the key appears in at the end *)
  let parent_on_end =
    Hashtbl.of_seq
      (Seq.map
         (fun (name, pattern) -> (name, SymbolSet.empty))
         (Hashtbl.to_seq g.rules_htbl))
  in

  (* fills the follow_non_terminal and parent_on_end *)
  let rec pattern_follow (s : symbol) (d : pattern) : unit =
    match d with
    | NonTerminal s1 :: s2 :: q ->
        Hashtbl.replace follow_non_terminal (NonTerminal s1)
          (SymbolSet.add s2 (Hashtbl.find follow_non_terminal (NonTerminal s1)));
        pattern_follow s (s2 :: q)
    | Terminal s1 :: q -> pattern_follow s q
    | NonTerminal s1 :: [] ->
        if NonTerminal s1 <> s && NonTerminal s1 <> Terminal E then
          Hashtbl.replace parent_on_end (NonTerminal s1)
            (SymbolSet.add s (Hashtbl.find parent_on_end (NonTerminal s1)))
        else ()
    | [] -> ()
  in

  let patterns_follow (s : symbol) (patterns : pattern list) : unit =
    List.iter (pattern_follow s) patterns
  in
  Hashtbl.iter patterns_follow g.rules_htbl;

  let first_h = first g in
  (* contains the terminals that can appear after a key in the grammar*)
  let follow_h = Hashtbl.create (Hashtbl.length first_h) in

  let rec union_and_next_on_epsilon (next_hashtable : symbol_SS_Htbl)
      (s : symbol) (acc : SymbolSet.t) : SymbolSet.t =
    if is_non_terminal_symbol s then
      if SymbolSet.mem (Terminal E) (Hashtbl.find next_hashtable s) then (
        follow_of_symbol s;
        SymbolSet.remove (Terminal E)
          (SymbolSet.union acc
             (SymbolSet.union (Hashtbl.find follow_h s)
                (Hashtbl.find next_hashtable s))))
      else SymbolSet.union acc (Hashtbl.find next_hashtable s)
    else SymbolSet.singleton s
  and follow_of_symbol (s : symbol) : unit =
    (* checking if already computed *)
    if not (Hashtbl.mem follow_h s) then
      let follow_non_terminal_set =
        SymbolSet.fold
          (union_and_next_on_epsilon first_h)
          (Hashtbl.find follow_non_terminal s)
          (SymbolSet.singleton (Terminal EOF))
      in
      let follow_parent_on_end_set =
        SymbolSet.fold
          (fun s ->
            follow_of_symbol s;
            union_and_next_on_epsilon follow_h s)
          (Hashtbl.find parent_on_end s)
          (SymbolSet.singleton (Terminal EOF))
      in

      Hashtbl.replace follow_h s
        (SymbolSet.union follow_non_terminal_set follow_parent_on_end_set)
    else ()
  in
  Hashtbl.iter (fun s _ -> follow_of_symbol s) g.rules_htbl;
  follow_h

let analyse_LL1_of_symbol (g : grammar) (text : (symbol * string) list)
    (s : symbol) : at * (symbol * string) list =
  let follow_sshtbl = follow g in
  let first_sshtbl = first g in

  let rec analyse_LL1_of_pattern (text : (symbol * string) list) (s : symbol)
      (p : pattern) : at * (symbol * string) list =
    (* print_endline "analyse_LL1_of_pattern"; *)
    if p = [ Terminal E ] then
      (Noeud ((s, ""), [ Noeud ((Terminal E, ""), []) ]), text)
    else
      let txt = ref text in
      let t =
        Noeud
          ( (s, ""),
            List.map
              (fun (s_p : symbol) ->
                let tree, txt' = analyse_LL1_of_symbol_aux !txt s_p in
                txt := txt';
                tree)
              p )
      in
      (t, !txt)
  and analyse_LL1_of_symbol_aux (text : (symbol * string) list) (s : symbol) :
      at * (symbol * string) list =
    (* print_endline ("analyse_LL1_of_symbol_aux " ^ string_of_symbol s); *)
    (*print_patterns [ List.map fst text ];
    print_newline ();*)
    if is_terminal (s, []) then
      (* print_endline "is terminal"; *)
      match text with
      | [] -> failwith "no text to match in analyse_LL1, text is empty"
      | t1 :: q ->
          if fst t1 = s then (Noeud (t1, []), q)
          else (
            print_endline
              ("Got "
              ^ string_of_symbol (fst t1)
              ^ " but expected " ^ string_of_symbol s);
            failwith "the expected terminal does not match the text")
    else
      (* print_endline "is non_terminal"; *)
      match text with
      | [] ->
          if
            (* print_endline "Terminal E"; *)
            SymbolSet.mem (Terminal E) (Hashtbl.find first_sshtbl s)
            && SymbolSet.mem (Terminal EOF) (Hashtbl.find follow_sshtbl s)
          then (
            let l =
              List.filter
                (fun p ->
                  SymbolSet.mem (Terminal E) (first_of_pattern first_sshtbl p))
                (Hashtbl.find g.rules_htbl s)
            in
            if l = [] then (
              print_string
                ("on " ^ string_of_symbol s ^ ", expected "
                ^ string_of_symbol (Terminal E)
                ^ " in first of patterns but first of patters is\n");
              print_SymbolSet (Hashtbl.find first_sshtbl s);
              failwith "error 1")
            else ();
            analyse_LL1_of_pattern text s (List.nth l 0))
          else failwith "text is epsilon but more symbols are expected"
      | _ ->
          if
            (*print_SymbolSet (Hashtbl.find first_sshtbl s);
            print_SymbolSet (Hashtbl.find follow_sshtbl s);*)
            SymbolSet.mem (Terminal E) (Hashtbl.find first_sshtbl s)
            && SymbolSet.mem
                 (fst (List.nth text 0))
                 (Hashtbl.find follow_sshtbl s)
          then (
            let l =
              List.filter
                (fun p ->
                  SymbolSet.mem (Terminal E) (first_of_pattern first_sshtbl p))
                (Hashtbl.find g.rules_htbl s)
            in
            if l = [] then (
              print_string
                ("on " ^ string_of_symbol s ^ ", expected "
                ^ string_of_symbol (Terminal E)
                ^ " in first of patterns but first of patters is\n");
              print_SymbolSet (Hashtbl.find first_sshtbl s);
              failwith "error 2")
            else ();
            analyse_LL1_of_pattern text s (List.nth l 0))
          else
            (* print_endline "cas 2";
            print_symbol (fst (List.nth text 0));
            print_newline (); *)
            let l =
              List.filter
                (fun p ->
                  SymbolSet.mem
                    (fst (List.nth text 0))
                    (first_of_pattern first_sshtbl p))
                (Hashtbl.find g.rules_htbl s)
            in
            if l = [] then (
              print_string
                ("on " ^ string_of_symbol s ^ ", expected "
                ^ string_of_symbol (fst (List.nth text 0))
                ^ " in first of patterns but first of patters is\n");
              print_SymbolSet (Hashtbl.find first_sshtbl s);
              failwith "error 3")
            else ();
            analyse_LL1_of_pattern text s (List.nth l 0)
  in
  analyse_LL1_of_symbol_aux text s

let analyse_LL1 (g : grammar) (text : (symbol * string) list) : at =
  fst (analyse_LL1_of_symbol g (text @ [ (Terminal EOF, "") ]) g.start_symbol)
