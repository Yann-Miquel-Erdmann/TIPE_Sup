type derivation = string list
type rule = string*(derivation list)
type grammar = rule list 

(** Returns the list of all the lines in the file named [file_name]. *)
let read_file (file_name: string) : string list = 
  let rec lire file liste = 
    let line = input_line file in
      ();
    try lire file (line::liste) with End_of_file->
      close_in file;
      line::liste
  in List.rev (lire (open_in file_name) [])



(* returns the name of the rule and the remaining chars on the line *)
let get_rule_name (s: char list): string*(char list) = 
  let rec get_rule_name_aux (s: char list) (revname: char list): string*(char list) = 
    match s with
    | [] -> failwith "no rule name one the line"
    | ' '::q -> String.of_seq (List.to_seq (List.rev revname)),q
    | c::q -> get_rule_name_aux q (c::revname)
  in
  get_rule_name_aux s []


(* removes \s*->\s* from the string*)
let rec remove_arrow (s: char list): char list = 
  match s with
  | [] -> failwith "there is no arrow in the rule"
  | ' '::q -> remove_arrow q 
  | '-'::'>'::q -> remove_arrow q
  | _ -> s




(* splits the derivation into a list of list of strings   *)
let get_derivations (s: char list): derivation list =
  let alphanumerical_maj (c:char) = 41 <= Char.code c && Char.code c <= 90 in    
  let alphanumerical_min (c:char) = 97 <= Char.code c && Char.code c <= 122 in 
  
  (* splits one derivation on [a-z]\s[A-Z]  *)
  let split_derivation (s: char list): derivation = 
    let rec split_derivation_aux (s: char list) (rev_rule_name: char list): derivation = 
      match s with
      | [] -> []
      | c1::' '::c2::q when (alphanumerical_maj c1) && ( alphanumerical_min c2) -> (String.of_seq (List.to_seq (List.rev rev_rule_name))) :: split_derivation_aux q []  
      | c:: q -> split_derivation_aux q (c::rev_rule_name) 
    in split_derivation_aux s []
  in
  (* splits the derivations on \s|\s  *)
  let rec split_derivations (s: char list) (rev_derivation: char list): derivation list = 
    match s with
    | [] -> []
    | ' '::'|'::' ' :: q -> (split_derivation (List.rev rev_derivation) )::(split_derivations q []) 
    | c:: q -> split_derivations q (c::rev_derivation) 
  in
  split_derivations s []  



(** Returns all the token names in [s] if they respect the regex rule [[A-Za-z0-9]]. Then it is stored in [out].
    [in_string] helps knowing if it is in string to not match there. *)
let get_rule (line : string): rule =
  let s = List.init (String.length line) (String.get line) in
  let rule_name,s1 = get_rule_name s in
  let derivations = get_derivations (remove_arrow s1) in
  rule_name,derivations

let get_grammar (file_name: string): grammar = 
  let lines = read_file file_name in
  List.map get_rule lines


