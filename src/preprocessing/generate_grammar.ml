type pattern = string list
type rule = string * pattern list
type grammar = rule list

module StringSet = Set.Make(String)

(** teste si le caractère [c] est une majuscule *)
let alphanumerical_maj (c:char) : bool = 65 <= Char.code c && Char.code c <= 90  
(** teste si le caractère [c] est une minuscule *)
let alphanumerical_min (c:char) : bool = 97 <= Char.code c && Char.code c <= 122  

(** renvoie la liste de toutes les lignes du fichier [file_name] *)
let read_file (file_name: string) : string list = 
  let rec lire file liste = 
    let line = input_line file in
    ();
    try lire file (line :: liste)
    with End_of_file ->
      close_in file;
      line::liste
  in List.rev (lire (open_in file_name) [])

(** renvoie le nom de toutes les règles et le reste des caractères dans la ligne [s] *)
let get_rule_name (s: char list): string * (char list) = 
  let rec get_rule_name_aux (s: char list) (revname: char list): string*(char list) = 
    match s with
    | [] -> failwith "no rule name one the line"
    | ' ' :: q -> (String.of_seq (List.to_seq (List.rev revname)), q)
    | c :: q -> get_rule_name_aux q (c :: revname)
  in
  get_rule_name_aux s []

(** enlèves les chaînes de la forme '\s*->\s*' dans la chaîne [s] *)
let rec remove_arrow (s: char list): char list = 
  match s with
  | [] -> failwith "there is no arrow in the rule"
  | ' ' :: q -> remove_arrow q
  | '-' :: '>' :: q -> remove_arrow q
  | _ -> s


(** sépare l'expression [s] en une liste de motifs *)
let get_derivations (s: char list): pattern list =
  (** sépare les différents non terminaux de [s] sur le motif '\[a-z] \[A-Z]' *)
  let split_derivation (s: char list): pattern = 
      (** sépare les différents non terminaux de [s] sur le motif '\[a-z] \[A-Z]' et les ajoute à [rev_rule_name] *)
    let rec split_derivation_aux (s: char list) (rev_rule_name: char list): pattern = 
      match s with
      | [] ->
          [ String.trim (String.of_seq (List.to_seq (List.rev rev_rule_name))) ]
      | c1 :: ' ' :: c2 :: q when alphanumerical_min c1 && alphanumerical_maj c2
        ->
          String.trim
            (String.of_seq (List.to_seq (List.rev (c1 :: rev_rule_name))))
          :: split_derivation_aux (c2 :: q) []
      | c :: q -> split_derivation_aux q (c :: rev_rule_name)
    in
    split_derivation_aux s []
  in

  (** sépare les disjonctions de non terminaux de [s] sur le motif ' | ' et les ajoute à [rev_derivation] *)
  let rec split_derivations (s: char list) (rev_derivation: char list): pattern list = 
    match s with
    | [] -> [ split_derivation (List.rev rev_derivation) ]
    | ' ' :: '|' :: ' ' :: q ->
        split_derivation (List.rev rev_derivation) :: split_derivations q []
    | c :: q -> split_derivations q (c :: rev_derivation)
  in

  split_derivations s []  

(** teste si la ligne [l] est vide ou est commentée par '//' *)
let rule_filter (l: string): bool = 
  if String.length (String.trim l) < 2 then false else l.[0] <> '/' && l.[1] <> '/'

  let get_rule (line : string): rule =
    let s = List.init (String.length line) (String.get line)
  in
  
  let rule_name,s1 = get_rule_name s in
  let derivations = get_derivations (remove_arrow s1) in
  (rule_name, derivations)

(** réfupère la grammaire contenue dans [file_name] *)
let get_grammar (file_name: string): grammar = 
  let lines = read_file file_name in
  let not_empty_lines = List.filter rule_filter lines in
  List.map get_rule not_empty_lines

(** teste si la règle [r] est terminale *)
let is_terminal (r: rule): bool =
  match r with 
  | (_,[[s]]) -> not (alphanumerical_maj (s.[0])) 
  | _ -> false 

(** récupère les terminaux de la grammaire [g] *)
let terminals (g: grammar): rule list = List.filter is_terminal g
(** récupère les non terminaux de la grammaire [g] *)
let non_terminals (g: grammar): rule list = List.filter (fun (r:rule) -> not (is_terminal r) ) g
