open GenerateGrammar

let repr_of_terminal ((_, p) : rule) : string = List.nth (List.nth p 0) 0

(** enlève les guillemets simples de l'expresion terminale [r] *)
let printable_repr_of_terminal (r : rule) : string =
  let s = repr_of_terminal r in
  if s.[0] = '\'' then String.sub s 1 (String.length s - 2)
  else String.sub s 2 (String.length s - 3)

(* array qui permet de convertir rapidement une lettre en la disjonction entre la minnuscule et la majuscule *)
let case_arr =
  Array.init 26 (fun i ->
      "("
      ^ String.make 1 (char_of_int (97 + i))
      ^ "|"
      ^ String.make 1 (char_of_int (65 + i))
      ^ ")")

(** convertit la chaîne [s] en une liste de caractères *)
let char_list_of_string (s : string) : char list = List.of_seq (String.to_seq s)

(** récupère le symbole safe de la liste de règles [term], le seul qui peut être
    remplacé par un autre *)
let get_safe_symbol (term : rule list) : string =
  fst
    (List.nth
       (List.filter (fun (r : rule) -> (repr_of_terminal r).[0] = 's') term)
       0)

(** récupère la liste des symboles non donnés à LL1 dans la liste [term] *)
let get_unparsed_symbol (term : rule list) : string list =
  List.map fst
    (List.filter (fun (r : rule) -> (repr_of_terminal r).[0] = '_') term)

(** transforme la chaîne [s] pour ne plus être sensible à la casse lorsque
    transformée en expression régulière *)
let make_non_case_sentive (s : string) : string =
  List.fold_left
    (fun acc x ->
      if alphanumerical_min x then acc ^ case_arr.(int_of_char x - 97)
      else acc ^ String.make 1 x)
    "" (char_list_of_string s)

(** crée le fichier [f_name] qui va créer l'automate pour une utilisation
    ultérieure dans [writing_file_name] à partir de la grammaire [g] *)
let generate_file_lexer (g : grammar) (f_name : string)
    (writing_file_name : string) : unit =
  let file = open_out f_name in
  let t = terminals g in
  print_endline ("Generating the " ^ f_name ^ " file...");

  (* création de l'automate *)
  output_string file "open Regex\nopen Automates\nopen Symbols\n";
  output_string file
    "\n\
     let syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates \
     (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [";
  List.iter
    (fun (s, p) ->
      output_string file
        ("(\""
        ^ String.escaped
            (let s = printable_repr_of_terminal (s, p) in
             if (repr_of_terminal (s, p)).[0] = '\'' then
               make_non_case_sentive s
             else s)
        ^ "\", " ^ s ^ "); "))
    t;
  output_string file "])))\n\n";

  (* écriture de l'automate prégénéré *)
  output_string file
    ("let () =\n\
      print_string \"generating prebuild automaton...\"; print_newline();\n\
      let file = open_out \"" ^ writing_file_name
   ^ "\" in\n\
      output_string file \"open Automates\\n\\nlet syntax_automate_det = \
      {\\n    nodes = [\";\n\
      output_string file (String.concat \"; \" (List.map string_of_int \
      syntax_automate_det.nodes));\n\
      output_string file (\"];\\n    debut = \"^ string_of_int \
      syntax_automate_det.debut ^\";\\n    fin = [|\");\n\
      output_string file (String.concat \"; \" (List.map (fun x -> match x \
      with | None -> \"None\" | Some e -> \"Some \"^string_of_terminal e) \
      (Array.to_list syntax_automate_det.fin))); output_string file \
      \"|];\\n    transitions = [|\\n        \";\n\
      output_string file (String.concat \";\\n        \" (Array.to_list \
      (Array.map (fun sub_arr -> \"[|\"^(String.concat \"; \" (Array.to_list \
      (Array.map string_of_int sub_arr)))^\"|]\") \
      syntax_automate_det.transitions)));\n\
      output_string file \"\\n    |]\\n}\";\n\
      flush file;\n\
      close_out file;\n\
      print_string \"automaton pregenerating done\"; print_newline()");

  flush file;
  close_out file

(** crée le fichier [f_name] qui va contenir l'ensemble des terminaux et non
    terminaux de la grammaire [g] *)
let generate_file_symbols (g : grammar) (f_name : string) : unit =
  let t =
    ("EOF", [ [ "'End of file'" ] ])
    :: ("E", [ [ "'Epsilon'" ] ]) :: terminals g
  in
  let nt = non_terminals g in

  print_endline ("Generating the " ^ f_name ^ " file...");
  let output_file = open_out f_name in

  (* ajout des terminaux *)
  output_string output_file "type terminal =\n";
  List.iter
    (fun (symbol, _) -> output_string output_file ("  | " ^ symbol ^ "\n"))
    t;
  (* ajout des non terminaux*)
  output_string output_file "\ntype non_terminal =\n";
  List.iter
    (fun (symbol, _) -> output_string output_file ("  | " ^ symbol ^ "\n"))
    nt;

  output_string output_file
    "type symbol = | Terminal of terminal | NonTerminal of non_terminal";
  (* ajoute le symbole safe *)
  output_string output_file ("\nlet safe_token = " ^ get_safe_symbol t ^ "\n");
  (* ajoute les tokens non utilisés par LL1 *)
  output_string output_file "let unparsed_tokens = [";
  List.iter
    (fun x -> output_string output_file (x ^ "; "))
    (get_unparsed_symbol t);
  output_string output_file "]\n\n";

  (* Fonctions utiles :*)
  (* représentation en expression régulière des terminaux *)
  output_string output_file
    "let repr_of_terminal (t : terminal) : string =\n  match t with\n";
  List.iter
    (fun (s, p) ->
      output_string output_file
        ("  | " ^ s ^ " -> \""
        ^ String.escaped (printable_repr_of_terminal (s, p))
        ^ "\"\n"))
    t;
  output_string output_file "\n";

  (* représentation en chaîne des terminaux *)
  output_string output_file
    "let string_of_terminal (t : terminal) : string =\n    match t with\n";
  List.iter
    (fun (s, _) ->
      output_string output_file ("    | " ^ s ^ " -> \"" ^ s ^ "\"\n"))
    t;
  output_string output_file "\n";

  (* représentation en chaîne des non terminaux *)
  output_string output_file
    "let string_of_non_terminal (nt : non_terminal) : string =\n\
    \  match nt with\n";
  List.iter
    (fun (s, _) ->
      output_string output_file ("  | " ^ s ^ " -> \"" ^ s ^ "\"\n"))
    nt;

  flush output_file;
  close_out output_file

let generate_file_grammar (g : grammar) (f_name : string) : unit =
  let nt = non_terminals g in
  let non_terminals_set = StringSet.of_list (List.map fst nt) in
  let type_string_of_symbol (s : string) : string =
    if StringSet.mem s non_terminals_set then "NonTerminal" else "Terminal"
  in

  print_endline ("Generating the " ^ f_name ^ " file...");
  let output_file = open_out f_name in
  output_string output_file "open GrammarFunctions\nopen Symbols\n";

  output_string output_file "let grammar = { ";
  output_string output_file
    ("start_symbol = NonTerminal "
    ^ fst (List.nth nt 0)
    ^ ";\nrules_htbl = Hashtbl.of_seq (List.to_seq [");
  List.iter
    (fun ((s, p) : rule) ->
      output_string output_file ("(" ^ type_string_of_symbol s ^ " " ^ s ^ ",[");
      List.iter
        (fun pattern ->
          output_string output_file "[";
          List.iter
            (fun s ->
              output_string output_file (type_string_of_symbol s ^ " " ^ s ^ ";"))
            pattern;
          output_string output_file "];")
        p;
      output_string output_file "]);\n")
    nt;
  output_string output_file "])}";

  flush output_file;
  close_out output_file

let generate_files (g_path : string) : unit =
  let g = get_grammar g_path in
  Printf.printf "Grammar length %d\n" (List.length g);
  generate_file_symbols g "symbols.ml";
  generate_file_lexer g "prebuild/buildAutomaton.ml" "src/detAutomaton.ml";
  generate_file_grammar g "grammar.ml"

let _ =
  Sys.chdir "src/";
  generate_files "../grammar/Our_Grammar.txt";
  print_endline "Finished generating, exiting."
