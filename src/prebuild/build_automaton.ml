open Regex
open Automates
open Symbols

let syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [("\\*\\*", PowerOp); ("\\.(n|N)(o|O)(t|T)\\.", NotOp); ("\\.(a|A)(n|N)(d|D)\\.", AndOp); ("\\.(o|O)(r|R)\\.", OrOp); ("(([0-9]+\\.[0-9]*)|(\\.[0-9]+))(d(\\+|-)?[0-9]+)", Dcon); ("(([0-9]+\\.[0-9]*)|(\\.[0-9]+))(e(\\+|-)?[0-9]+)?", Rcon); ("[0-9]+(e(\\+|-)?[0-9]+)?", Icon); ("['](~[']|'')*[']", SconSingle); ("[\"](~[\"]|\"\")*[\"]", SconDouble); ("[A-Za-z][A-Za-z0-9_]*", Ident); ("((!~[\\n]*)?\\n[ ]*)+", EOS); ("\\.(t|T)(r|R)(u|U)(e|E)\\.", True); ("\\.(f|F)(a|A)(l|L)(s|S)(e|E)\\.", False); ("(p|P)(r|R)(o|O)(g|G)(r|R)(a|A)(m|M)", Program); ("(f|F)(u|U)(n|N)(c|C)(t|T)(i|I)(o|O)(n|N)", Function); ("(s|S)(u|U)(b|B)(r|R)(o|O)(u|U)(t|T)(i|I)(n|N)(e|E)", Subroutine); ("(e|E)(n|N)(d|D) (p|P)(r|R)(o|O)(g|G)(r|R)(a|A)(m|M)", EndProgram); ("(e|E)(n|N)(d|D) (f|F)(u|U)(n|N)(c|C)(t|T)(i|I)(o|O)(n|N)", EndFunction); ("(e|E)(n|N)(d|D) (s|S)(u|U)(b|B)(r|R)(o|O)(u|U)(t|T)(i|I)(n|N)(e|E)", EndSubroutine); ("(e|E)(n|N)(d|D) (d|D)(o|O)", EndDo); ("(e|E)(n|N)(d|D) (i|I)(f|F)", EndIf); (":", Colon); (",", Comma); ("=", Equal); ("\\*", Asterisk); ("\\(", LParenthesis); ("\\)", RParenthesis); ("(i|I)(n|N)(t|T)(e|E)(g|G)(e|E)(r|R)", Integer); ("(r|R)(e|E)(a|A)(l|L)", Real); ("(d|D)(o|O)(u|U)(b|B)(l|L)(e|E) (p|P)(r|R)(e|E)(c|C)(i|I)(s|S)(i|I)(o|O)(n|N)", Double); ("(c|C)(o|O)(m|M)(p|P)(l|L)(e|E)(x|X)", Complex); ("(c|C)(h|H)(a|A)(r|R)(a|A)(c|C)(t|T)(e|E)(r|R)", Character); ("(l|L)(o|O)(g|G)(i|I)(c|C)(a|A)(l|L)", Logical); ("(p|P)(a|A)(r|R)(a|A)(m|M)(e|E)(t|T)(e|E)(r|R)", Parameter); ("(i|I)(n|N)(t|T)(e|E)(n|N)(t|T)", Intent); ("(i|I)(n|N)", In); ("(o|O)(u|U)(t|T)", Out); ("(i|I)(n|N)(o|O)(u|U)(t|T)", InOut); ("(c|C)(a|A)(l|L)(l|L)", Call); ("(p|P)(r|R)(i|I)(n|N)(t|T)", Print); ("(d|D)(o|O)", Do); ("(w|W)(h|H)(i|I)(l|L)(e|E)", While); ("(i|I)(f|F)", If); ("(e|E)(l|L)(s|S)(e|E)", Else); ("(t|T)(h|H)(e|E)(n|N)", Then); ("/", Divise); ("\\+", Plus); ("-", Minus); ("(==)|(\\.(e|E)(q|Q)\\.)", IsEqual); ("(/=)|(\\.(n|N)(e|E)\\.)", NotEqual); ("(<)|(\\.(l|L)(t|T)\\.)", StrictLess); ("(<=)|(\\.(l|L)(e|E)\\.)", LessEqual); ("(>)|(\\.(g|G)(t|T)\\.)", StrictGreater); ("(>=)|(\\.(g|G)(e|E)\\.)", GreaterEqual); ("\\.(e|E)(q|Q)(v|V)\\.", Equivalent); ("\\.(n|N)(e|E)(q|Q)(v|V)\\.", NotEquivalent); (" ", Space); ("(r|R)(e|E)(c|C)(u|U)(r|R)(s|S)(i|I)(v|V)(e|E)", Recursive); ])))

let () =
	print_string "generating prebuild automaton..."; print_newline();
	let file = open_out "src/det_automaton.ml" in
	output_string file "open Automates\n\nlet syntax_automate_det = {\n\tnodes = [";
	output_string file (String.concat "; " (List.map string_of_int syntax_automate_det.nodes));
	output_string file ("];\n\tdebut = "^ string_of_int syntax_automate_det.debut ^";\n\tfin = [|");
	output_string file (String.concat "; " (List.map (fun x -> match x with | None -> "None" | Some e -> "Some "^string_of_terminal e) (Array.to_list syntax_automate_det.fin))); output_string file "|];\n\ttransitions = [|\n\t\t";
	output_string file (String.concat ";\n\t\t" (Array.to_list (Array.map (fun sub_arr -> "[|"^(String.concat "; " (Array.to_list (Array.map string_of_int sub_arr)))^"|]") syntax_automate_det.transitions)));
	output_string file "\n\t|]\n}";
	flush file;
	close_out file;
	print_string "automaton pregenerating done"; print_newline()