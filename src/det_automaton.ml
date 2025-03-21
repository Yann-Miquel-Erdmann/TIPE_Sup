open Regex
open Automates

let syntax_automate_det = determinise (enleve_epsilon_trans (ou_automates (List.map (fun (s, t) -> automate_gen (gen_regex s) t) [("\\*\\*", PowerOp); ("\\.not\\.", NotOp); ("\\.and\\.", AndOp); ("\\.or\\.", OrOp); ("([0-9]+\\.[0-9]*)|(\\.[0-9]+)", Rcon); ("[0-9]+", Icon); ("['](~[']|'')*[']", SconSingle); ("[\"](~[\"]|\"\")*[\"]", SconDouble); ("[A-Za-z][A-Za-z0-9_]*", Ident); ("((!~[\\n]*)?\\n[ ]*)+", EOS); ("\\.true\\.", True); ("\\.false\\.", False); ("program", Program); ("end program", EndProgram); ("end do", EndDo); ("end if", EndIf); (":", Colon); (",", Comma); ("=", Equal); ("\\*", Asterisk); ("\\(", LParenthesis); ("\\)", RParenthesis); ("integer", Integer); ("real", Real); ("double precision", Double); ("complex", Complex); ("character", Character); ("logical", Logical); ("kind", Kind); ("call", Call); ("print", Print); ("do", Do); ("while", While); ("if", If); ("else", Else); ("then", Then); ("/", Divise); ("\\+", Plus); ("-", Minus); ("(==)|(\\.eq\\.)", IsEqual); ("(/=)|(\\.ne\\.)", NotEqual); ("(<)|(\\.lt\\.)", StrictLess); ("(<=)|(\\.le\\.)", LessEqual); ("(>)|(\\.gt\\.)", StrictGreater); ("(>=)|(\\.ge\\.)", GreaterEqual); ("\\.eqv\\.", Equivalent); ("\\.neqv\\.", NotEquivalent); (" ", Space); ])))