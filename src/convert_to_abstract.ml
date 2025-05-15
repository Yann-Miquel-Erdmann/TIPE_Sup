open Abstract_tokens
open LL1
open Symbols

(** écrase les noeuds à écraser à partir de [t] et l'ajoute à ceux de la liste
    [l] (pour remonter des arguments par exemple) *)
let flatten (t : ast) (l : ast list) : ast list =
  let ended = ref false in
  let rec aux (t : ast) (out : ast list) : ast list =
    match t with
    | Noeud (ToFlatten, []) ->
        if !ended then out
        else (
          ended := true;
          aux (Noeud (ToFlatten, l)) out)
    | Noeud (ToFlatten, x :: q) ->
        let out = aux x out in
        aux (Noeud (ToFlatten, q)) out
    | _ ->
        if !ended then t :: out
        else (
          ended := true;
          aux (Noeud (ToFlatten, l)) (t :: out))
  in
  List.rev (aux t [])

(** Si [t] est un commentaire, le convertit pour séparer les différentes lignes,
    sinon renvoie une erreur *)
let convert_comments (t : ast) : ast list =
  let nb = ref 0 in
  match t with
  | Noeud (Commentaire s, []) ->
      let l = List.map String.trim (String.split_on_char '\n' s) in
      let l2 =
        List.map
          (fun (s : string) : ast ->
            if s = "" then Noeud (NewLine, [])
            else (
              incr nb;
              Noeud (Commentaire (String.sub s 1 (String.length s - 1)), [])))
          l
      in
      if !nb = 0 then match l2 with [] -> l2 | _ :: q -> q else l2
  | _ -> failwith "l'argument donné n'est pas un commentaire"

(** Enlève le niveau d'abstraction de l'arbre de syntaxe [t] créé avec LL1 *)
let rec convert_to_abstract (t : at) : ast =
  match t with
  | Noeud ((NonTerminal ExecutableProgram, s0), l) -> (
      match l with
      | [
       Noeud ((Terminal EOS, s1), []); Noeud ((NonTerminal ProgramUnit, s), l);
      ] ->
          Noeud
            ( ProgramRoot,
              [
                convert_to_abstract (Noeud ((Terminal EOS, s1), []));
                convert_to_abstract (Noeud ((NonTerminal ProgramUnit, s), l));
              ] )
      | [ Noeud ((NonTerminal ProgramUnit, s), l) ] ->
          Noeud
            ( ProgramRoot,
              [ convert_to_abstract (Noeud ((NonTerminal ProgramUnit, s), l)) ]
            )
      | _ -> failwith "")
  | Noeud
      ((NonTerminal ProgramUnit, _), [ Noeud ((NonTerminal MainProgram, s), l) ])
    ->
      convert_to_abstract (Noeud ((NonTerminal MainProgram, s), l))
  | Noeud
      ( (NonTerminal MainProgram, _),
        [
          Noeud ((NonTerminal ProgramStmt, s), l);
          Noeud ((NonTerminal MainRange, s1), l2);
        ] ) ->
      Noeud
        ( Syntax Program,
          flatten
            (Noeud
               ( ToFlatten,
                 [
                   convert_to_abstract (Noeud ((NonTerminal ProgramStmt, s), l));
                   convert_to_abstract (Noeud ((NonTerminal MainRange, s1), l2));
                 ] ))
            [] )
  | Noeud
      ( (NonTerminal ProgramStmt, _),
        [
          Noeud ((Terminal Program, _), []);
          Noeud ((NonTerminal ProgramName, s), l);
          Noeud ((Terminal EOS, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal ProgramName, s), l));
            convert_to_abstract (Noeud ((Terminal EOS, s1), l1));
          ] )
  | Noeud
      ( (NonTerminal MainRange, _),
        [ Noeud ((NonTerminal EndProgramStmt, s), l) ] ) ->
      convert_to_abstract (Noeud ((NonTerminal EndProgramStmt, s), l))
  | Noeud
      ( (NonTerminal MainRange, _),
        [
          Noeud ((NonTerminal BodyConstruct, s), l);
          Noeud
            ( (NonTerminal BodyConstruct_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal EndProgramStmt, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal BodyConstruct, s), l));
            convert_to_abstract (Noeud ((NonTerminal EndProgramStmt, s1), l1));
          ] )
  | Noeud
      ( (NonTerminal MainRange, _),
        [
          Noeud ((NonTerminal BodyConstruct, s), l);
          Noeud ((NonTerminal BodyConstruct_star, s2), l2);
          Noeud ((NonTerminal EndProgramStmt, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal BodyConstruct, s), l));
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract
                    (Noeud ((NonTerminal BodyConstruct_star, s2), l2));
                ] );
            convert_to_abstract (Noeud ((NonTerminal EndProgramStmt, s1), l1));
          ] )
  | Noeud
      ( (NonTerminal BodyConstruct_star, _),
        [
          Noeud ((NonTerminal BodyConstruct, s), l);
          Noeud
            ( (NonTerminal BodyConstruct_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      Noeud
        ( ToFlatten,
          [ convert_to_abstract (Noeud ((NonTerminal BodyConstruct, s), l)) ] )
  | Noeud
      ( (NonTerminal BodyConstruct_star, _),
        [
          Noeud ((NonTerminal BodyConstruct, s), l);
          Noeud ((NonTerminal BodyConstruct_star, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal BodyConstruct, s), l));
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract
                    (Noeud ((NonTerminal BodyConstruct_star, s1), l1));
                ] );
          ] )
  | Noeud
      ( (NonTerminal EndProgramStmt, _),
        [
          Noeud ((Terminal EndProgram, _), []);
          Noeud ((NonTerminal EndName_opt, _), _);
          Noeud ((Terminal EOS, s), []);
        ] ) ->
      convert_to_abstract (Noeud ((Terminal EOS, s), []))
  | Noeud
      ( (NonTerminal BodyConstruct, _),
        [ Noeud ((NonTerminal SpecificationPartConstruct, s), l) ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract
              (Noeud ((NonTerminal SpecificationPartConstruct, s), l));
          ] )
  | Noeud
      ( (NonTerminal BodyConstruct, _),
        [ Noeud ((NonTerminal ExecutableConstruct, s), l) ] ) ->
      convert_to_abstract (Noeud ((NonTerminal ExecutableConstruct, s), l))
  | Noeud
      ( (NonTerminal SpecificationPartConstruct, _),
        [ Noeud ((NonTerminal DeclarationConstruct, s), l) ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract
              (Noeud ((NonTerminal DeclarationConstruct, s), l));
          ] )
  | Noeud
      ( (NonTerminal DeclarationConstruct, _),
        [ Noeud ((NonTerminal TypeDeclarationStmt, s), l) ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract
              (Noeud ((NonTerminal TypeDeclarationStmt, s), l));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Double, _), []) ]);
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Double_precision,
                [
                  convert_to_abstract
                    (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                ] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Integer, _), []);
                Noeud
                  ( (NonTerminal KindSelector_opt, _),
                    [ Noeud ((Terminal E, _), []) ] );
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Integer,
                flatten
                  (convert_to_abstract
                     (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Character, _), []) ]);
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Character,
                flatten
                  (convert_to_abstract
                     (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Character, _), []) ]);
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [
                Noeud ((Terminal Comma, _), []);
                Noeud ((NonTerminal AttrSpec, s1), l1);
                Noeud
                  ( (NonTerminal Comma_AttrSpec_star, _),
                    [ Noeud ((Terminal E, _), []) ] );
              ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Character,
                flatten
                  (Noeud
                     ( ToFlatten,
                       convert_to_abstract
                         (Noeud ((NonTerminal AttrSpec, s1), l1))
                       :: flatten
                            (convert_to_abstract
                               (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                            [] ))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Complex, _), []);
                Noeud
                  ( (NonTerminal KindSelector_opt, _),
                    [ Noeud ((Terminal E, _), []) ] );
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Complex,
                flatten
                  (convert_to_abstract
                     (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Logical, _), []);
                Noeud
                  ( (NonTerminal KindSelector_opt, _),
                    [ Noeud ((Terminal E, _), []) ] );
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Logical,
                flatten
                  (convert_to_abstract
                     (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Real, _), []);
                Noeud
                  ( (NonTerminal KindSelector_opt, _),
                    [ Noeud ((Terminal E, _), []) ] );
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Real,
                flatten
                  (convert_to_abstract
                     (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Integer, _), []);
                Noeud ((NonTerminal KindSelector_opt, s1), l1);
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Integer,
                Noeud
                  ( Syntax Size,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal KindSelector_opt, s1), l1));
                    ] )
                :: flatten
                     (convert_to_abstract
                        (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                     [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Complex, _), []);
                Noeud ((NonTerminal KindSelector_opt, s1), l1);
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Complex,
                Noeud
                  ( Syntax Size,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal KindSelector_opt, s1), l1));
                    ] )
                :: flatten
                     (convert_to_abstract
                        (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                     [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Logical, _), []);
                Noeud ((NonTerminal KindSelector_opt, s1), l1);
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Logical,
                Noeud
                  ( Syntax Size,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal KindSelector_opt, s1), l1));
                    ] )
                :: flatten
                     (convert_to_abstract
                        (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                     [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal TypeDeclarationStmt, _),
        [
          Noeud
            ( (NonTerminal TypeSpec, _),
              [
                Noeud ((Terminal Real, _), []);
                Noeud ((NonTerminal KindSelector_opt, s1), l1);
              ] );
          Noeud
            ( (NonTerminal Comma_AttrSpec_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal TypeDecl_Assignment, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Real,
                Noeud
                  ( Syntax Size,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal KindSelector_opt, s1), l1));
                    ] )
                :: flatten
                     (convert_to_abstract
                        (Noeud ((NonTerminal TypeDecl_Assignment, s), l)))
                     [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud ((NonTerminal AttrSpec, _), [ Noeud ((Terminal Parameter, _), []) ])
    ->
      Noeud (Syntax Constant, [])
  | Noeud
      ( (NonTerminal Comma_EntityDecl_star, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal EntityDecl, s), l);
          Noeud
            ( (NonTerminal Comma_EntityDecl_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal TypeDecl_Assignment, _),
        [
          Noeud ((Terminal Colon, _), []);
          Noeud ((Terminal Colon, _), []);
          Noeud ((NonTerminal EntityDecl, s), l);
          Noeud
            ( (NonTerminal Comma_EntityDecl_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal EntityDecl, s), l))
  | Noeud
      ( (NonTerminal Comma_EntityDecl_star, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal EntityDecl, s), l);
          Noeud ((NonTerminal Comma_EntityDecl_star, s2), l2);
        ] )
  | Noeud
      ( (NonTerminal TypeDecl_Assignment, _),
        [
          Noeud ((Terminal Colon, _), []);
          Noeud ((Terminal Colon, _), []);
          Noeud ((NonTerminal EntityDecl, s), l);
          Noeud ((NonTerminal Comma_EntityDecl_star, s2), l2);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal EntityDecl, s), l));
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract
                    (Noeud ((NonTerminal Comma_EntityDecl_star, s2), l2));
                ] );
          ] )
  | Noeud
      ( (NonTerminal Comma_ObjectName_star, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal ObjectName, s), l);
          Noeud
            ( (NonTerminal Comma_ObjectName_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal TypeDecl_Assignment, _),
        [
          Noeud ((NonTerminal ObjectName, s), l);
          Noeud
            ( (NonTerminal Comma_ObjectName_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal ObjectName, s), l))
  | Noeud
      ( (NonTerminal Comma_ObjectName_star, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal ObjectName, s), l);
          Noeud ((NonTerminal Comma_ObjectName_star, s1), l1);
        ] )
  | Noeud
      ( (NonTerminal TypeDecl_Assignment, _),
        [
          Noeud ((NonTerminal ObjectName, s), l);
          Noeud ((NonTerminal Comma_ObjectName_star, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal ObjectName, s), l));
            convert_to_abstract
              (Noeud ((NonTerminal Comma_ObjectName_star, s1), l1));
          ] )
  | Noeud
      ( (NonTerminal EntityDecl, _),
        [
          Noeud ((NonTerminal ObjectName, s), l);
          Noeud
            ( (NonTerminal Asterisk_CharLength_opt, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud
            ((NonTerminal Equal_Expr_opt, _), [ Noeud ((Terminal E, _), []) ]);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal ObjectName, s), l))
  | Noeud
      ( (NonTerminal EntityDecl, _),
        [
          Noeud
            ((NonTerminal ObjectName, _), [ Noeud ((Terminal Ident, s), []) ]);
          Noeud
            ( (NonTerminal Asterisk_CharLength_opt, _),
              [
                Noeud ((Terminal Asterisk, _), []);
                Noeud ((NonTerminal CharLength, s1), l1);
              ] );
          Noeud
            ((NonTerminal Equal_Expr_opt, _), [ Noeud ((Terminal E, _), []) ]);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Size,
                [
                  convert_to_abstract (Noeud ((NonTerminal CharLength, s1), l1));
                ] );
            Noeud (Name s, []);
          ] )
  | Noeud
      ( (NonTerminal EntityDecl, _),
        [
          Noeud
            ((NonTerminal ObjectName, _), [ Noeud ((Terminal Ident, s), []) ]);
          Noeud
            ( (NonTerminal Asterisk_CharLength_opt, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud
            ( (NonTerminal Equal_Expr_opt, _),
              [
                Noeud ((Terminal Equal, _), []);
                Noeud ((NonTerminal Expr, s2), l2);
              ] );
        ] ) ->
      Noeud
        ( Operateur Assignation,
          [
            Noeud (Name s, []);
            convert_to_abstract (Noeud ((NonTerminal Expr, s2), l2));
          ] )
  | Noeud
      ( (NonTerminal EntityDecl, _),
        [
          Noeud
            ((NonTerminal ObjectName, _), [ Noeud ((Terminal Ident, s), []) ]);
          Noeud
            ( (NonTerminal Asterisk_CharLength_opt, _),
              [
                Noeud ((Terminal Asterisk, _), []);
                Noeud ((NonTerminal CharLength, s1), l1);
              ] );
          Noeud
            ( (NonTerminal Equal_Expr_opt, _),
              [
                Noeud ((Terminal Equal, _), []);
                Noeud ((NonTerminal Expr, s2), l2);
              ] );
        ] ) ->
      Noeud
        ( Operateur Assignation,
          [
            Noeud
              ( Syntax Size,
                [
                  convert_to_abstract (Noeud ((NonTerminal CharLength, s1), l1));
                ] );
            Noeud (Name s, []);
            convert_to_abstract (Noeud ((NonTerminal Expr, s2), l2));
          ] )
  | Noeud
      ( (NonTerminal CharLength, _),
        [
          Noeud ((Terminal LParenthesis, _), []);
          Noeud ((NonTerminal TypeParamValue, s), l);
          Noeud ((Terminal RParenthesis, _), []);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal TypeParamValue, s), l))
  | Noeud
      ( (NonTerminal CharLength, _),
        [ Noeud ((NonTerminal ScalarIntLiteralConstant, s), l) ] ) ->
      convert_to_abstract (Noeud ((NonTerminal ScalarIntLiteralConstant, s), l))
  | Noeud
      ( (NonTerminal TypeParamValue, _),
        [ Noeud ((NonTerminal Expr_Or_Asterisk, s), l) ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Expr_Or_Asterisk, s), l))
  | Noeud
      ((NonTerminal Expr_Or_Asterisk, _), [ Noeud ((Terminal Asterisk, _), []) ])
    ->
      Noeud (Syntax Any, [])
  | Noeud
      ((NonTerminal Expr_Or_Asterisk, _), [ Noeud ((NonTerminal Expr, s), l) ])
    ->
      convert_to_abstract (Noeud ((NonTerminal Expr, s), l))
  | Noeud
      ( (NonTerminal KindSelector_opt, _),
        [
          Noeud ((Terminal LParenthesis, _), []);
          Noeud ((NonTerminal Expr, s), l);
          Noeud ((Terminal RParenthesis, _), []);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Expr, s), l))
  | Noeud ((NonTerminal ExecutableConstruct, _), l) -> (
      match l with
      | [ Noeud ((NonTerminal ActionStmt, s1), l1) ] ->
          Noeud
            ( ToFlatten,
              [ convert_to_abstract (Noeud ((NonTerminal ActionStmt, s1), l1)) ]
            )
      | [ Noeud ((NonTerminal DoConstruct, s1), l1) ] ->
          Noeud
            ( ToFlatten,
              [
                convert_to_abstract (Noeud ((NonTerminal DoConstruct, s1), l1));
              ] )
      | [ Noeud ((NonTerminal IfConstruct, s1), l1) ] ->
          Noeud
            ( ToFlatten,
              [
                convert_to_abstract (Noeud ((NonTerminal IfConstruct, s1), l1));
              ] )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal ActionStmt, _),
        [ Noeud ((NonTerminal AssignmentStmt, s), l) ] ) ->
      Noeud
        ( ToFlatten,
          [ convert_to_abstract (Noeud ((NonTerminal AssignmentStmt, s), l)) ]
        )
  | Noeud
      ((NonTerminal ActionStmt, _), [ Noeud ((NonTerminal PrintStmt, s), l) ])
    ->
      Noeud
        ( ToFlatten,
          [ convert_to_abstract (Noeud ((NonTerminal PrintStmt, s), l)) ] )
  | Noeud
      ( (NonTerminal AssignmentStmt, _),
        [
          Noeud ((NonTerminal Name, s), l);
          Noeud ((Terminal Equal, _), []);
          Noeud ((NonTerminal Expr, s1), l1);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Operateur Assignation,
                [
                  convert_to_abstract (Noeud ((NonTerminal Name, s), l));
                  convert_to_abstract (Noeud ((NonTerminal Expr, s1), l1));
                ] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal PrintStmt, _),
        [
          Noeud ((Terminal Print, _), []);
          Noeud ((NonTerminal FormatIdentifier, _), _);
          Noeud
            ( (NonTerminal Comma_OutputItemList_opt, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud (Syntax Print, [ Noeud (Chaine "", []) ]);
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal PrintStmt, _),
        [
          Noeud ((Terminal Print, _), []);
          Noeud ((NonTerminal FormatIdentifier, _), _);
          Noeud ((NonTerminal Comma_OutputItemList_opt, s), l);
          Noeud ((Terminal EOS, s2), []);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            Noeud
              ( Syntax Print,
                flatten
                  (convert_to_abstract
                     (Noeud ((NonTerminal Comma_OutputItemList_opt, s), l)))
                  [] );
            convert_to_abstract (Noeud ((Terminal EOS, s2), []));
          ] )
  | Noeud
      ( (NonTerminal Comma_OutputItemList_opt, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal OutputItemList, s), l);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal OutputItemList, s), l))
  | Noeud
      ( (NonTerminal Comma_OutputItem_star, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal OutputItem, s), l);
          Noeud
            ( (NonTerminal Comma_OutputItem_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal OutputItemList, _),
        [
          Noeud ((NonTerminal OutputItem, s), l);
          Noeud
            ( (NonTerminal Comma_OutputItem_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal OutputItem, s), l))
  | Noeud
      ( (NonTerminal Comma_OutputItem_star, _),
        [
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal OutputItem, s), l);
          Noeud ((NonTerminal Comma_OutputItem_star, s1), l1);
        ] )
  | Noeud
      ( (NonTerminal OutputItemList, _),
        [
          Noeud ((NonTerminal OutputItem, s), l);
          Noeud ((NonTerminal Comma_OutputItem_star, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract (Noeud ((NonTerminal OutputItem, s), l));
            convert_to_abstract
              (Noeud ((NonTerminal Comma_OutputItem_star, s1), l1));
          ] )
  | Noeud ((NonTerminal OutputItem, _), [ Noeud ((NonTerminal Expr, s), l) ]) ->
      convert_to_abstract (Noeud ((NonTerminal Expr, s), l))
  | Noeud
      ( (NonTerminal DoConstruct, _),
        [ Noeud ((NonTerminal BlockDoConstruct, s), l) ] ) ->
      convert_to_abstract (Noeud ((NonTerminal BlockDoConstruct, s), l))
  | Noeud
      ( (NonTerminal BlockDoConstruct, _),
        [
          Noeud ((Terminal Do, _), []);
          Noeud ((NonTerminal LoopControl_opt, _), l);
          Noeud
            ( (NonTerminal ExecutionPartConstruct_star, _),
              [ Noeud ((Terminal E, _), []) ] );
          Noeud ((NonTerminal EndDoStmt, _), _);
        ] ) -> (
      match l with
      | [ Noeud ((Terminal EOS, s), []) ] ->
          Noeud
            ( Syntax While,
              [
                Noeud (DataType (Booleen true), []);
                convert_to_abstract (Noeud ((Terminal EOS, s), []));
              ] )
      | [
       Noeud ((NonTerminal LoopControl, s1), l1); Noeud ((Terminal EOS, s), []);
      ] -> (
          match l1 with
          | Noeud ((Terminal While, _), _) :: _ ->
              Noeud
                ( Syntax While,
                  flatten
                    (convert_to_abstract
                       (Noeud ((NonTerminal LoopControl, s1), l1)))
                    [ convert_to_abstract (Noeud ((Terminal EOS, s), [])) ] )
          | Noeud ((NonTerminal VariableName, _), _) :: _ ->
              Noeud
                ( Syntax For,
                  flatten
                    (convert_to_abstract
                       (Noeud ((NonTerminal LoopControl, s1), l1)))
                    [ convert_to_abstract (Noeud ((Terminal EOS, s), [])) ] )
          | _ -> failwith "")
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal BlockDoConstruct, _),
        [
          Noeud ((Terminal Do, _), []);
          Noeud ((NonTerminal LoopControl_opt, _), l);
          Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
          Noeud ((NonTerminal EndDoStmt, _), _);
        ] ) -> (
      match l with
      | [ Noeud ((Terminal EOS, s), []) ] ->
          Noeud
            ( Syntax While,
              [
                Noeud (DataType (Booleen true), []);
                convert_to_abstract (Noeud ((Terminal EOS, s), []));
                convert_to_abstract
                  (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1));
              ] )
      | [
       Noeud ((NonTerminal LoopControl, s2), l2); Noeud ((Terminal EOS, s), []);
      ] -> (
          match l2 with
          | Noeud ((Terminal While, _), _) :: _ ->
              Noeud
                ( Syntax While,
                  flatten
                    (Noeud
                       ( ToFlatten,
                         [
                           convert_to_abstract
                             (Noeud ((NonTerminal LoopControl, s2), l2));
                           convert_to_abstract (Noeud ((Terminal EOS, s), []));
                           Noeud
                             ( ToFlatten,
                               flatten
                                 (convert_to_abstract
                                    (Noeud
                                       ( ( NonTerminal
                                             ExecutionPartConstruct_star,
                                           s1 ),
                                         l1 )))
                                 [] );
                         ] ))
                    [] )
          | Noeud ((NonTerminal VariableName, _), _) :: _ ->
              Noeud
                ( Syntax For,
                  flatten
                    (Noeud
                       ( ToFlatten,
                         [
                           convert_to_abstract
                             (Noeud ((NonTerminal LoopControl, s2), l2));
                           convert_to_abstract (Noeud ((Terminal EOS, s), []));
                           Noeud
                             ( ToFlatten,
                               flatten
                                 (convert_to_abstract
                                    (Noeud
                                       ( ( NonTerminal
                                             ExecutionPartConstruct_star,
                                           s1 ),
                                         l1 )))
                                 [] );
                         ] ))
                    [] )
          | _ -> failwith "")
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal LoopControl, _),
        [
          Noeud ((Terminal While, _), []);
          Noeud ((Terminal LParenthesis, _), []);
          Noeud ((NonTerminal Expr, s), l);
          Noeud ((Terminal RParenthesis, _), []);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Expr, s), l))
  | Noeud
      ( (NonTerminal LoopControl, _),
        [
          Noeud ((NonTerminal VariableName, s), l);
          Noeud ((Terminal Equal, _), []);
          Noeud ((NonTerminal IntRealDpExpression, s1), l1);
          Noeud ((Terminal Comma, _), []);
          Noeud ((NonTerminal IntRealDpExpression, s2), l2);
          Noeud ((NonTerminal Comma_IntRealDpExpression_opt, _), l3);
        ] ) -> (
      match l3 with
      | [ Noeud ((Terminal E, _), []) ] ->
          Noeud
            ( ToFlatten,
              [
                Noeud
                  ( Operateur Assignation,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal VariableName, s), l));
                      convert_to_abstract
                        (Noeud ((NonTerminal IntRealDpExpression, s1), l1));
                    ] );
                convert_to_abstract
                  (Noeud ((NonTerminal IntRealDpExpression, s2), l2));
                Noeud (Syntax Step, [ Noeud (Integer "1", []) ]);
              ] )
      | [
       Noeud ((Terminal Comma, _), _);
       Noeud ((NonTerminal IntRealDpExpression, s4), l4);
      ] ->
          Noeud
            ( ToFlatten,
              [
                Noeud
                  ( Operateur Assignation,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal VariableName, s), l));
                      convert_to_abstract
                        (Noeud ((NonTerminal IntRealDpExpression, s1), l1));
                    ] );
                convert_to_abstract
                  (Noeud ((NonTerminal IntRealDpExpression, s2), l2));
                Noeud
                  ( Syntax Step,
                    [
                      convert_to_abstract
                        (Noeud ((NonTerminal IntRealDpExpression, s4), l4));
                    ] );
              ] )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal IntRealDpExpression, _),
        [ Noeud ((NonTerminal Expr, s), l) ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Expr, s), l))
  | Noeud
      ( (NonTerminal IfConstruct, _),
        [
          Noeud ((NonTerminal IfThenStmt, _), l);
          Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
          Noeud
            ((NonTerminal ElseIfStmt_ExecutionPartConstruct_star_star, s2), l2);
          Noeud ((NonTerminal ElseStmt_ExecutionPartConstruct_star_opt, s3), l3);
          Noeud
            ( (NonTerminal EndIfStmt, _),
              [
                Noeud ((Terminal EndIf, _), []); Noeud ((Terminal EOS, s4), []);
              ] );
        ] ) -> (
      let next = ref [ convert_to_abstract (Noeud ((Terminal EOS, s4), [])) ] in
      let inner = ref [] in
      (match l3 with
      | [ Noeud ((Terminal E, _), []) ] -> ()
      | _ ->
          next :=
            convert_to_abstract
              (Noeud
                 ((NonTerminal ElseStmt_ExecutionPartConstruct_star_opt, s3), l3))
            :: !next);
      (match l2 with
      | [ Noeud ((Terminal E, _), []) ] -> ()
      | _ ->
          next :=
            convert_to_abstract
              (Noeud
                 ( (NonTerminal ElseIfStmt_ExecutionPartConstruct_star_star, s2),
                   l2 ))
            :: !next);
      (match l1 with
      | [ Noeud ((Terminal E, _), []) ] -> ()
      | _ ->
          inner :=
            flatten
              (convert_to_abstract
                 (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1)))
              !inner);
      match l with
      | [
       Noeud ((Terminal If, _), []);
       Noeud ((Terminal LParenthesis, _), []);
       Noeud ((NonTerminal ScalarLogicalExpr, s), l);
       Noeud ((Terminal RParenthesis, _), []);
       Noeud ((Terminal Then, _), []);
       Noeud ((Terminal EOS, s5), []);
      ] ->
          Noeud
            ( ToFlatten,
              Noeud
                ( Syntax If,
                  convert_to_abstract
                    (Noeud ((NonTerminal ScalarLogicalExpr, s), l))
                  :: flatten
                       (convert_to_abstract (Noeud ((Terminal EOS, s5), [])))
                       !inner )
              :: !next )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal ElseIfStmt_ExecutionPartConstruct_star_star, _),
        [
          Noeud ((NonTerminal ElseIfStmt, s), l);
          Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
        ] ) -> (
      let inner = ref [] in
      (match l1 with
      | [ Noeud ((Terminal E, _), []) ] -> ()
      | _ ->
          inner :=
            flatten
              (convert_to_abstract
                 (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1)))
              !inner);
      match l with
      | [
       Noeud ((Terminal Else, _), []);
       Noeud ((Terminal If, _), []);
       Noeud ((Terminal LParenthesis, _), []);
       Noeud ((NonTerminal ScalarLogicalExpr, s1), l1);
       Noeud ((Terminal RParenthesis, _), []);
       Noeud ((Terminal Then, _), []);
       Noeud ((Terminal EOS, s2), []);
      ] ->
          Noeud
            ( Syntax Else_if,
              convert_to_abstract
                (Noeud ((NonTerminal ScalarLogicalExpr, s1), l1))
              :: convert_to_abstract (Noeud ((Terminal EOS, s2), []))
              :: !inner )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal ExecutionPartConstruct_star, _),
        [
          Noeud ((NonTerminal ExecutionPartConstruct, s), l);
          Noeud
            ( (NonTerminal ExecutionPartConstruct_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal ExecutionPartConstruct, s), l))
  | Noeud
      ( (NonTerminal ExecutionPartConstruct_star, _),
        [
          Noeud ((NonTerminal ExecutionPartConstruct, s), l);
          Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
        ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract
              (Noeud ((NonTerminal ExecutionPartConstruct, s), l));
            convert_to_abstract
              (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1));
          ] )
  | Noeud
      ( (NonTerminal ElseStmt_ExecutionPartConstruct_star_opt, _),
        [
          Noeud ((NonTerminal ElseStmt, _), l);
          Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
        ] ) -> (
      let inner = ref [] in
      (match l1 with
      | [ Noeud ((Terminal E, _), []) ] -> ()
      | _ ->
          inner :=
            convert_to_abstract
              (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1))
            :: !inner);
      match l with
      | [ Noeud ((Terminal Else, _), []); Noeud ((Terminal EOS, s2), []) ] ->
          Noeud
            ( Syntax Else,
              convert_to_abstract (Noeud ((Terminal EOS, s2), [])) :: !inner )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal ExecutionPartConstruct, _),
        [ Noeud ((NonTerminal ExecutableConstruct, s), l) ] ) ->
      Noeud
        ( ToFlatten,
          [
            convert_to_abstract
              (Noeud ((NonTerminal ExecutableConstruct, s), l));
          ] )
  | Noeud
      ((NonTerminal ScalarLogicalExpr, _), [ Noeud ((NonTerminal Expr, s), l) ])
    ->
      convert_to_abstract (Noeud ((NonTerminal Expr, s), l))
  | Noeud
      ( (NonTerminal ScalarIntLiteralConstant, _),
        [ Noeud ((Terminal Icon, s), l) ] ) ->
      Noeud (Integer s, [])
  | Noeud ((NonTerminal Expr, _), [ Noeud ((NonTerminal Level5Expr, s), l) ]) ->
      convert_to_abstract (Noeud ((NonTerminal Level5Expr, s), l))
  | Noeud
      ( (NonTerminal EquivOp_EquivOperand_star, _),
        [
          Noeud ((NonTerminal EquivOp, _), _);
          Noeud ((NonTerminal EquivOperand, s), l);
          Noeud
            ( (NonTerminal EquivOp_EquivOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal Level5Expr, _),
        [
          Noeud ((NonTerminal EquivOperand, s), l);
          Noeud
            ( (NonTerminal EquivOp_EquivOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal EquivOperand, s), l))
  | Noeud
      ( (NonTerminal EquivOp_EquivOperand_star, _),
        [
          Noeud ((NonTerminal EquivOp, _), _);
          Noeud ((NonTerminal EquivOperand, s), l);
          Noeud ((NonTerminal EquivOp_EquivOperand_star, s1), l1);
        ] )
  | Noeud
      ( (NonTerminal Level5Expr, _),
        [
          Noeud ((NonTerminal EquivOperand, s), l);
          Noeud ((NonTerminal EquivOp_EquivOperand_star, s1), l1);
        ] ) -> (
      match l1 with
      | Noeud
          ((NonTerminal EquivOp, _), [ Noeud ((Terminal Equivalent, _), []) ])
        :: _ ->
          Noeud
            ( OperateurLogique Equivalent,
              [
                convert_to_abstract (Noeud ((NonTerminal EquivOperand, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal EquivOp_EquivOperand_star, s1), l1));
              ] )
      | Noeud
          ((NonTerminal EquivOp, _), [ Noeud ((Terminal NotEquivalent, _), []) ])
        :: _ ->
          Noeud
            ( OperateurLogique NonEquivalent,
              [
                convert_to_abstract (Noeud ((NonTerminal EquivOperand, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal EquivOp_EquivOperand_star, s1), l1));
              ] )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal OrOp_OrOperand_star, _),
        [
          Noeud ((Terminal OrOp, _), []);
          Noeud ((NonTerminal OrOperand, s), l);
          Noeud
            ( (NonTerminal OrOp_OrOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal EquivOperand, _),
        [
          Noeud ((NonTerminal OrOperand, s), l);
          Noeud
            ( (NonTerminal OrOp_OrOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal OrOperand, s), l))
  | Noeud
      ( (NonTerminal OrOp_OrOperand_star, _),
        [
          Noeud ((Terminal OrOp, _), []);
          Noeud ((NonTerminal OrOperand, s), l);
          Noeud ((NonTerminal OrOp_OrOperand_star, s2), l2);
        ] )
  | Noeud
      ( (NonTerminal EquivOperand, _),
        [
          Noeud ((NonTerminal OrOperand, s), l);
          Noeud ((NonTerminal OrOp_OrOperand_star, s2), l2);
        ] ) ->
      Noeud
        ( OperateurLogique Ou,
          [
            convert_to_abstract (Noeud ((NonTerminal OrOperand, s), l));
            convert_to_abstract
              (Noeud ((NonTerminal OrOp_OrOperand_star, s2), l2));
          ] )
  | Noeud
      ( (NonTerminal AndOp_AndOperand_star, _),
        [
          Noeud ((Terminal AndOp, _), []);
          Noeud ((NonTerminal AndOperand, s), l);
          Noeud
            ( (NonTerminal AndOp_AndOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal OrOperand, _),
        [
          Noeud ((NonTerminal AndOperand, s), l);
          Noeud
            ( (NonTerminal AndOp_AndOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal AndOperand, s), l))
  | Noeud
      ( (NonTerminal AndOp_AndOperand_star, _),
        [
          Noeud ((Terminal AndOp, _), []);
          Noeud ((NonTerminal AndOperand, s1), l1);
          Noeud ((NonTerminal AndOp_AndOperand_star, s2), l2);
        ] )
  | Noeud
      ( (NonTerminal OrOperand, _),
        [
          Noeud ((NonTerminal AndOperand, s1), l1);
          Noeud ((NonTerminal AndOp_AndOperand_star, s2), l2);
        ] ) ->
      Noeud
        ( OperateurLogique Et,
          [
            convert_to_abstract (Noeud ((NonTerminal AndOperand, s1), l1));
            convert_to_abstract
              (Noeud ((NonTerminal AndOp_AndOperand_star, s2), l2));
          ] )
  | Noeud
      ( (NonTerminal AndOperand, _),
        [
          Noeud ((NonTerminal NotOp_opt, _), [ Noeud ((Terminal E, _), []) ]);
          Noeud ((NonTerminal Level4Expr, s), l);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Level4Expr, s), l))
  | Noeud
      ( (NonTerminal AndOperand, _),
        [
          Noeud ((NonTerminal NotOp_opt, _), [ Noeud ((Terminal NotOp, _), []) ]);
          Noeud ((NonTerminal Level4Expr, s), l);
        ] ) ->
      Noeud
        ( OperateurLogique Non,
          [ convert_to_abstract (Noeud ((NonTerminal Level4Expr, s), l)) ] )
  | Noeud
      ( (NonTerminal RelOp_Level3Expr_star, _),
        [
          Noeud ((NonTerminal RelOp, _), _);
          Noeud ((NonTerminal Level3Expr, s), l);
          Noeud
            ( (NonTerminal RelOp_Level3Expr_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal Level4Expr, _),
        [
          Noeud ((NonTerminal Level3Expr, s), l);
          Noeud
            ( (NonTerminal RelOp_Level3Expr_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l))
  | Noeud
      ( (NonTerminal RelOp_Level3Expr_star, _),
        [
          Noeud ((NonTerminal RelOp, _), _);
          Noeud ((NonTerminal Level3Expr, s), l);
          Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1);
        ] )
  | Noeud
      ( (NonTerminal Level4Expr, _),
        [
          Noeud ((NonTerminal Level3Expr, s), l);
          Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1);
        ] ) -> (
      match l1 with
      | [ Noeud ((Terminal E, _), []) ] ->
          convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l))
      | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal IsEqual, _), []) ])
        :: _ ->
          Noeud
            ( Comparateur Egal,
              [
                convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
              ] )
      | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal NotEqual, _), []) ])
        :: _ ->
          Noeud
            ( Comparateur NonEgal,
              [
                convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
              ] )
      | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal StrictLess, _), []) ])
        :: _ ->
          Noeud
            ( Comparateur StrictPlusPetit,
              [
                convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
              ] )
      | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal LessEqual, _), []) ])
        :: _ ->
          Noeud
            ( Comparateur PlusPetit,
              [
                convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
              ] )
      | Noeud
          ((NonTerminal RelOp, _), [ Noeud ((Terminal StrictGreater, _), []) ])
        :: _ ->
          Noeud
            ( Comparateur StrictPlusGrand,
              [
                convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
              ] )
      | Noeud
          ((NonTerminal RelOp, _), [ Noeud ((Terminal GreaterEqual, _), []) ])
        :: _ ->
          Noeud
            ( Comparateur PlusGrand,
              [
                convert_to_abstract (Noeud ((NonTerminal Level3Expr, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
              ] )
      | _ -> failwith "")
  | Noeud
      ((NonTerminal Level3Expr, _), [ Noeud ((NonTerminal Level2Expr, s), l) ])
    ->
      convert_to_abstract (Noeud ((NonTerminal Level2Expr, s), l))
  | Noeud
      ( (NonTerminal AddOp_Sign_opt_AddOperand_star, _),
        [
          Noeud ((NonTerminal AddOp, _), _);
          Noeud ((NonTerminal Sign_opt_AddOperand, s), l);
          Noeud
            ( (NonTerminal AddOp_Sign_opt_AddOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal Level2Expr, _),
        [
          Noeud ((NonTerminal Sign_opt_AddOperand, s), l);
          Noeud
            ( (NonTerminal AddOp_Sign_opt_AddOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Sign_opt_AddOperand, s), l))
  | Noeud
      ( (NonTerminal AddOp_Sign_opt_AddOperand_star, _),
        [
          Noeud ((NonTerminal AddOp, _), _);
          Noeud ((NonTerminal Sign_opt_AddOperand, s), l);
          Noeud ((NonTerminal AddOp_Sign_opt_AddOperand_star, s2), l2);
        ] )
  | Noeud
      ( (NonTerminal Level2Expr, _),
        [
          Noeud ((NonTerminal Sign_opt_AddOperand, s), l);
          Noeud ((NonTerminal AddOp_Sign_opt_AddOperand_star, s2), l2);
        ] ) -> (
      match l2 with
      | Noeud
          ( (NonTerminal AddOp, _),
            [
              Noeud ((NonTerminal Sign, _), [ Noeud ((Terminal Plus, _), []) ]);
            ] )
        :: _ ->
          Noeud
            ( Operateur Plus,
              [
                convert_to_abstract
                  (Noeud ((NonTerminal Sign_opt_AddOperand, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal AddOp_Sign_opt_AddOperand_star, s2), l2));
              ] )
      | Noeud
          ( (NonTerminal AddOp, _),
            [
              Noeud ((NonTerminal Sign, _), [ Noeud ((Terminal Minus, _), []) ]);
            ] )
        :: _ ->
          Noeud
            ( Operateur Moins,
              [
                convert_to_abstract
                  (Noeud ((NonTerminal Sign_opt_AddOperand, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal AddOp_Sign_opt_AddOperand_star, s2), l2));
              ] )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal Sign_opt_AddOperand, _),
        [
          Noeud ((NonTerminal Sign_opt, _), [ Noeud ((Terminal E, _), []) ]);
          Noeud ((NonTerminal AddOperand, s), l);
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal AddOperand, s), l))
  | Noeud
      ( (NonTerminal Sign_opt_AddOperand, _),
        [
          Noeud
            ( (NonTerminal Sign_opt, _),
              [
                Noeud ((NonTerminal Sign, _), [ Noeud ((Terminal Plus, _), []) ]);
              ] );
          Noeud ((NonTerminal AddOperand, s), l);
        ] ) ->
      Noeud
        ( Operateur Plus,
          [ convert_to_abstract (Noeud ((NonTerminal AddOperand, s), l)) ] )
  | Noeud
      ( (NonTerminal Sign_opt_AddOperand, _),
        [
          Noeud
            ( (NonTerminal Sign_opt, _),
              [
                Noeud
                  ((NonTerminal Sign, _), [ Noeud ((Terminal Minus, _), []) ]);
              ] );
          Noeud ((NonTerminal AddOperand, s), l);
        ] ) ->
      Noeud
        ( Operateur Moins,
          [ convert_to_abstract (Noeud ((NonTerminal AddOperand, s), l)) ] )
  | Noeud
      ( (NonTerminal MultOp_MultOperand_star, _),
        [
          Noeud ((NonTerminal MultOp, _), _);
          Noeud ((NonTerminal MultOperand, s), l);
          Noeud
            ( (NonTerminal MultOp_MultOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal AddOperand, _),
        [
          Noeud ((NonTerminal MultOperand, s), l);
          Noeud
            ( (NonTerminal MultOp_MultOperand_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal MultOperand, s), l))
  | Noeud
      ( (NonTerminal MultOp_MultOperand_star, _),
        [
          Noeud ((NonTerminal MultOp, _), _);
          Noeud ((NonTerminal MultOperand, s), l);
          Noeud ((NonTerminal MultOp_MultOperand_star, s2), l2);
        ] )
  | Noeud
      ( (NonTerminal AddOperand, _),
        [
          Noeud ((NonTerminal MultOperand, s), l);
          Noeud ((NonTerminal MultOp_MultOperand_star, s2), l2);
        ] ) -> (
      match l2 with
      | Noeud ((NonTerminal MultOp, _), [ Noeud ((Terminal Asterisk, _), []) ])
        :: _ ->
          Noeud
            ( Operateur Fois,
              [
                convert_to_abstract (Noeud ((NonTerminal MultOperand, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal MultOp_MultOperand_star, s2), l2));
              ] )
      | Noeud ((NonTerminal MultOp, _), [ Noeud ((Terminal Divise, _), []) ])
        :: _ ->
          Noeud
            ( Operateur Division,
              [
                convert_to_abstract (Noeud ((NonTerminal MultOperand, s), l));
                convert_to_abstract
                  (Noeud ((NonTerminal MultOp_MultOperand_star, s2), l2));
              ] )
      | _ -> failwith "")
  | Noeud
      ( (NonTerminal PowerOp_Level1Expr_star, _),
        [
          Noeud ((Terminal PowerOp, _), []);
          Noeud ((NonTerminal Level1Expr, s), l);
          Noeud
            ( (NonTerminal PowerOp_Level1Expr_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] )
  | Noeud
      ( (NonTerminal MultOperand, _),
        [
          Noeud ((NonTerminal Level1Expr, s), l);
          Noeud
            ( (NonTerminal PowerOp_Level1Expr_star, _),
              [ Noeud ((Terminal E, _), []) ] );
        ] ) ->
      convert_to_abstract (Noeud ((NonTerminal Level1Expr, s), l))
  | Noeud
      ( (NonTerminal PowerOp_Level1Expr_star, _),
        [
          Noeud ((Terminal PowerOp, _), []);
          Noeud ((NonTerminal Level1Expr, s), l);
          Noeud ((NonTerminal PowerOp_Level1Expr_star, s2), l2);
        ] )
  | Noeud
      ( (NonTerminal MultOperand, _),
        [
          Noeud ((NonTerminal Level1Expr, s), l);
          Noeud ((NonTerminal PowerOp_Level1Expr_star, s2), l2);
        ] ) ->
      Noeud
        ( Operateur Puissance,
          [
            convert_to_abstract (Noeud ((NonTerminal Level1Expr, s), l));
            convert_to_abstract
              (Noeud ((NonTerminal PowerOp_Level1Expr_star, s2), l2));
          ] )
  | Noeud ((NonTerminal Level1Expr, _), [ Noeud ((NonTerminal Primary, s), l) ])
    ->
      convert_to_abstract (Noeud ((NonTerminal Primary, s), l))
  | Noeud ((NonTerminal Primary, _), l) -> (
      match l with
      | [ Noeud ((Terminal Icon, s), []) ] -> Noeud (Integer s, [])
      | [ Noeud ((Terminal Rcon, s), []) ] -> Noeud (Floating s, [])
      | [ Noeud ((Terminal Dcon, s), []) ] -> Noeud (Double s, [])
      | [ Noeud ((NonTerminal Name, _), [ Noeud ((Terminal Ident, s), []) ]) ]
        ->
          Noeud (Name s, [])
      | [
       Noeud ((NonTerminal Scon, _), [ Noeud ((Terminal SconSingle, s), []) ]);
      ] ->
          Noeud (Chaine s, [])
      | [
       Noeud ((NonTerminal Scon, _), [ Noeud ((Terminal SconDouble, s), []) ]);
      ] ->
          Noeud (Chaine s, [])
      | [
       Noeud
         ((NonTerminal LogicalConstant, _), [ Noeud ((Terminal True, _), []) ]);
      ] ->
          Noeud (Booleen true, [])
      | [
       Noeud
         ((NonTerminal LogicalConstant, _), [ Noeud ((Terminal False, _), []) ]);
      ] ->
          Noeud (Booleen false, [])
      | [
       Noeud ((Terminal LParenthesis, _), []);
       Noeud ((NonTerminal Expr, s), l);
       Noeud ((Terminal RParenthesis, _), []);
      ] ->
          Noeud
            ( ToFlatten,
              [
                Noeud (Parentheseouvrante, []);
                convert_to_abstract (Noeud ((NonTerminal Expr, s), l));
                Noeud (Parenthesefermante, []);
              ] )
      | _ -> failwith "")
  | Noeud ((NonTerminal Name, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal ArrayName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal ComponentName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal EndName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal DummyArgName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal FunctionName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud
      ((NonTerminal ImpliedDoVariable, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal ProgramName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal SubroutineName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud
      ((NonTerminal SubroutineNameUse, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal VariableName, _), [ Noeud ((Terminal Ident, s), []) ])
  | Noeud ((NonTerminal ObjectName, _), [ Noeud ((Terminal Ident, s), []) ]) ->
      Noeud (Name s, [])
  | Noeud ((Terminal EOS, s), []) ->
      Noeud (ToFlatten, convert_comments (Noeud (Commentaire s, [])))
  | _ ->
      (let string_of_symbol (s : symbol) : string =
         match s with
         | Terminal x -> string_of_terminal x
         | NonTerminal x -> string_of_non_terminal x
       in
       match t with
       | Noeud ((s, _), Noeud ((x, _), _) :: _) ->
           prerr_string (string_of_symbol s);
           prerr_char ' ';
           prerr_string (string_of_symbol x);
           prerr_newline ()
       | Noeud ((s, _), _) -> prerr_string (string_of_symbol s));
      failwith "is not implemented yet"
