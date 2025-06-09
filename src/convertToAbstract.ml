open AbstractTokens
open LL1
open Symbols

(** écrase l'entièreté de l'arbre [t] pour enlever les Noeud ToFlatten *)
let global_flatten (t : ast) : ast =
  let rec aux (t : ast) : ast list =
    match t with
    | Noeud (ToFlatten, l) ->
        List.rev
          (List.fold_left
             (fun acc x ->
               match aux x with
               | [] -> acc
               | [ x1 ] -> x1 :: acc
               | l1 -> List.rev_append l1 acc)
             [] l)
    | Noeud (t, l) ->
        [
          Noeud
            ( t,
              List.rev
                (List.fold_left
                   (fun acc x ->
                     match aux x with
                     | [] -> acc
                     | [ x1 ] -> x1 :: acc
                     | l1 -> List.rev_append l1 acc)
                   [] l) );
        ]
  in
  match aux t with
  | [ x ] -> x
  | _ -> failwith "Il ne peut y avoir qu'un seul Program_Root"

(** convertis l'arbre de syntaxe abstrait [t] pour que les fonctions, dont le
    nom est stocké au fur et à mesure dans [curr_func], aient leur valeur de
    retour *)
let rec link_return_function (t : ast) (return_val : string option) : ast =
  match t with
  | Noeud (Syntax Function, l) ->
      let (opt : ast list option ref) = ref None in
      let name, l =
        List.fold_left
          (fun (_name, _l) (x : ast) ->
            match x with
            | Noeud (Name s, []) when _name = "" -> (s, x :: _l)
            | Noeud (Syntax Out, []) -> (
                match !opt with
                | Some
                    [ Noeud (NewLine, []); Noeud (t, [ Noeud (Name "", []) ]) ]
                  ->
                    ( _name,
                      List.rev_append
                        ([
                           Noeud (NewLine, []);
                           Noeud (t, [ Noeud (Name _name, []) ]);
                         ]
                          : ast list)
                        _l )
                | Some l1 -> (_name, List.rev_append l1 _l)
                | _ -> (_name, _l))
            | Noeud (Syntax Out, [ Noeud (Name s, []) ]) -> (
                match !opt with
                | Some
                    [ Noeud (NewLine, []); Noeud (t, [ Noeud (Name _, []) ]) ]
                  ->
                    ( s,
                      List.rev_append
                        ([
                           Noeud (NewLine, []); Noeud (t, [ Noeud (Name s, []) ]);
                         ]
                          : ast list)
                        _l )
                | Some l1 -> (s, List.rev_append l1 _l)
                | None -> (s, _l))
            | Noeud (Syntax Out, [ Noeud (t, []) ]) ->
                opt :=
                  Some
                    [
                      Noeud (NewLine, []); Noeud (t, [ Noeud (Name _name, []) ]);
                    ];
                (_name, Noeud (t, []) :: _l)
            | _ -> (_name, x :: _l))
          ("", []) l
      in
      let l =
        match l with
        | Noeud (Syntax Return, []) :: q -> List.rev l
        | _ ->
            let (l : ast list) =
              Noeud (Syntax Return, [ Noeud (Name name, []) ]) :: l
            in
            List.rev l
      in
      Noeud
        ( Syntax Function,
          List.map (fun x -> link_return_function x (Some name)) l )
  | Noeud (Syntax Subroutine, l) ->
      Noeud
        ( Syntax Function,
          match List.rev l with
          | Noeud (Syntax Return, []) :: q -> List.rev q
          | _ -> l )
  | Noeud (Syntax Return, []) -> (
      match return_val with
      | None -> failwith "Return doit être dans une fonction"
      | Some s -> Noeud (Syntax Return, [ Noeud (Name s, []) ]))
  | Noeud (x, l) ->
      Noeud (x, List.map (fun x -> link_return_function x return_val) l)

(** met les fonction contenue dans le morceau "contains" de [t] un niveau au
    dessus des fonctions au lieu d'en dessous pour avoir un traitement correct
*)
let increase_function_level (t : ast) : ast =
  match t with
  | Noeud (ProgramRoot, l) ->
      let functions =
        List.fold_left
          (fun acc (x : ast) ->
            match x with
            | Noeud (Syntax Program, l1) ->
                List.fold_left
                  (fun acc1 (x1 : ast) ->
                    match x1 with
                    | Noeud (Syntax Function, l2) -> x1 :: acc
                    | _ -> acc)
                  acc l1
            | _ -> acc)
          [] l
      in
      Noeud
        ( ProgramRoot,
          functions
          @ List.map
              (fun (x0 : ast) : ast ->
                match x0 with
                | Noeud (Syntax Program, l1) ->
                    Noeud
                      ( Syntax Program,
                        List.filter
                          (fun (x : ast) ->
                            match x with
                            | Noeud (Syntax Function, l2) -> false
                            | _ -> true)
                          l1 )
                | _ -> x0)
              l )
  | _ -> failwith "The root should be ProgramRoot"

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
let convert_to_abstract (t : at) : ast =
  let rec convert_to_abstract_aux (t : at) : ast =
    match t with
    | Noeud ((NonTerminal ExecutableProgram, _), l) -> (
        match l with
        | [
         Noeud ((NonTerminal StartCommentBlock, s1), l1);
         Noeud ((NonTerminal Function_or_Subroutine_star_MainProgram, s), l);
        ] ->
            Noeud
              ( ProgramRoot,
                [
                  convert_to_abstract_aux (Noeud ((Terminal EOS, s1), []));
                  convert_to_abstract_aux
                    (Noeud
                       ( (NonTerminal Function_or_Subroutine_star_MainProgram, s),
                         l ));
                ] )
        | [
         Noeud ((NonTerminal Function_or_Subroutine_star_MainProgram, s), l);
        ] ->
            Noeud
              ( ProgramRoot,
                [
                  convert_to_abstract_aux
                    (Noeud
                       ( (NonTerminal Function_or_Subroutine_star_MainProgram, s),
                         l ));
                ] )
        | _ -> failwith "ExecutableProgram")
    | Noeud ((NonTerminal Function_or_Subroutine_star_MainProgram, _), l) -> (
        match l with
        | [
         Noeud ((NonTerminal Recursive_opt_Function_or_Subroutine, s), l);
         Noeud ((NonTerminal Function_or_Subroutine_star_MainProgram, s1), l1);
        ] ->
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract_aux
                    (Noeud
                       ((NonTerminal Recursive_opt_Function_or_Subroutine, s), l));
                  convert_to_abstract_aux
                    (Noeud
                       ( ( NonTerminal Function_or_Subroutine_star_MainProgram,
                           s1 ),
                         l1 ));
                ] )
        | [
         Noeud ((NonTerminal MainProgram, s), l);
         Noeud ((NonTerminal Function_or_Subroutine_star, s1), l1);
        ] ->
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal MainProgram, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Function_or_Subroutine_star, s1), l1));
                ] )
        | _ -> failwith "Function_or_Subroutine_star_MainProgram")
    | Noeud
        ( (NonTerminal Function_or_Subroutine_star, _),
          [
            Noeud ((NonTerminal Recursive_opt_Function_or_Subroutine, s), l);
            Noeud ((NonTerminal Function_or_Subroutine_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal Recursive_opt_Function_or_Subroutine, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Function_or_Subroutine_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal Recursive_opt_Function_or_Subroutine, _),
          [
            Noeud ((Terminal Recursive, _), []);
            Noeud ((NonTerminal Function_or_Subroutine, s), l);
          ] )
    | Noeud
        ( (NonTerminal Recursive_opt_Function_or_Subroutine, _),
          [ Noeud ((NonTerminal Function_or_Subroutine, s), l) ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal Function_or_Subroutine, s), l))
    | Noeud
        ( (NonTerminal Function_or_Subroutine, _),
          [ Noeud ((NonTerminal FunctionSubprogram, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal FunctionSubprogram, s), l))
    | Noeud
        ( (NonTerminal Function_or_Subroutine, _),
          [ Noeud ((NonTerminal SubroutineSubprogram, s), l) ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal SubroutineSubprogram, s), l))
    | Noeud
        ( (NonTerminal FunctionSubprogram, _),
          [
            Noeud ((NonTerminal FunctionPrefix, s), l);
            Noeud ((NonTerminal FunctionName, s1), l1);
            Noeud ((NonTerminal FunctionRange, s2), l2);
          ] ) -> (
        match l with
        | [ Noeud ((Terminal Function, _), []) ] ->
            Noeud
              ( Syntax Function,
                [
                  Noeud
                    ( ToFlatten,
                      [
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal FunctionName, s1), l1));
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal FunctionRange, s2), l2));
                      ] );
                ] )
        | _ ->
            Noeud
              ( Syntax Function,
                [
                  Noeud
                    ( ToFlatten,
                      [
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal FunctionPrefix, s), l));
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal FunctionName, s1), l1));
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal FunctionRange, s2), l2));
                      ] );
                ] ))
    | Noeud
        ( (NonTerminal FunctionPrefix, _),
          [
            Noeud ((NonTerminal TypeSpec, s), l);
            Noeud ((Terminal Function, _), []);
          ] ) ->
        Noeud
          ( Syntax Out,
            [ convert_to_abstract_aux (Noeud ((NonTerminal TypeSpec, s), l)) ]
          )
    | Noeud ((NonTerminal TypeSpec, s), l) ->
        let t =
          match l with
          | [ Noeud ((Terminal Integer, _), []) ] -> Syntax Integer
          | [ Noeud ((Terminal Double, _), []) ] -> Syntax Double_precision
          | [ Noeud ((Terminal Complex, _), []) ] -> Syntax Complex
          | [ Noeud ((Terminal Logical, _), []) ] -> Syntax Logical
          | [ Noeud ((Terminal Real, _), []) ] -> Syntax Real
          | [ Noeud ((Terminal Character, _), []) ] -> Syntax Character
          | _ -> failwith "TypeSpec"
        in
        Noeud (t, [])
    | Noeud
        ( (NonTerminal FunctionRange, _),
          [
            Noeud ((NonTerminal FunctionParList, s), l);
            Noeud ((NonTerminal FunctionResult_opt, s4), l4);
            Noeud ((Terminal EOS, s1), l1);
            Noeud ((NonTerminal BodyConstruct_star, s3), l3);
            Noeud ((NonTerminal EndFunctionStmt, s2), l2);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal FunctionParList, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal FunctionResult_opt, s4), l4));
              convert_to_abstract_aux (Noeud ((Terminal EOS, s1), l1));
              convert_to_abstract_aux
                (Noeud ((NonTerminal BodyConstruct_star, s3), l3));
              convert_to_abstract_aux
                (Noeud ((NonTerminal EndFunctionStmt, s2), l2));
            ] )
    | Noeud
        ( (NonTerminal FunctionResult_opt, _),
          [
            Noeud ((Terminal Result, _), []);
            Noeud ((Terminal LParenthesis, _), []);
            Noeud ((NonTerminal VariableName, s), l);
            Noeud ((Terminal RParenthesis, _), []);
          ] ) ->
        Noeud
          ( Syntax Out,
            [
              convert_to_abstract_aux (Noeud ((NonTerminal VariableName, s), l));
            ] )
    | Noeud
        ( (NonTerminal FunctionParList, _),
          [
            Noeud ((Terminal LParenthesis, _), []);
            Noeud ((NonTerminal FunctionPar_Comma_FunctionPar_star_opt, s), l);
            Noeud ((Terminal RParenthesis, _), []);
          ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal FunctionPar_Comma_FunctionPar_star_opt, s), l))
    | Noeud
        ( (NonTerminal FunctionPar_Comma_FunctionPar_star_opt, _),
          [
            Noeud ((NonTerminal FunctionPar, s), l);
            Noeud ((NonTerminal Comma_FunctionPar_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((NonTerminal FunctionPar, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_FunctionPar_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal Comma_FunctionPar_star, _),
          [
            Noeud ((Terminal Comma, _), []);
            Noeud ((NonTerminal FunctionPar, s), l);
            Noeud ((NonTerminal Comma_FunctionPar_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((NonTerminal FunctionPar, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_FunctionPar_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal FunctionPar, _),
          [ Noeud ((NonTerminal DummyArgName, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal DummyArgName, s), l))
    | Noeud
        ( (NonTerminal EndFunctionStmt, _),
          [
            Noeud ((Terminal EndFunction, _), []);
            Noeud ((NonTerminal EndName_opt, _), _);
            Noeud ((Terminal EOS, s), l);
          ] ) ->
        convert_to_abstract_aux (Noeud ((Terminal EOS, s), l))
    | Noeud
        ( (NonTerminal SubroutineSubprogram, _),
          [
            Noeud ((Terminal Subroutine, _), []);
            Noeud ((NonTerminal SubroutineName, s), l);
            Noeud ((NonTerminal SubroutineRange, s1), l1);
          ] ) ->
        Noeud
          ( Syntax Subroutine,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal SubroutineName, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal SubroutineRange, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal SubroutineRange, _),
          [
            Noeud ((NonTerminal SubroutineParList_opt, s), l);
            Noeud ((Terminal EOS, s1), l1);
            Noeud ((NonTerminal BodyConstruct_star, s2), l2);
            Noeud ((NonTerminal EndSubroutineStmt, s3), l3);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal SubroutineParList_opt, s), l));
              convert_to_abstract_aux (Noeud ((Terminal EOS, s1), l1));
              convert_to_abstract_aux
                (Noeud ((NonTerminal BodyConstruct_star, s2), l2));
              convert_to_abstract_aux
                (Noeud ((NonTerminal EndSubroutineStmt, s3), l3));
            ] )
    | Noeud
        ( (NonTerminal SubroutineParList_opt, _),
          [
            Noeud ((Terminal LParenthesis, _), []);
            Noeud
              ((NonTerminal SubroutinePar_Comma_SubroutinePar_star_opt, s), l);
            Noeud ((Terminal RParenthesis, _), []);
          ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal SubroutinePar_Comma_SubroutinePar_star_opt, s), l))
    | Noeud
        ( (NonTerminal SubroutinePar_Comma_SubroutinePar_star_opt, _),
          [
            Noeud ((NonTerminal SubroutinePar, s), l);
            Noeud ((NonTerminal Comma_SubroutinePar_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal SubroutinePar, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_SubroutinePar_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal Comma_SubroutinePar_star, _),
          [
            Noeud ((Terminal Comma, _), []);
            Noeud ((NonTerminal SubroutinePar, s), l);
            Noeud ((NonTerminal Comma_SubroutinePar_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal SubroutinePar, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_SubroutinePar_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal SubroutinePar, _),
          [ Noeud ((NonTerminal DummyArgName, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal DummyArgName, s), l))
    | Noeud
        ( (NonTerminal EndSubroutineStmt, _),
          [
            Noeud ((Terminal EndSubroutine, _), []);
            Noeud ((NonTerminal EndName_opt, _), _);
            Noeud ((Terminal EOS, s), l);
          ] ) ->
        convert_to_abstract_aux (Noeud ((Terminal EOS, s), l))
    | Noeud
        ( (NonTerminal MainProgram, _),
          [
            Noeud ((NonTerminal ProgramStmt, s), l);
            Noeud ((NonTerminal MainRange, s1), l1);
          ] ) ->
        Noeud
          ( Syntax Program,
            [
              Noeud
                ( ToFlatten,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal ProgramStmt, s), l));
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal MainRange, s1), l1));
                  ] );
            ] )
    | Noeud
        ( (NonTerminal Contains_Function, _),
          [
            Noeud ((Terminal Contains, _), []);
            Noeud ((Terminal EOS, s), l);
            Noeud ((NonTerminal FunctionSubprogram_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((Terminal EOS, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal FunctionSubprogram_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal FunctionSubprogram_star, _),
          [
            Noeud ((NonTerminal FunctionSubprogram, s), l);
            Noeud ((NonTerminal FunctionSubprogram_star, s1), l1);
          ] )
    | Noeud
        ( (NonTerminal FunctionSubprogram_star, _),
          [
            Noeud ((Terminal Recursive, _), []);
            Noeud ((NonTerminal FunctionSubprogram, s), l);
            Noeud ((NonTerminal FunctionSubprogram_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal FunctionSubprogram, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal FunctionSubprogram_star, s1), l1));
            ] )
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
              convert_to_abstract_aux (Noeud ((NonTerminal ProgramName, s), l));
              convert_to_abstract_aux (Noeud ((Terminal EOS, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal MainRange, _),
          [ Noeud ((NonTerminal Contains_Function_opt_EndProgramStmt, s), l) ]
        ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal Contains_Function_opt_EndProgramStmt, s), l))
    | Noeud
        ( (NonTerminal MainRange, _),
          [
            Noeud ((NonTerminal BodyConstruct, s), l);
            Noeud ((NonTerminal BodyConstruct_star, s2), l2);
            Noeud ((NonTerminal Contains_Function_opt_EndProgramStmt, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal BodyConstruct, s), l));
              Noeud
                ( ToFlatten,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal BodyConstruct_star, s2), l2));
                  ] );
              convert_to_abstract_aux
                (Noeud
                   ((NonTerminal Contains_Function_opt_EndProgramStmt, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal BodyConstruct_star, _),
          [
            Noeud ((NonTerminal BodyConstruct, s), l);
            Noeud ((NonTerminal BodyConstruct_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal BodyConstruct, s), l));
              Noeud
                ( ToFlatten,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal BodyConstruct_star, s1), l1));
                  ] );
            ] )
    | Noeud
        ( (NonTerminal Contains_Function_opt_EndProgramStmt, _),
          [ Noeud ((NonTerminal EndProgramStmt, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal EndProgramStmt, s), l))
    | Noeud
        ( (NonTerminal Contains_Function_opt_EndProgramStmt, _),
          [
            Noeud ((NonTerminal Contains_Function, s1), l1);
            Noeud ((NonTerminal EndProgramStmt, s), l);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal EndProgramStmt, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Contains_Function, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal EndProgramStmt, _),
          [
            Noeud ((Terminal EndProgram, _), []);
            Noeud ((NonTerminal EndName_opt, _), _);
            Noeud ((Terminal EOS, s), []);
          ] ) ->
        convert_to_abstract_aux (Noeud ((Terminal EOS, s), []))
    | Noeud
        ( (NonTerminal BodyConstruct, _),
          [ Noeud ((NonTerminal SpecificationPartConstruct, s), l) ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal SpecificationPartConstruct, s), l));
            ] )
    | Noeud
        ( (NonTerminal BodyConstruct, _),
          [ Noeud ((NonTerminal ExecutableConstruct, s), l) ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal ExecutableConstruct, s), l))
    | Noeud
        ( (NonTerminal SpecificationPartConstruct, _),
          [ Noeud ((NonTerminal DeclarationConstruct, s), l) ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal DeclarationConstruct, s), l));
            ] )
    | Noeud
        ( (NonTerminal DeclarationConstruct, _),
          [ Noeud ((NonTerminal TypeDeclarationStmt, s), l) ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal TypeDeclarationStmt, s), l));
            ] )
    | Noeud
        ( (NonTerminal TypeDeclarationStmt, _),
          [
            Noeud
              ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Double, _), []) ]);
            Noeud ((NonTerminal Comma_AttrSpec_star, _), l1);
            Noeud ((NonTerminal TypeDecl_Assignment, s), l);
            Noeud ((Terminal EOS, s2), []);
          ] ) ->
        (match l1 with
        | [ Noeud ((Terminal E, _), []) ]
        | [
            Noeud ((Terminal Comma, _), []);
            Noeud
              ( (NonTerminal AttrSpec, _),
                [ Noeud ((NonTerminal Intent_in_out, _), _) ] );
            Noeud
              ( (NonTerminal Comma_AttrSpec_star, _),
                [ Noeud ((Terminal E, _), []) ] );
          ] ->
            ()
        | _ -> failwith "TypeDeclarationStmt 1");
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax Double_precision,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud
        ( (NonTerminal TypeDeclarationStmt, _),
          [
            Noeud
              ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Integer, _), []) ]);
            Noeud ((NonTerminal Comma_AttrSpec_star, _), l1);
            Noeud ((NonTerminal TypeDecl_Assignment, s), l);
            Noeud ((Terminal EOS, s2), []);
          ] ) ->
        (match l1 with
        | [ Noeud ((Terminal E, _), []) ]
        | [
            Noeud ((Terminal Comma, _), []);
            Noeud
              ( (NonTerminal AttrSpec, _),
                [ Noeud ((NonTerminal Intent_in_out, _), _) ] );
            Noeud
              ( (NonTerminal Comma_AttrSpec_star, _),
                [ Noeud ((Terminal E, _), []) ] );
          ] ->
            ()
        | _ -> failwith "TypeDeclarationStmt 2");
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax Integer,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud
        ( (NonTerminal TypeDeclarationStmt, _),
          [
            Noeud
              ( (NonTerminal TypeSpec, _),
                [ Noeud ((Terminal Character, _), []) ] );
            Noeud ((NonTerminal Comma_AttrSpec_star, s1), l1);
            Noeud ((NonTerminal TypeDecl_Assignment, s), l);
            Noeud ((Terminal EOS, s2), []);
          ] ) ->
        let n1 = ref [] in
        (match l1 with
        | [ Noeud ((Terminal E, _), []) ]
        | [
            Noeud ((Terminal Comma, _), []);
            Noeud
              ( (NonTerminal AttrSpec, _),
                [ Noeud ((NonTerminal Intent_in_out, _), _) ] );
            Noeud
              ( (NonTerminal Comma_AttrSpec_star, _),
                [ Noeud ((Terminal E, _), []) ] );
          ] ->
            ()
        | _ ->
            n1 :=
              [
                convert_to_abstract_aux
                  (Noeud ((NonTerminal Comma_AttrSpec_star, s1), l1));
              ]);
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax Character,
                  [
                    Noeud (ToFlatten, !n1);
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud
        ( (NonTerminal Comma_AttrSpec_star, _),
          [
            Noeud ((NonTerminal Intent_in_out, _), _);
            Noeud ((NonTerminal Comma_AttrSpec_star, s), l);
          ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal Comma_AttrSpec_star, s), l))
    | Noeud
        ( (NonTerminal Comma_AttrSpec_star, _),
          [
            Noeud ((Terminal Parameter, _), []);
            Noeud ((NonTerminal Comma_AttrSpec_star, _), _);
            (* the rest must be intent_in_out if not empty. either way, nothing to be done *)
          ] ) ->
        Noeud (Syntax Constant, [])
    | Noeud
        ( (NonTerminal TypeDeclarationStmt, _),
          [
            Noeud
              ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Complex, _), []) ]);
            Noeud ((NonTerminal Comma_AttrSpec_star, _), l1);
            Noeud ((NonTerminal TypeDecl_Assignment, s), l);
            Noeud ((Terminal EOS, s2), []);
          ] ) ->
        (match l1 with
        | [ Noeud ((Terminal E, _), []) ]
        | [
            Noeud ((Terminal Comma, _), []);
            Noeud
              ( (NonTerminal AttrSpec, _),
                [ Noeud ((NonTerminal Intent_in_out, _), _) ] );
            Noeud
              ( (NonTerminal Comma_AttrSpec_star, _),
                [ Noeud ((Terminal E, _), []) ] );
          ] ->
            ()
        | _ -> failwith "TypeDeclarationStmt 3");
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax Complex,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud
        ( (NonTerminal TypeDeclarationStmt, _),
          [
            Noeud
              ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Logical, _), []) ]);
            Noeud ((NonTerminal Comma_AttrSpec_star, _), l1);
            Noeud ((NonTerminal TypeDecl_Assignment, s), l);
            Noeud ((Terminal EOS, s2), []);
          ] ) ->
        (match l1 with
        | [ Noeud ((Terminal E, _), []) ]
        | [
            Noeud ((Terminal Comma, _), []);
            Noeud
              ( (NonTerminal AttrSpec, _),
                [ Noeud ((NonTerminal Intent_in_out, _), _) ] );
            Noeud
              ( (NonTerminal Comma_AttrSpec_star, _),
                [ Noeud ((Terminal E, _), []) ] );
          ] ->
            ()
        | _ -> failwith "TypeDeclarationStmt 4");
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax Logical,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud
        ( (NonTerminal TypeDeclarationStmt, _),
          [
            Noeud ((NonTerminal TypeSpec, _), [ Noeud ((Terminal Real, _), []) ]);
            Noeud ((NonTerminal Comma_AttrSpec_star, _), l1);
            Noeud ((NonTerminal TypeDecl_Assignment, s), l);
            Noeud ((Terminal EOS, s2), []);
          ] ) ->
        (match l1 with
        | [ Noeud ((Terminal E, _), []) ]
        | [
            Noeud ((Terminal Comma, _), []);
            Noeud
              ( (NonTerminal AttrSpec, _),
                [ Noeud ((NonTerminal Intent_in_out, _), _) ] );
            Noeud
              ( (NonTerminal Comma_AttrSpec_star, _),
                [ Noeud ((Terminal E, _), []) ] );
          ] ->
            ()
        | _ -> failwith "TypeDeclarationStmt 5");
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax Real,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal TypeDecl_Assignment, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud ((NonTerminal AttrSpec, _), [ Noeud ((Terminal Parameter, _), []) ])
      ->
        Noeud (Syntax Constant, [])
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
              convert_to_abstract_aux (Noeud ((NonTerminal EntityDecl, s), l));
              Noeud
                ( ToFlatten,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal Comma_EntityDecl_star, s2), l2));
                  ] );
            ] )
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
              convert_to_abstract_aux (Noeud ((NonTerminal ObjectName, s), l));
              convert_to_abstract_aux
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
        convert_to_abstract_aux (Noeud ((NonTerminal ObjectName, s), l))
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
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal CharLength, s1), l1));
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
              convert_to_abstract_aux (Noeud ((NonTerminal Expr, s2), l2));
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
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal CharLength, s1), l1));
                  ] );
              Noeud (Name s, []);
              convert_to_abstract_aux (Noeud ((NonTerminal Expr, s2), l2));
            ] )
    | Noeud
        ( (NonTerminal CharLength, _),
          [
            Noeud ((Terminal LParenthesis, _), []);
            Noeud ((NonTerminal TypeParamValue, s), l);
            Noeud ((Terminal RParenthesis, _), []);
          ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal TypeParamValue, s), l))
    | Noeud
        ( (NonTerminal CharLength, _),
          [ Noeud ((NonTerminal ScalarIntLiteralConstant, s), l) ] ) ->
        convert_to_abstract_aux
          (Noeud ((NonTerminal ScalarIntLiteralConstant, s), l))
    | Noeud
        ( (NonTerminal TypeParamValue, _),
          [ Noeud ((NonTerminal Expr_Or_Asterisk, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr_Or_Asterisk, s), l))
    | Noeud
        ( (NonTerminal Expr_Or_Asterisk, _),
          [ Noeud ((Terminal Asterisk, _), []) ] ) ->
        Noeud (Syntax Any, [])
    | Noeud
        ((NonTerminal Expr_Or_Asterisk, _), [ Noeud ((NonTerminal Expr, s), l) ])
      ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l))
    | Noeud ((NonTerminal ExecutableConstruct, _), l) -> (
        match l with
        | [ Noeud ((NonTerminal ActionStmt, s1), l1) ] ->
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal ActionStmt, s1), l1));
                ] )
        | [ Noeud ((NonTerminal DoConstruct, s1), l1) ] ->
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal DoConstruct, s1), l1));
                ] )
        | [ Noeud ((NonTerminal IfConstructEndif, s1), l1) ] ->
            Noeud
              ( ToFlatten,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal IfConstructEndif, s1), l1));
                ] )
        | [
         Noeud
           ( (NonTerminal ReturnStmt, _),
             [ Noeud ((Terminal Return, _), []); Noeud ((Terminal EOS, s), l) ]
           );
        ] ->
            Noeud
              ( ToFlatten,
                [
                  Noeud (Syntax Return, []);
                  convert_to_abstract_aux (Noeud ((Terminal EOS, s), l));
                ] )
        | _ -> failwith "ExecutableConstruct")
    | Noeud
        ( (NonTerminal ActionStmt, _),
          [ Noeud ((NonTerminal AssignmentStmt, s), l) ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal AssignmentStmt, s), l));
            ] )
    | Noeud
        ((NonTerminal ActionStmt, _), [ Noeud ((NonTerminal PrintStmt, s), l) ])
      ->
        Noeud
          ( ToFlatten,
            [ convert_to_abstract_aux (Noeud ((NonTerminal PrintStmt, s), l)) ]
          )
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
                    convert_to_abstract_aux (Noeud ((NonTerminal Name, s), l));
                    convert_to_abstract_aux (Noeud ((NonTerminal Expr, s1), l1));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
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
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal Comma_OutputItemList_opt, s), l));
                  ] );
              convert_to_abstract_aux (Noeud ((Terminal EOS, s2), []));
            ] )
    | Noeud
        ( (NonTerminal Comma_OutputItemList_opt, _),
          [
            Noeud ((Terminal Comma, _), []);
            Noeud ((NonTerminal OutputItemList, s), l);
          ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal OutputItemList, s), l))
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
              convert_to_abstract_aux (Noeud ((NonTerminal OutputItem, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_OutputItem_star, s1), l1));
            ] )
    | Noeud ((NonTerminal OutputItem, _), [ Noeud ((NonTerminal Expr, s), l) ])
      ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l))
    | Noeud
        ( (NonTerminal DoConstruct, _),
          [ Noeud ((NonTerminal BlockDoConstruct, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal BlockDoConstruct, s), l))
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
                  Noeud (Booleen true, []);
                  convert_to_abstract_aux (Noeud ((Terminal EOS, s), []));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1));
                ] )
        | [
         Noeud ((NonTerminal LoopControl, s2), l2); Noeud ((Terminal EOS, s), []);
        ] -> (
            match l2 with
            | Noeud ((Terminal While, _), _) :: _ ->
                Noeud
                  ( Syntax While,
                    [
                      Noeud
                        ( ToFlatten,
                          [
                            convert_to_abstract_aux
                              (Noeud ((NonTerminal LoopControl, s2), l2));
                            convert_to_abstract_aux
                              (Noeud ((Terminal EOS, s), []));
                            Noeud
                              ( ToFlatten,
                                [
                                  convert_to_abstract_aux
                                    (Noeud
                                       ( ( NonTerminal
                                             ExecutionPartConstruct_star,
                                           s1 ),
                                         l1 ));
                                ] );
                          ] );
                    ] )
            | Noeud ((NonTerminal VariableName, _), _) :: _ ->
                Noeud
                  ( Syntax For,
                    [
                      Noeud
                        ( ToFlatten,
                          [
                            convert_to_abstract_aux
                              (Noeud ((NonTerminal LoopControl, s2), l2));
                            convert_to_abstract_aux
                              (Noeud ((Terminal EOS, s), []));
                            Noeud
                              ( ToFlatten,
                                [
                                  convert_to_abstract_aux
                                    (Noeud
                                       ( ( NonTerminal
                                             ExecutionPartConstruct_star,
                                           s1 ),
                                         l1 ));
                                ] );
                          ] );
                    ] )
            | _ -> failwith "BlockDoConstruct 1")
        | _ -> failwith "BlockDoConstruct 2")
    | Noeud
        ( (NonTerminal LoopControl, _),
          [
            Noeud ((Terminal While, _), []);
            Noeud ((Terminal LParenthesis, _), []);
            Noeud ((NonTerminal Expr, s), l);
            Noeud ((Terminal RParenthesis, _), []);
          ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l))
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
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal VariableName, s), l));
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal IntRealDpExpression, s1), l1));
                      ] );
                  convert_to_abstract_aux
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
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal VariableName, s), l));
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal IntRealDpExpression, s1), l1));
                      ] );
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal IntRealDpExpression, s2), l2));
                  Noeud
                    ( Syntax Step,
                      [
                        convert_to_abstract_aux
                          (Noeud ((NonTerminal IntRealDpExpression, s4), l4));
                      ] );
                ] )
        | _ -> failwith "LoopControl")
    | Noeud
        ( (NonTerminal IntRealDpExpression, _),
          [ Noeud ((NonTerminal Expr, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l))
    | Noeud
        ( (NonTerminal IfConstructEndif, _),
          [
            Noeud ((NonTerminal IfConstruct, s), l);
            Noeud ((NonTerminal ElseIfConstruct_star, s1), l1);
            Noeud ((NonTerminal EndIfStmt, s2), l2);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              Noeud
                ( Syntax If,
                  [
                    convert_to_abstract_aux
                      (Noeud ((NonTerminal IfConstruct, s), l));
                  ] );
              convert_to_abstract_aux
                (Noeud ((NonTerminal ElseIfConstruct_star, s1), l1));
              convert_to_abstract_aux (Noeud ((NonTerminal EndIfStmt, s2), l2));
            ] )
    | Noeud
        ( (NonTerminal IfConstruct, _),
          [
            Noeud ((NonTerminal IfThenStmt, s), l);
            Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((NonTerminal IfThenStmt, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal ElseIfConstruct_star, _),
          [
            Noeud ((NonTerminal ElseIfConstruct, s), l);
            Noeud ((NonTerminal ElseIfConstruct_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal ElseIfConstruct, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal ElseIfConstruct_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal ElseIfConstruct, _),
          [
            Noeud ((Terminal Else, _), []);
            Noeud ((NonTerminal IfConstruct_opt, s), l);
          ] ) -> (
        match l with
        | [ Noeud ((NonTerminal IfConstruct, _), _) ] ->
            Noeud
              ( Syntax Else_if,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal IfConstruct_opt, s), l));
                ] )
        | [
         Noeud ((Terminal EOS, _), _);
         Noeud ((NonTerminal ExecutionPartConstruct_star, _), _);
        ] ->
            Noeud
              ( Syntax Else,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal IfConstruct_opt, s), l));
                ] )
        | _ -> failwith "ElseIfConstruct")
    | Noeud
        ( (NonTerminal IfConstruct_opt, _),
          [ Noeud ((NonTerminal IfConstruct, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal IfConstruct, s), l))
    | Noeud
        ( (NonTerminal IfConstruct_opt, _),
          [
            Noeud ((Terminal EOS, s), l);
            Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((Terminal EOS, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal IfThenStmt, _),
          [
            Noeud ((Terminal If, _), []);
            Noeud ((Terminal LParenthesis, _), []);
            Noeud ((NonTerminal ScalarLogicalExpr, s), l);
            Noeud ((Terminal RParenthesis, _), []);
            Noeud ((Terminal Then, _), []);
            Noeud ((Terminal EOS, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal ScalarLogicalExpr, s), l));
              convert_to_abstract_aux (Noeud ((Terminal EOS, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal EndIfStmt, _),
          [ Noeud ((Terminal EndIf, _), []); Noeud ((Terminal EOS, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((Terminal EOS, s), l))
    | Noeud
        ( (NonTerminal ExecutionPartConstruct_star, _),
          [
            Noeud ((NonTerminal ExecutionPartConstruct, s), l);
            Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal ExecutionPartConstruct, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal ExecutionPartConstruct_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal ExecutionPartConstruct, _),
          [ Noeud ((NonTerminal ExecutableConstruct, s), l) ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux
                (Noeud ((NonTerminal ExecutableConstruct, s), l));
            ] )
    | Noeud
        ( (NonTerminal ScalarLogicalExpr, _),
          [ Noeud ((NonTerminal Expr, s), l) ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l))
    | Noeud
        ( (NonTerminal ScalarIntLiteralConstant, _),
          [ Noeud ((Terminal Icon, s), l) ] ) ->
        Noeud (Integer s, [])
    | Noeud ((NonTerminal Expr, _), [ Noeud ((NonTerminal Level5Expr, s), l) ])
      ->
        convert_to_abstract_aux (Noeud ((NonTerminal Level5Expr, s), l))
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
        convert_to_abstract_aux (Noeud ((NonTerminal EquivOperand, s), l))
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
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal EquivOperand, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal EquivOp_EquivOperand_star, s1), l1));
                ] )
        | Noeud
            ( (NonTerminal EquivOp, _),
              [ Noeud ((Terminal NotEquivalent, _), []) ] )
          :: _ ->
            Noeud
              ( OperateurLogique NonEquivalent,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal EquivOperand, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal EquivOp_EquivOperand_star, s1), l1));
                ] )
        | _ -> failwith "Level5Expr")
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
        convert_to_abstract_aux (Noeud ((NonTerminal OrOperand, s), l))
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
              convert_to_abstract_aux (Noeud ((NonTerminal OrOperand, s), l));
              convert_to_abstract_aux
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
        convert_to_abstract_aux (Noeud ((NonTerminal AndOperand, s), l))
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
              convert_to_abstract_aux (Noeud ((NonTerminal AndOperand, s1), l1));
              convert_to_abstract_aux
                (Noeud ((NonTerminal AndOp_AndOperand_star, s2), l2));
            ] )
    | Noeud
        ( (NonTerminal AndOperand, _),
          [
            Noeud ((NonTerminal NotOp_opt, _), [ Noeud ((Terminal E, _), []) ]);
            Noeud ((NonTerminal Level4Expr, s), l);
          ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal Level4Expr, s), l))
    | Noeud
        ( (NonTerminal AndOperand, _),
          [
            Noeud
              ((NonTerminal NotOp_opt, _), [ Noeud ((Terminal NotOp, _), []) ]);
            Noeud ((NonTerminal Level4Expr, s), l);
          ] ) ->
        Noeud
          ( OperateurLogique Non,
            [ convert_to_abstract_aux (Noeud ((NonTerminal Level4Expr, s), l)) ]
          )
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
            convert_to_abstract_aux (Noeud ((NonTerminal Level3Expr, s), l))
        | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal IsEqual, _), []) ])
          :: _ ->
            Noeud
              ( Comparateur Egal,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Level3Expr, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
                ] )
        | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal NotEqual, _), []) ])
          :: _ ->
            Noeud
              ( Comparateur NonEgal,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Level3Expr, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
                ] )
        | Noeud
            ((NonTerminal RelOp, _), [ Noeud ((Terminal StrictLess, _), []) ])
          :: _ ->
            Noeud
              ( Comparateur StrictPlusPetit,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Level3Expr, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
                ] )
        | Noeud ((NonTerminal RelOp, _), [ Noeud ((Terminal LessEqual, _), []) ])
          :: _ ->
            Noeud
              ( Comparateur PlusPetit,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Level3Expr, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
                ] )
        | Noeud
            ((NonTerminal RelOp, _), [ Noeud ((Terminal StrictGreater, _), []) ])
          :: _ ->
            Noeud
              ( Comparateur StrictPlusGrand,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Level3Expr, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
                ] )
        | Noeud
            ((NonTerminal RelOp, _), [ Noeud ((Terminal GreaterEqual, _), []) ])
          :: _ ->
            Noeud
              ( Comparateur PlusGrand,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Level3Expr, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal RelOp_Level3Expr_star, s1), l1));
                ] )
        | _ -> failwith "Level4Expr")
    | Noeud
        ((NonTerminal Level3Expr, _), [ Noeud ((NonTerminal Level2Expr, s), l) ])
      ->
        convert_to_abstract_aux (Noeud ((NonTerminal Level2Expr, s), l))
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
        | [ Noeud ((Terminal E, _), []) ] ->
            convert_to_abstract_aux
              (Noeud ((NonTerminal Sign_opt_AddOperand, s), l))
        | Noeud
            ( (NonTerminal AddOp, _),
              [
                Noeud ((NonTerminal Sign, _), [ Noeud ((Terminal Plus, _), []) ]);
              ] )
          :: _ ->
            Noeud
              ( Operateur Plus,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Sign_opt_AddOperand, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal AddOp_Sign_opt_AddOperand_star, s2), l2));
                ] )
        | Noeud
            ( (NonTerminal AddOp, _),
              [
                Noeud
                  ((NonTerminal Sign, _), [ Noeud ((Terminal Minus, _), []) ]);
              ] )
          :: _ ->
            Noeud
              ( Operateur Moins,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal Sign_opt_AddOperand, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal AddOp_Sign_opt_AddOperand_star, s2), l2));
                ] )
        | _ -> failwith "Level2Expr")
    | Noeud
        ( (NonTerminal Sign_opt_AddOperand, _),
          [
            Noeud ((NonTerminal Sign_opt, _), [ Noeud ((Terminal E, _), []) ]);
            Noeud ((NonTerminal AddOperand, s), l);
          ] ) ->
        convert_to_abstract_aux (Noeud ((NonTerminal AddOperand, s), l))
    | Noeud
        ( (NonTerminal Sign_opt_AddOperand, _),
          [
            Noeud
              ( (NonTerminal Sign_opt, _),
                [
                  Noeud
                    ((NonTerminal Sign, _), [ Noeud ((Terminal Plus, _), []) ]);
                ] );
            Noeud ((NonTerminal AddOperand, s), l);
          ] ) ->
        Noeud
          ( Operateur Plus,
            [ convert_to_abstract_aux (Noeud ((NonTerminal AddOperand, s), l)) ]
          )
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
            [ convert_to_abstract_aux (Noeud ((NonTerminal AddOperand, s), l)) ]
          )
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
        | [ Noeud ((Terminal E, _), []) ] ->
            convert_to_abstract_aux (Noeud ((NonTerminal MultOperand, s), l))
        | Noeud ((NonTerminal MultOp, _), [ Noeud ((Terminal Asterisk, _), []) ])
          :: _ ->
            Noeud
              ( Operateur Fois,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal MultOperand, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal MultOp_MultOperand_star, s2), l2));
                ] )
        | Noeud ((NonTerminal MultOp, _), [ Noeud ((Terminal Divise, _), []) ])
          :: _ ->
            Noeud
              ( Operateur Division,
                [
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal MultOperand, s), l));
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal MultOp_MultOperand_star, s2), l2));
                ] )
        | _ -> failwith "AddOperand")
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
        convert_to_abstract_aux (Noeud ((NonTerminal Level1Expr, s), l))
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
              convert_to_abstract_aux (Noeud ((NonTerminal Level1Expr, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal PowerOp_Level1Expr_star, s2), l2));
            ] )
    | Noeud
        ((NonTerminal Level1Expr, _), [ Noeud ((NonTerminal Primary, s), l) ])
      ->
        convert_to_abstract_aux (Noeud ((NonTerminal Primary, s), l))
    | Noeud ((NonTerminal Primary, _), l) -> (
        match l with
        | [ Noeud ((Terminal Icon, s), []) ] -> Noeud (Integer s, [])
        | [ Noeud ((Terminal Rcon, s), []) ] -> Noeud (Floating s, [])
        | [ Noeud ((Terminal Dcon, s), []) ] -> Noeud (Double s, [])
        | [
         Noeud ((NonTerminal Name, _), [ Noeud ((Terminal Ident, s), []) ]);
         Noeud
           ( (NonTerminal FunctionReference_opt, _),
             [ Noeud ((Terminal E, _), []) ] );
        ] ->
            Noeud (Name s, [])
        | [
         Noeud ((NonTerminal Name, _), [ Noeud ((Terminal Ident, s), []) ]);
         Noeud
           ( (NonTerminal FunctionReference_opt, _),
             [
               Noeud ((Terminal LParenthesis, _), []);
               Noeud
                 ( ( NonTerminal
                       FunctionArg_Comma_FunctionArg_star_opt_RParenthesis,
                     _ ),
                   [ Noeud ((Terminal RParenthesis, _), []) ] );
             ] );
        ] ->
            Noeud (Syntax Call, [ Noeud (Name s, []) ])
        | [
         Noeud ((NonTerminal Name, _), [ Noeud ((Terminal Ident, s), []) ]);
         Noeud ((NonTerminal FunctionReference_opt, s1), l1);
        ] ->
            Noeud
              ( Syntax Call,
                [
                  Noeud (Name s, []);
                  convert_to_abstract_aux
                    (Noeud ((NonTerminal FunctionReference_opt, s1), l1));
                ] )
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
           ( (NonTerminal LogicalConstant, _),
             [ Noeud ((Terminal False, _), []) ] );
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
                  convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l));
                  Noeud (Parenthesefermante, []);
                ] )
        | _ -> failwith "Primary")
    | Noeud
        ( (NonTerminal FunctionReference_opt, _),
          [
            Noeud ((Terminal LParenthesis, _), []);
            Noeud
              ( ( NonTerminal FunctionArg_Comma_FunctionArg_star_opt_RParenthesis,
                  s ),
                l );
          ] ) ->
        convert_to_abstract_aux
          (Noeud
             ( ( NonTerminal FunctionArg_Comma_FunctionArg_star_opt_RParenthesis,
                 s ),
               l ))
    | Noeud
        ( (NonTerminal FunctionArg_Comma_FunctionArg_star_opt_RParenthesis, _),
          [
            Noeud ((NonTerminal FunctionArg, s), l);
            Noeud ((NonTerminal Comma_FunctionArg_star, s1), l1);
            Noeud ((Terminal RParenthesis, _), []);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((NonTerminal FunctionArg, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_FunctionArg_star, s1), l1));
            ] )
    | Noeud
        ( (NonTerminal Comma_FunctionArg_star, _),
          [
            Noeud ((Terminal Comma, _), []);
            Noeud ((NonTerminal FunctionArg, s), l);
            Noeud ((NonTerminal Comma_FunctionArg_star, s1), l1);
          ] ) ->
        Noeud
          ( ToFlatten,
            [
              convert_to_abstract_aux (Noeud ((NonTerminal FunctionArg, s), l));
              convert_to_abstract_aux
                (Noeud ((NonTerminal Comma_FunctionArg_star, s1), l1));
            ] )
    | Noeud ((NonTerminal FunctionArg, _), [ Noeud ((NonTerminal Expr, s), l) ])
      ->
        convert_to_abstract_aux (Noeud ((NonTerminal Expr, s), l))
    | Noeud ((NonTerminal Name, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal ArrayName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal ComponentName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal EndName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal DummyArgName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal FunctionName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud
        ((NonTerminal ImpliedDoVariable, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal ProgramName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud
        ((NonTerminal SubroutineName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud
        ((NonTerminal SubroutineNameUse, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal VariableName, _), [ Noeud ((Terminal Ident, s), []) ])
    | Noeud ((NonTerminal ObjectName, _), [ Noeud ((Terminal Ident, s), []) ])
      ->
        Noeud (Name s, [])
    | Noeud ((Terminal EOS, s), []) ->
        Noeud (ToFlatten, convert_comments (Noeud (Commentaire s, [])))
    | Noeud ((NonTerminal _, _), [ Noeud ((Terminal E, _), []) ])
    | Noeud ((Terminal E, _), []) ->
        Noeud (ToFlatten, [])
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
         | Noeud ((s, _), _) ->
             prerr_string (string_of_symbol s);
             prerr_newline ());
        failwith "is not implemented yet"
  in
  increase_function_level
    (link_return_function (global_flatten (convert_to_abstract_aux t)) None)
