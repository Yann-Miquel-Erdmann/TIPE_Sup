open AbstractTokens

type environnement = (string, token) Hashtbl.t
type intent = In | Out | InOut
type var_type_subroutine = (string, intent) Hashtbl.t

let print_env (env : environnement) : unit =
  Hashtbl.iter
    (fun k v ->
      print_string k;
      print_string " -> ";
      print_token v;
      print_newline ())
    env

let rec last_of_list (l : 'a list) : 'a =
  match l with
  | [] -> failwith "Liste vide "
  | [ e ] -> e
  | e :: q -> last_of_list q

(** crée un environnement à partir de l'ast [t] et modifie l'ast pour enlever
    les redéfinitions inutiles *)
let create_env_from_ast (t : ast) : environnement * ast =
  let env = Hashtbl.create 0 in
  let func_arg_mem = Hashtbl.create 0 in
  (* None : unset, true : fuction, false subroutine with name if the function/subroutine *)
  let curr_func = ref (None, "") in

  let is_valid_env_token (t : syntax) : bool =
    match t with
    | Integer | Double_precision | Complex | Character | Logical | Real -> true
    | _ -> false
  in

  (** ajoute récursivement sur [t] les variables dans l'environnement *)
  let rec add_env (t : ast) : ast =
    match t with
    | Noeud (Syntax Program, l) ->
        curr_func := (Some true, "main");
        Noeud (Syntax Program, List.map add_env l)
    | Noeud (Syntax Return, _) | Noeud (Syntax Print, _) -> t
    | Noeud (Syntax Function, Noeud (Name s, []) :: l) ->
        List.iter
          (fun x ->
            match x with
            | Noeud (Name s1, []) -> (
                match last_of_list l with
                | Noeud (Syntax Return, Noeud (Name _, []) :: _) ->
                    (* is a function*)
                    curr_func := (Some true, s);
                    Hashtbl.add func_arg_mem ("f_" ^ s ^ "_" ^ s1) ()
                | _ ->
                    (* is a subroutine*)
                    curr_func := (Some false, s);
                    Hashtbl.add func_arg_mem ("s_" ^ s ^ "_" ^ s1) ())
            | _ -> ())
          l;

        let l1 =
          List.filter (fun x -> x <> Noeud (ToFlatten, [])) (List.map add_env l)
        in
        (match last_of_list l1 with
        | Noeud (Syntax Return, Noeud (Name v, []) :: _) ->
            Hashtbl.add env s (Hashtbl.find env v)
        | _ -> ());
        curr_func := (None, "");
        Noeud (Syntax Function, Noeud (Name s, []) :: l1)
    | Noeud
        (Syntax tok, [ Noeud (Operateur Assignation, Noeud (Name s1, []) :: l) ])
      when is_valid_env_token tok -> (
        Hashtbl.add env s1 (Syntax tok);
        match !curr_func with
        | Some true, s when Hashtbl.mem func_arg_mem ("f_" ^ s ^ "_" ^ s1) ->
            Noeud (Operateur Assignation, Noeud (Name s1, []) :: l)
        | Some false, s when Hashtbl.mem func_arg_mem ("s_" ^ s ^ "_" ^ s1) ->
            Noeud (Operateur Assignation, Noeud (Name s1, []) :: l)
        | _ -> t)
    | Noeud (Syntax tok, l) when is_valid_env_token tok -> (
        let l1 =
          List.filter
            (fun x -> x <> Noeud (ToFlatten, []))
            (List.map
               (fun x ->
                 match x with
                 | Noeud (Name s1, []) -> (
                     Hashtbl.add env s1 (Syntax tok);
                     match !curr_func with
                     | Some true, s
                       when Hashtbl.mem func_arg_mem ("f_" ^ s ^ "_" ^ s1) ->
                         Noeud (ToFlatten, [])
                     | Some false, s
                       when Hashtbl.mem func_arg_mem ("s_" ^ s ^ "_" ^ s1) ->
                         Noeud (ToFlatten, [])
                     | _ -> Noeud (Name s1, []))
                 | _ -> Noeud (ToFlatten, []))
               l)
        in
        match l1 with [] -> Noeud (ToFlatten, []) | _ -> Noeud (Syntax tok, l1))
    (* TODO make this working with a list *)
    | Noeud
        ( Syntax Character,
          [
            Noeud (Syntax Size, _);
            Noeud (Name s1, []) (* <- this can be a list *);
          ] ) -> (
        Hashtbl.add env s1 (Syntax Character);
        match !curr_func with
        | Some true, s when Hashtbl.mem func_arg_mem ("f_" ^ s ^ "_" ^ s1) ->
            Noeud (ToFlatten, [])
        | Some false, s when Hashtbl.mem func_arg_mem ("s_" ^ s ^ "_" ^ s1) ->
            Noeud (ToFlatten, [])
        | _ -> t
        (* TODO make this working with a list *))
    | Noeud
        ( Syntax Character,
          [
            Noeud
              ( Operateur Assignation,
                Noeud (Syntax Size, _) :: Noeud (Name s1, []) :: l1 );
          ] ) -> (
        Hashtbl.add env s1 (Syntax Character);
        match !curr_func with
        | Some true, s when Hashtbl.mem func_arg_mem ("f_" ^ s ^ "_" ^ s1) ->
            Noeud (Operateur Assignation, Noeud (Name s1, []) :: l1)
        | Some false, s when Hashtbl.mem func_arg_mem ("s_" ^ s ^ "_" ^ s1) ->
            Noeud (Operateur Assignation, Noeud (Name s1, []) :: l1)
        | _ -> t)
    | Noeud (t, l) -> Noeud (t, List.map add_env l)
  in
  let t1 = add_env t in
  if
    Hashtbl.fold
      (fun _ v acc ->
        acc
        &&
        match v with
        | Syntax Double_precision
        | Syntax Integer
        | Syntax Real
        | Syntax Logical
        | Syntax Character ->
            true
        | t ->
            print_endline (string_of_token t);
            false)
      env true
  then (env, t1)
  else failwith "type invalide"

(** déduis de l'utilisation des variables dans [t] si elles sont in, out out *)
let create_subroutine_intent (t : ast) : var_type_subroutine =
  let sub_intent = Hashtbl.create 0 in
  (** traite récursivement les variables sur [t] *)
  let rec subroutine_intent_aux (t : ast) : unit =
    (* TODO ajouter lorsque la syntaxe abstraite pour le in/out/inout est ok *)
    match t with
    | Noeud (Operateur Assignation, Noeud (Name s, []) :: _)
      when not (Hashtbl.mem sub_intent s) ->
        Hashtbl.add sub_intent s Out
    | Noeud (Operateur Assignation, Noeud (Name s, []) :: _)
      when Hashtbl.find sub_intent s = In ->
        Hashtbl.replace sub_intent s InOut
    | Noeud (Operateur Assignation, Noeud (Name s, []) :: _)
      when Hashtbl.find sub_intent s = Out || Hashtbl.find sub_intent s = InOut
      ->
        ()
    | Noeud (Name s, []) when not (Hashtbl.mem sub_intent s) ->
        Hashtbl.add sub_intent s In
    | Noeud (Name s, []) when Hashtbl.find sub_intent s = Out ->
        Hashtbl.add sub_intent s InOut
    | Noeud (Name s, [])
      when Hashtbl.find sub_intent s = Out || Hashtbl.find sub_intent s = InOut
      ->
        ()
    | Noeud (_, l) -> List.iter subroutine_intent_aux l
  in
  subroutine_intent_aux t;
  sub_intent
