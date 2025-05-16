open Abstract_tokens

type environnement = (string * token) list
type environnement_v2 = (string, token) Hashtbl.t
type intent = In | Out | InOut
type var_type_subroutine = (string, intent) Hashtbl.t

(** crée un environnement à partir de l'ast [t] *)
let create_env_from_ast (t : ast) : environnement_v2 =
  let env = Hashtbl.create 0 in
  (** ajoute réccursivement sur [t] les variables dans l'environnement *)
  let rec add_env (t : ast) : unit =
    match t with
    | Noeud (Syntax Double_precision, [ Noeud (Name s, []) ])
    | Noeud
        ( Syntax Double_precision,
          [ Noeud (Operateur Assignation, [ Noeud (Name s, []); _ ]) ] ) ->
        Hashtbl.add env s (Syntax Double_precision)
    | Noeud (Syntax Integer, [ Noeud (Name s, []) ])
    | Noeud
        ( Syntax Integer,
          [ Noeud (Operateur Assignation, [ Noeud (Name s, []); _ ]) ] ) ->
        Hashtbl.add env s (Syntax Integer)
    | Noeud (Syntax Real, [ Noeud (Name s, []) ])
    | Noeud
        ( Syntax Real,
          [ Noeud (Operateur Assignation, [ Noeud (Name s, []); _ ]) ] ) ->
        Hashtbl.add env s (Syntax Real)
    | Noeud (Syntax Logical, [ Noeud (Name s, []) ])
    | Noeud
        ( Syntax Logical,
          [ Noeud (Operateur Assignation, [ Noeud (Name s, []); _ ]) ] ) ->
        Hashtbl.add env s (Syntax Real)
    | Noeud (Syntax Character, [ Noeud (Syntax Size, _); Noeud (Name s, []) ])
    | Noeud (Syntax Character, [ Noeud (Name s, []) ])
    | Noeud
        ( Syntax Character,
          [
            Noeud
              ( Operateur Assignation,
                [ Noeud (Syntax Size, _); Noeud (Name s, []); _ ] );
          ] )
    | Noeud
        ( Syntax Character,
          [ Noeud (Operateur Assignation, [ Noeud (Name s, []); _ ]) ] ) ->
        Hashtbl.add env s (Syntax Real)
    | Noeud (_, l) -> List.iter add_env l
  in
  add_env t;
  env

(** Met le type de [var] dans [env] à [t] *)
let rec set_type (env : environnement) (var : string) (t : token) :
    environnement =
  match env with
  | (v, t2) :: q ->
      if var = v then (var, t) :: q else (v, t2) :: set_type q var t
  | [] -> (var, t) :: []

(** récupère le type de [var] dans [env] *)
let get_type (env : environnement) (var : string) : token = List.assoc var env

(** déduis de l'utilisation des variables dans [t] si elles sont in, out out *)
let create_subroutine_intent (t : ast) : var_type_subroutine =
  let sub_intent = Hashtbl.create 0 in
  (** traite réccursivement les variables sur [t] *)
  let rec subroutine_intent_aux (t : ast) : unit =
    (* TODO ajouter lorsque la syntaxe obstraite pour le in/out/inout est ok *)
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
