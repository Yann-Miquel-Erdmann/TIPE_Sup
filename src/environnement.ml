open Abstract_tokens

type environnement = (string * token) list
type environnement_v2 = (string, token) Hashtbl.t

(** crée un environnement à partir de l'ast [t] *)
let create_env_from_ast (t : ast) : environnement_v2 =
  let env = Hashtbl.create 0 in
  (** ajoute réccursivement sur t les variables dans l'environnement *)
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

let get_type (env : environnement) (var : string) : token = List.assoc var env
