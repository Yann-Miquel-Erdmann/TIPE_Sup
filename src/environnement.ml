open Abstract_tokens

type environnement = (string, token) Hashtbl.t
type intent = In | Out | InOut
type var_type_subroutine = (string, intent) Hashtbl.t

let print_env (env: environnement): unit = 
  Hashtbl.iter (fun k v -> print_string k ; print_string " -> " ; print_token v; print_newline ()) env 


let rec last_of_list (l: 'a list): 'a =   
  match l with 
  | [] -> failwith "Liste vide "
  | e::[] -> e
  | e::q -> last_of_list q


(** crée un environnement à partir de l'ast [t] *)
let create_env_from_ast (t : ast) : environnement =
  let env = Hashtbl.create 0 in
  (** ajoute récursivement sur [t] les variables dans l'environnement *)
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
    | Noeud (Syntax Function, Noeud(Name n,[])::l) ->(
        List.iter add_env l; 
        match last_of_list l with
        | Noeud(Syntax Return , Noeud(Name v,[])::_) ->  Hashtbl.add env n (Hashtbl.find env v)   
        | _ -> () (* pas de return à la fin de la fct donc c'est une subroutine *)
      )

    | Noeud (_, l) -> List.iter add_env l
  in
  add_env t;
  env

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
