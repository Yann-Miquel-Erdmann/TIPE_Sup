open Tokens
type environnement = (string*Tokens.data_type) list


let rec set_type (env: environnement) (var: string) (t: Tokens.data_type): environnement =
  match env with
  | [] -> (var,t)::[]
  | (v,t2)::q when v = var -> (var,t)::q
  | (v,t2)::q -> (v,t2)::(set_type env var t)

let get_type (env:environnement) (var:string) : Tokens.data_type = List.assoc var env


