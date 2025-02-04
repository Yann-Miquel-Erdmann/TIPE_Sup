open Tokens

  
type environnement = (string*Tokens.syntax) list



let rec set_type (env: environnement) (var: string) (t: Tokens.syntax): environnement =
  match env with
  | (v,t2)::q  -> if var=v then (var,t)::q else (v,t2)::(set_type q var t)
  | [] -> (var,t)::[]

let get_type (env:environnement) (var:string) : Tokens.syntax = 
  List.assoc var env


