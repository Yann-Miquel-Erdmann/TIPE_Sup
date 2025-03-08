open Abstract_tokens

  
type environnement = (string*token) list



let rec set_type (env: environnement) (var: string) (t: token): environnement =
  match env with
  | (v,t2)::q  -> if var=v then (var,t)::q else (v,t2)::(set_type q var t)
  | [] -> (var,t)::[]

let get_type (env:environnement) (var:string) : token = 
  List.assoc var env


