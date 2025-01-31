open Tokens

type libs = string list

let add_lib (l: libs) (name: string): libs = 
  if not (List.mem name l) then
    name::l
  else
    l
