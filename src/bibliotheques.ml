open AbstractTokens

type libs = string list

module StringSet = Set.Make (String)

let add_lib (l : libs) (name : string) : libs =
  if not (List.mem name l) then name :: l else l

let generate_libs (t : ast) : libs =
  let rec generate_libs_aux (t : ast) : StringSet.t =
    match t with
    | Noeud (_, []) -> StringSet.empty
    | Noeud (Syntax Print, l) ->
        List.fold_left
          (fun acc x -> StringSet.union acc (generate_libs_aux x))
          (StringSet.singleton "stdio")
          l
    | Noeud (Operateur Puissance, l) ->
        List.fold_left
          (fun acc x -> StringSet.union acc (generate_libs_aux x))
          (StringSet.singleton "math")
          l
    | Noeud (_, l) ->
        List.fold_left
          (fun acc x -> StringSet.union acc (generate_libs_aux x))
          StringSet.empty l
  in
  StringSet.to_list (generate_libs_aux t)
