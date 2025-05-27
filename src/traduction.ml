type string_or_string_list = S of string | L of string_or_string_list list

(** crée une chaîne de [n] tabulations *)
let tabs_to_string (n : int) : string_or_string_list = S (String.make n '\t')

(** crée une chaîne de [n] retours à la ligne *)
let rec n_new_lines (n : int) : string_or_string_list = S (String.make n '\n')

let string_of_string_or_string_list (sosl : string_or_string_list) : string =
  let rec aux (sosl : string_or_string_list) (acc : string list) : string list =
    match sosl with
    | S s -> s :: acc
    | L [] -> acc
    | L (e :: q) ->
        let acc2 = aux e acc in
        aux (L q) acc2
  in
  String.concat "" (List.rev (aux sosl []))

let rec last_of_list (l : 'a list) : 'a =
  match l with
  | [] -> failwith "Liste vide "
  | e :: [] -> e
  | e :: q -> last_of_list q

let print_sosl (sosl : string_or_string_list) : unit =
  print_string (string_of_string_or_string_list sosl);
  print_newline ()
