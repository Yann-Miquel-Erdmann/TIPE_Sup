open Tokens
open Regex
open Automates
open Dictionnaire

module Parser2 = struct 

type dico = automaton list;;

let file = open_in "test.f90";;
let read_file (file_name: string):string list = 
  let rec lire file liste = 
    let line = input_line file in
      ();
    try lire file (line::liste) with End_of_file->
      close_in file;
      line::liste
  in List.rev (lire (open_in file_name) [])
;;

let exec_char (a : automate_det_v2) (node : int) (c : char) : int =
  (Vector.get a.transitions_ node).(int_of_char c)
;;

let execution_mot (a : automate_det_v2) (texte : char list) : int * char list * char list =
  let node = ref a.debut in
  let last_found = ref (-1) in
  let text_as_last = ref texte in
  let texte = ref texte in
  let text_read = ref [] in
  let last_read = ref [] in
  while !node != -1 do
    match !texte with
    | [] -> node := -1 (* forcer la fin de la boucle*)
    | c::q ->
      begin
        text_read := c::!text_read;
        node := exec_char a !node c;
        texte := q;
        if !node = -1 then () else 
        match a.fin.(!node) with
        | None -> ()
        | Some t ->
          last_found := !node;
          text_as_last := !texte;
          last_read := !text_read
      end;
  done;
  !last_found, !text_as_last, !last_read
;;

let rec exec (a : automate_det_v2) (texte : char list) (out : token list): token list =
  match texte with
  | [] -> List.rev out
  | _ -> 
    match execution_mot a texte with
    | (-1, _, s) -> print_string "Le lexème '"; print_string (String.of_seq(List.to_seq (List.rev s))); print_string " n'est pas un lexème reconnu"; failwith ""
    | (x, q, s) ->
      let s = (String.of_seq(List.to_seq (List.rev s))) in 
      match a.fin.(x) with
      | None -> print_string "Le lexème '"; print_string s; print_string " n'est pas un lexème reconnu"; failwith ""
      | Some (Name _) -> exec a q (Name s::out) 
      | Some (Integer _) -> exec a q (Integer s::out)
      | Some (Floating _) -> exec a q (Floating s::out)
      | Some (Chaine _) -> exec a q (Chaine s::out)
      | Some (Commentaire _ ) -> exec a q (Commentaire s::out)
      | Some t -> exec a q (t::out)
;;

end 

