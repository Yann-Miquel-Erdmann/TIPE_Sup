open Tokens

type regcomponent =
  (* teste si l'entier associé au caractère donné est entre le premier entier et le dernier entier inclus *)
| Range of (int*int) list 

  (* teste un par un les cractères donnés à ceux de la liste de caractère et garde l'index dans l'entier *)
| Litteral of char list*int 

  (* teste le recomponent tant qu'il peut être comparé et stocke le nombre de fois dans l'entier *)
| UnPlus of regcomponent*int 

  (* Même chose *)
| ZeroPlus of regcomponent*int 

  (* correspond à n'importe quel caractère *)
| AllChars 
;;

type regex = regcomponent list;;

(* à chacun de ses types est associé
    - un booléen qui indique si l'automate a fini et réussi
    - un token, un composant de syntaxe qui sera converti vers le language suivant
*)
type automaton =
  (* teste un par un les cractères donnés à ceux de la chaine de caractère, garde l'index dans l'entier *)
| N of string*int*token*bool 

  (* teste un par un les caractères donnés sur ceux du regex, garde l'index dans l'entier 1er entier
     stocke l'ensemble des cacarères dans la liste de chaine de caractères
     le dernier entier sert à savoir quel est l'index du regex a tester
  *)
| C of regex*int*token*int*bool*char list 
;;

(* l'entier sert à savoir quel est l'index du caractère dans le mot
   le booléen sert pour savoir si l'on est à la fin de la ligne
*)
type search = int*bool;;

(* renvoie l'élément de la liste l à l'indice i *)
let rec index_list (l: 'a list) (i: int) : 'a =
  match l with
  | [] -> failwith "Invalid index"
  | x::q -> if i == 0 then x else index_list q (i-1)
;;

(* remplace l'élément de la liste l à l'indice i par l'élément e *)
let replace_index (l: 'a list) (i: int) (e:'a) : 'a list =
  let rec replace_aux (l: 'a list) (i: int) (e:'a) (out:'a list) =
    match l, i with
    | [], -1 -> List.rev out
    | [], _ -> failwith "invalid index"
    | x::q, 0 -> replace_aux q (i-1) e (e::out)
    | x::q, -1 -> replace_aux q i e (x::out)
    | x::q, _ -> replace_aux q (i-1) e (x::out)
  in replace_aux l i e []
;;

(* affiche le contenu de la liste c *)
let print_list (c: char list) : unit =
  print_char '[';
  let rec print_list_aux (c:char list) =
    match c with
    | [] -> ()
    | x::[] -> print_char x;
    | x::q -> print_char x; print_char ','; print_list_aux q;
  in print_list_aux c;
  print_char ']';
  print_newline ()
;;

(* converti la chaine de cacatère s à partir de l'index index et l'ajoute à la liste c *)
let rec string_to_char_2 (s:string) (c : char list) (index: int): char list =
  if index == String.length s then
    List.rev c
  else
    string_to_char_2 s (s.[index]::c) (index + 1)
;;

(* génère *)
let rec gen_list (s: char list) (out: (int*int) list) ((buffer, b): char option*bool): (int*int) list =
  match s, buffer, b with
  | [], _, _ -> out
  | '-'::q, None, _ -> failwith "Invalid type"
  | '-'::q, Some c, false -> gen_list q out (Some c, true)
  | x::q, None, _ -> gen_list q out (Some x, false)
  | x::q, Some c, true -> gen_list q ((int_of_char x, int_of_char c)::out) (None, false)
  | x::q, Some c, false -> failwith "Invalid type"
;;

let rec reset_regex (reg:regex) : regex =
  reset reg []
and reset (reg: regex) (out:regex) = 
  match reg with
  | [] -> List.rev out
  | (Litteral (s, _))::q -> reset q (Litteral (s, 0)::out)
  | (UnPlus (r, i))::q -> reset q ((UnPlus (index_list (reset_regex [r]) 0, 0))::out)
  | (ZeroPlus (r, i))::q -> reset q ((ZeroPlus (index_list (reset_regex [r]) 0, 0))::out)
  | x::q -> reset q (x::out)
;;

let rec gen_regex (str:string) : regex =
  let rec gen_regex_aux (c:char list) (reg: regex) (buffer: char): regex =
    match c, buffer, reg with
    | [], _, _ ->
      begin
        let rec reverse (l:regex) (out:regex) : regex =
          match l with
          | [] -> out
          | (Litteral (x, i))::q -> reverse q (Litteral ((List.rev x), i)::out)
          | x::q -> reverse q (x::out)
        in reverse reg []
      end
    | 'n'::q, '\\', Litteral (l, i)::q2 -> gen_regex_aux q (Litteral('\n'::l, i)::q2) ' '
    | 'n'::q, '\\', _ -> gen_regex_aux q (Litteral(['\n'], 0)::reg) ' '
    | x::q, '\\', Litteral (l, i)::q2 -> gen_regex_aux q (Litteral(x::l, i)::q2) ' '
    | x::q, '\\', _ -> gen_regex_aux q (Litteral([x], 0)::reg) ' '
    | ']'::q, '[', Litteral(l, -1)::q2 -> gen_regex_aux q (Range (gen_list l [] (None, false))::q2) ' '
    | ']'::q, '[', _ -> failwith "[] need at least one range"
    | x::q, '[', Litteral (l, -1)::q2 -> gen_regex_aux q (Litteral(x::l, -1)::q2) '['
    | x::q, '[', _ -> gen_regex_aux q (Litteral([x], -1)::reg) '['
    | x::q, _ , Litteral (l, -1)::q2 -> failwith "[ not matched"
    | ']'::q, _, _ -> failwith "missing ["
    | '\\'::q, _, _ -> gen_regex_aux q reg '\\'
    | '.'::q, _, _ -> gen_regex_aux q (AllChars::reg) ' '
    | '*'::q, _, (UnPlus _)::q2 | '*'::q, _, (ZeroPlus _)::q2 | '+'::q, _, (UnPlus _)::q2 | '+'::q, _, (ZeroPlus _)::q2 -> failwith "Pas possibilité de mettre deux boucles à la suite"
    | '+'::q, _, Litteral (l, i)::q2 ->
      begin
        match l with
        | [] -> failwith "should not happend"
        | [x] -> gen_regex_aux q (UnPlus (Litteral(l, i), 0)::q2) ' '
        | x::q3 -> gen_regex_aux q (UnPlus (Litteral([x], i), 0)::(Litteral (q3, i)::q2)) ' '
      end
    | '*'::q, _, Litteral (l, i)::q2 ->
      begin
        match l with
        | [] -> failwith "should not happend"
        | [x] -> gen_regex_aux q (ZeroPlus (Litteral(l, i), 0)::q2) ' '
        | x::q3 -> gen_regex_aux q (ZeroPlus (Litteral([x], i), 0)::(Litteral (q3, i)::q2)) ' '
      end
    | '*'::q, _, comp::q2 -> gen_regex_aux q (ZeroPlus (comp, 0)::q2) ' '
    | '+'::q, _, comp::q2 -> gen_regex_aux q (UnPlus (comp, 0)::q2) ' '
    | '['::q, _, _ -> gen_regex_aux q reg '['
    | x::q, _, Litteral (l, i)::q2 -> gen_regex_aux q (Litteral(x::l, i)::q2) ' '
    | x::q, _, _ -> gen_regex_aux q (Litteral([x], 0)::reg) ' '
  in gen_regex_aux (string_to_char_2 str [] 0) [] ' '
;;

let rec match_regex (a:automaton) (s:search) (c: char) : automaton =
  match a, s with
  | N _, (index, _)-> a
  | C (reg, -2, t, index, b, l), (indexfin, _) ->
    begin
      if index >= List.length reg then
        C(reg, indexfin, t, index, true, l)
      else 
        begin
        match index_list reg index with
        | Litteral (l2, i) ->
            if index_list l2 i == c then
              if (i+1) == List.length l2 then
                C (replace_index reg index (Litteral (l2, (i+1))), -2, t, index+1, true, c::l)
              else
                C (replace_index reg index (Litteral (l2, (i+1))), -2, t, index, false, c::l)
            else
              C (reg, indexfin, t, index, false, l)
        | Range l2 ->
          begin
            let rec checkrange (l:(int*int) list) : bool =
              match l with
              | (n1, n2)::q -> if n1 <= (int_of_char c) && n2 >= (int_of_char c) then true else checkrange q
              | [] -> false
            in if checkrange l2 then C(reg, -2, t, index+1, b, c::l) else C(reg, indexfin, t, index, false, l)
          end
        | AllChars -> C(reg, -2, t, index+1, false, c::l)
        (* deux cas suivants à finir *)
        | UnPlus (r, count) -> begin 
          (* check the next one first*) (* for the unplus, i'll have to check at least one char before trying the first one*)
          let result, a2 =
            if index+1 >= List.length reg || count == 0 then
              (* rien ne se passe *)
              0, AllChars
            else
              (* tester le suivant *)
              let a2 = match_regex (C([index_list reg (index+1)], -2, t, 0, false, [])) s c in
              match a2 with
              | N _ -> failwith "impossible"
              | C (r, -2, _, _, false, _) -> 1, index_list r 0
              | C (r, _, _, _ , true, [x]) -> 2,  index_list r 0
              | C (r, _, _, _ , _, _) -> 0, index_list r 0
          in
            if result == 0 then
              let a2 = match_regex (C([r], -2, t, 0, b, l)) s c in
              match a2 with
              | N _ -> failwith "impossible"
              | C(r2, -2, t2, i, b2, l2) ->
                if i > 0 then
                  begin
                  (* recommencer au début *)
                  C(replace_index reg index (UnPlus ((index_list (reset_regex r2) 0), (count+1))), -2, t, index, b, l2)
                  end
                else
                  begin
                  (* continuer sur le même *)
                  C(replace_index reg index (UnPlus ((index_list r2 0), count)), -2, t, index, b, l2)
                  end
              | C(_, failindex, _, i, _, l2) ->
                if count == 0 then 
                  (* fail *)
                  C(reg, indexfin, t, index, false, l2)
                else
                  (* passer au suivant *)
                  C (reg, -2, t, index+1, b, l2) 
            else
              (* tester le suivant ici *)
              let reg = replace_index reg (index+1) a2 in
              if result == 1 then
                C(replace_index reg index (UnPlus (r, count)), -2, t, index+1, b, c::l) 
              else
                C(replace_index reg index (UnPlus (r, count)), -2, t, index+2, b, c::l)
            end 
        | ZeroPlus (r, count) -> 
          let result, a2 = if index+1 >= List.length reg then
              0, AllChars
            else 
            (* tester suivant *)
            let a2 = match_regex (C([index_list reg (index+1)], -2, t, 0, b, [])) s c in
            match a2 with
            | N _ -> failwith "impossible"
            | C (r, -2, _, _, false, _) -> 1, index_list r 0
            | C (r, _, _, _, true, [x]) -> 2,  index_list r 0
            | C (r, _, _, i, b, l) -> 0, index_list r 0
          in
          if result == 0 then
            let a2 = match_regex (C([r], -2, t, 0, b, l)) s c in
            match a2 with
            | N _ -> failwith "impossible"
            | C(r2, -2, t2, i, b2, l2) ->
              if i > 0 then
                begin
                (* recommencer au début *)
                C(replace_index reg index (ZeroPlus ((index_list (reset_regex r2) 0), (count+1))), -2, t, index, b, l2)
                end
              else
                begin
                (* continuer sur le même *)
                C(replace_index reg index (ZeroPlus ((index_list r2 0), count)), -2, t, index, b, l2)
                end
            | C(_, failindex, _, i, _, l2) ->
              (* passer au suivant *)
              C (reg, -2, t, index+1, b, l2) 
          else
            (* tester le suivant ici *)
            let reg = replace_index reg (index+1) a2 in
            if result == 1 then
              C(replace_index reg index (ZeroPlus (r, count)), -2, t, index+1, b, c::l) 
            else
              C(replace_index reg index (ZeroPlus (r, count)), -2, t, index+2, b, c::l)
        end
    end
  | C _, _ -> a

;;

gen_regex "[a-zA-Z0-9]+";;
gen_regex "[0-9]+\\.[0-9]+";;
gen_regex "\".*\"";;
gen_regex "'.*'";;
gen_regex "[0-9]+";;
gen_regex "!.*\n";;
let num_reg = gen_regex "\".*\"";;

let num = match_regex (C(num_reg, -2, Integer [], 0, false, [])) (0, false) '"'
let num = match_regex num (1, false) 'a'

let num = match_regex num (2, false) 'a'

let num = match_regex num (3, false) 'a'

let num = match_regex num (4, false) '\''

let num = match_regex num (5, false) '"'
