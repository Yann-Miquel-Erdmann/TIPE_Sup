open Regex2
open Tokens
open Vector

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

type automate = {
  nodes : int list;
  debut_l : int list;
  fin : (int * token) list;
  trasitions : (int*char option*int)list;
  (* expr_reg : regex;
  current_node : int;
  lexeme: token; *)

  (* recherche d'un mot :
      * on commence de l'état initial (il ne devrais en y avoir qu'un après la déterminisation)
      * on consomme tous les caractères jusqu'a ce que l'on rencontre un état final
      * on le note comme dernier état final
      * si on tombe dans l'état de puit, on renvoi le dernier état final => trouver comment déterminer si dans puit
  *)
}

type automate_v2 = {
  nodes : int list;
  debut_l : int list;
  fin : (int * token) list;
  transitions_ : (char option*int) list array; 
}

type automate_sans_eps = {
  nodes : int list;
  debut_l : int list;
  fin : (int * token) list;
  transitions_sans_eps : (int*char*int)list;
}

type automate_sans_eps_v2 = {
  nodes : int list;
  debut_l : int list;
  fin : (int * token) list;
  transitions_sans_eps_ : (char*int) list array;  (* arr.(i).(j), i le sommet de départ, j l'entier du caractère *)
}

type automate_det = {
  nodes : int list;
  debut : int;
  fin : (int * token) list;
  transitions : (int*char*int)list;
}

type automate_det_v2 = {
  mutable nodes : int list;
  debut : int;
  mutable fin : (int * token) list;
  mutable transitions_ : (int array) Vector.t; (* arr.(i).(j), i le sommet de départ, j l'entier du caractère *)
}

let alphabet_to_list (arr : bool array) : char list =
  let l = ref [] in
  for i = 0 to (Array.length arr -1) do
    if arr.(i) then l := (char_of_int i)::!l
  done;
  !l
;;

let range_list (n : int) : int list =
  let l = ref [] in
  for i = 0 to n-1 do
    l := i::!l
  done;
  !l
;;

let automate_gen (reg : regex) (t : token):  automate_v2 =
  let dico = Hashtbl.create 0 in
  let a = ref {
    nodes = [];
    debut_l = [0];
    fin = [(1, t)];
    transitions_ = [||];
  } in
  let add_transition ((n1, c, n2) : int * char option * int) =
    if Hashtbl.mem dico n1 then
      Hashtbl.replace dico n1 ((c, n2)::(Hashtbl.find dico n1))
    else
      Hashtbl.add dico n1 [(c, n2)];
  in
  let next_node = ref 2 in
  let rec automate_gen_aux (reg : regex) ((node_before, node_after) : int * int): unit =
    match reg with
    | Vide -> ()
    | Epsilon -> add_transition (node_before, None, node_after);
    | AllChars -> 
      for i = 0 to 127 do
        add_transition (node_before, Some (char_of_int i), node_after)
      done;
    | Caractere x -> add_transition (node_before, Some x, node_after);
    | Concat (g, d) -> let node = !next_node in next_node := !next_node + 1; automate_gen_aux g (node_before, node); automate_gen_aux d (node, node_after)
    | Ou (g, d) -> automate_gen_aux g (node_before, node_after); automate_gen_aux d (node_before, node_after)
    | Range (a1, a2) -> 
      begin
        for i = int_of_char a1 to int_of_char a2 do
          automate_gen_aux (Caractere(char_of_int i)) (node_before, node_after);
        done
      end
    | ZeroPlus e -> automate_gen_aux e (node_before, node_before); automate_gen_aux Epsilon (node_before, node_after)
    | UnPlus e -> automate_gen_aux e (node_before, node_before); automate_gen_aux e (node_before, node_after)
    | Facultatif e -> automate_gen_aux e (node_before, node_after); automate_gen_aux Epsilon (node_before, node_after)
  in automate_gen_aux reg (0, 1);

  let l1 = List.of_seq (Hashtbl.to_seq_keys dico) in
  let rec build_trans (l : int list) (arr : (char option * int) list array) : unit =
    match l with
    | [] -> ()
    | x::q -> arr.(x) <- (Hashtbl.find dico x); build_trans q arr
  in 
  let n = ((List.fold_left max min_int l1) + 1) in
  let arr = Array.make n [] in
  build_trans l1 arr;
  {
    nodes = range_list !next_node;
    debut_l = !a.debut_l;
    fin = !a.fin;
    transitions_ = arr
  }
;;

let ou_automates (l_a : automate_v2 list) : automate_v2 =
  let rec ou_automate_aux (l : automate_v2 list) (out : automate_v2) =
    match l with
    | [] -> out
    | x::q ->
      let inc = List.length out.nodes in
      let x2 = 
        {
          nodes        = List.map ((+) inc) x.nodes;
          debut_l      = List.map ((+) inc) x.debut_l;
          fin          = List.map (fun (x, y) -> (x+inc , y)) x.fin;
          transitions_ = [||];
        }
      in
      ou_automate_aux q
      {
        nodes         = out.nodes @ x2.nodes;
        debut_l       = out.debut_l @ x2.debut_l;
        fin           = out.fin @ x2.fin;
        transitions_  = Array.init (inc + (Array.length x.transitions_)) (fun i -> if i < inc then out.transitions_.(i) else List.map (fun (c, x) -> c, x + inc) x.transitions_.(i-inc));
      }
  in
  let a = ou_automate_aux l_a
  {
    nodes         = [];
    debut_l       = [];
    fin           = [];
    transitions_  = [||];
  } in
  let nouv_d = List.length a.nodes in
  let rec ajouter_debut (l : int list) (out : (char option*int) list array): (char option*int) list array =
    Array.append out [|List.map (fun x -> (None, x)) l|]
  in 
  {
    nodes         = a.nodes;
    debut_l       = [nouv_d];
    fin           = a.fin;
    transitions_  = ajouter_debut a.debut_l a.transitions_;
  }
;;

(*
  Enlever les epsilon-transitions:
  - On prend un sommet
  - On regarde toutes les epsilon-transitions sortantes (pas les boucles)
  - On regarde tous les transitions entrantes
  - On ajoute des nouvelles transions des entrantes vers les sortantes
  - On supprime l'epsilon-transition

  Requis:
  - transitions entrantes de chaque sommet => précalculé
*)

let enleve_epsilon_trans (a : automate_v2) : automate_sans_eps_v2 = 
  let len = (List.length a.nodes) in
  (* on stocke les degrés entrants et sortants de chaque sommet *)
  (* les éléments stockés ne sont pas linéarisés *)
  let entrants = Array.make len [] in

  (* degré entrant de chaque sommet *)
  let degres = Array.make len 0 in

  let remove_duplicates (l : 'a list) : 'a list =
    let tbl = Hashtbl.create 0 in
    let rec aux (l : 'a list) (out : 'a list): 'a list =
      match l with
      | [] -> out
      | x::q -> if Hashtbl.mem tbl x then aux q out else (Hashtbl.add tbl x 0; aux q (x::out))
    in aux l []
  in

  let trans_temp = ref a.transitions_ in
  let fin_temp = ref a.fin in
  let deg_traite = ref 0 in

  (* trouve le premier sommet de degré zéro *)
  let rec find_premier_deg_zero () : int  =
    let deg = !deg_traite in
    if degres.(!deg_traite) <= 0 then
      incr deg_traite;
    while !deg_traite < len && degres.(!deg_traite) > 0 do
      incr deg_traite
    done;
    if deg == !deg_traite then (incr deg_traite; find_premier_deg_zero()) else 
    if !deg_traite >= len then -1 else !deg_traite
  in

  (* construit les listes d'entrants de chaque sommet *)
  Array.iteri (fun e l -> List.iter (fun (c, s) -> entrants.(s) <- (e, c)::entrants.(s); degres.(s) <- degres.(s) + 1) l)  a.transitions_;
  (* ajouter un de degré entrant pour chaque entrée *)
  List.iter (fun x -> degres.(x) <- degres.(x) + 1) a.debut_l;
  
  (* applique l'algorithme de supression des epsilon transitions sur chaqun des sommets *)
  List.iter (fun node_i ->
    print_string "treating node "; print_int node_i; print_newline();
    (* récupération les transitions entrantes avec epsilon transitions qui ne sont pas des boucles + enlever du degré entrant pour chaque epsilon transition *)
    let res = remove_duplicates(List.fold_left (
      fun acc x ->
        match x with
        | (_, Some x) -> acc
        | (node , None) ->
          degres.(node_i) <- degres.(node_i) -1 ;
          if node == node_i then
            acc
          else
            node::acc
    ) [] entrants.(node_i)) in
    print_int node_i; print_string " has degre "; print_int degres.(node_i); print_newline();
    List.iter (fun (a, c) -> print_int node_i; print_string " has entring "; print_int a; print_string " with "; print_char (match c with | None -> '#' | Some c -> c); print_newline();) entrants.(node_i);
    List.iter (fun (c, a) -> print_int node_i; print_string " has exiting "; print_int a; print_string " with "; print_char (match c with | None -> '#' | Some c -> c); print_newline();) !trans_temp.(node_i);
    (* on enlève les epsilon entrants de chaque epsilon transition *)
    entrants.(node_i) <- List.filter (fun (_, c) -> c != None) entrants.(node_i);
    (* afficher les entrants de node_i *)
    List.iter (fun (x, c) -> print_int x; print_string " -> "; print_int node_i; print_char ' '; (match c with | None -> print_char '#' | Some c -> print_char c); print_newline()) entrants.(node_i);
    (* on enlève les sortants de chaque epsilon transition *)
    !trans_temp.(node_i) <- List.filter (fun (c, x) -> if (c == None && x == node_i) then (print_string "removed "; print_int x; print_char '#'; print_int x; print_newline(); false) else true) !trans_temp.(node_i);
    List.iter (fun x -> !trans_temp.(x) <- List.filter (fun (c, x1) -> if c == None && x1 == node_i then (print_string "removed "; print_int x; print_char '#'; print_int x1; print_newline(); false) else (print_int x; print_string " >> "; print_int x1; print_char ' '; (match c with | None -> print_char '#' | Some c -> print_char c); print_newline(); true)) !trans_temp.(x)) res;
    (* ajoute les transitions des entrants avec epsilon transitions vers les sortants et actualise les entrants/sortants/degré de deux sommets *)
    List.iter (
      fun x ->
        List.iter (
          fun (c, node) ->
            print_string "adding "; print_int (x); (match c with | None -> print_char '#' | Some c -> print_char c); print_int (node); print_newline();
            !trans_temp.(x) <-  (c, node)::!trans_temp.(x);
            entrants.(node) <- (x, c)::entrants.(node);
            degres.(node) <- degres.(node) + 1
      ) (List.filter (fun (c, n) -> not (n == node_i && c = None)) !trans_temp.(node_i));
    ) res;
    
    (* récupération des états de fin du sommet de départ *)
    let fins = List.fold_left (fun acc (x, t) -> if x == node_i then t::acc else acc) [] a.fin in
    
    (* applique l'ensemble des états de fin aux sommets qui avaient une epsilon transition *)
    List.iter (fun x -> List.iter (fun t -> print_int x; print_string " is now final"; print_newline (); fin_temp :=  (x, t)::!fin_temp) fins) res
  ) a.nodes;
  
  (* enlever les nodes qui ne sont plus atteintes *)
  let todo = ref (find_premier_deg_zero()) in
  while !todo <> -1 do
    List.iter (fun (_, x) -> degres.(x) <- degres.(x) -1) !trans_temp.(!todo);
    todo := find_premier_deg_zero ();
  done;

  (* enlever les option car toutes les transition (devraient être) sans epsilon transition *)
  {
    nodes = List.filter (fun x -> degres.(x) > 0) a.nodes;
    debut_l = a.debut_l;
    fin = List.filter (fun (x, _) -> degres.(x) > 0) !fin_temp;
    transitions_sans_eps_ = Array.map (fun x -> List.map (fun (c, n) -> match c with | None -> failwith "pas correct" | Some c1 -> (c1, n)) (List.filter (fun (_, n) -> degres.(n) > 0) x)) !trans_temp;
    (*transitions_sans_eps_ = List.map (fun (n1, c) -> match c with | None -> failwith "pas correct" | Some c1 -> (lin n1, c1, lin n2)) (List.filter (fun (_, _, x) -> degres.(x) > 0) !trans_temp);*)
  } 
;;


let print_transitions (vect : (int array) Vector.t) : unit =
  Vector.iteri (fun i a -> Array.iteri (fun j x -> if x <> -1 then (print_int i; print_string " -> "; print_int x; print_string ", "; print_char (char_of_int j); print_newline())) a) vect
;;

let lin_v2 (elem : IntSet.t) (lin_tbl : (IntSet.t, int) Hashtbl.t) (delin_tbl : IntSet.t Vector.t) : int=
  if not (Hashtbl.mem lin_tbl IntSet.empty) then
    Hashtbl.add lin_tbl IntSet.empty 0;

  if (Hashtbl.find lin_tbl IntSet.empty <> Vector.length delin_tbl) then
    failwith "The size of the two tables are not matching";

  if not (Hashtbl.mem lin_tbl elem) then
    (Hashtbl.add lin_tbl elem (Hashtbl.find lin_tbl IntSet.empty);
    Hashtbl.replace lin_tbl IntSet.empty (Hashtbl.find lin_tbl IntSet.empty + 1);
    Vector.push delin_tbl elem);
  Hashtbl.find lin_tbl elem
;;

let delin_v2 (elem : int) (delin_tbl : IntSet.t Vector.t) : IntSet.t =
  if elem = -1 then
    IntSet.empty
  else if elem >= Vector.length delin_tbl then
    failwith "Element was not found in the table"
  else
    Vector.get delin_tbl elem
;;

let determinise_v2 (a : automate_sans_eps_v2) : automate_det_v2 =
  let lin_tbl = Hashtbl.create (List.length a.nodes) in
  let delin_tbl = Vector.create ~dummy:IntSet.empty in
  
  let start_node = lin_v2 (IntSet.of_list a.debut_l) lin_tbl delin_tbl in 
  let todo = ref [start_node] in
  
  let a_det = {
    nodes = [start_node];
    debut = start_node;
    fin = [];
    transitions_ = Vector.create ~dummy:(Array.make 1 0);
  } in

  Vector.push a_det.transitions_ (Array.make 128 (-1)); (* pour le début *)
  let fin  = IntSet.of_list (List.map (fun (a, b) -> a) a.fin) in 
  (*let fin = lin_v2 () lin_tbl delin_tbl in
  a_det.nodes <- fin::a_det.nodes;
  Vector.push a_det.transitions_ (Array.make 128 (-1)); (* pour la fin *)*)

  (* l est un set des sommets dont il faut trouver les sommets et les place en fonction de leur étiquette de transition dans arr *)
  let rec trouver_suivants (l : IntSet.t) (arr: int array) : unit =
    let len = Array.length arr in

    (* on rassemble les éléments accessibles depuis tous les sommets de l*)
    let storage = Array.make len IntSet.empty in
    IntSet.iter (fun x -> List.iter (fun (c, e) -> storage.(int_of_char c) <- IntSet.add e storage.(int_of_char c)) a.transitions_sans_eps_.(x)) l;
    
    (* on linéarise les sommets obtenus et on les stocke dans arr *)
    for i = 0 to (len-1) do
      if not (IntSet.is_empty storage.(i)) then
        arr.(i) <- lin_v2 storage.(i) lin_tbl delin_tbl
    done;
  in

  (* teste si le sommet elem linéarisé contient des éléments finaux et l'ajoute aux finaux si c'est le cas *)
  let ajouter_fin (elem : int) : unit =
    let res = IntSet.to_list (IntSet.inter fin (delin_v2 elem delin_tbl)) in
      match res with
      | [] -> ()
      | [e] -> a_det.fin <- (elem, List.assoc e a.fin)::a_det.fin
      | [e1; e2] -> 
        let token1 = List.assoc e1 a.fin in
        let token2 = List.assoc e2 a.fin in
        begin
          match token1, token2 with
          | Name _, token | token, Name _ ->
            (* L'un des deux est une variable, elle peut être vue autrement donc elle est ignorée *)
            a_det.fin <- (elem, token)::a_det.fin
          | _ -> failwith "A syntax can't have more than one output" (* il a plus d'un élément final *)
        end
      | _ -> failwith "A syntax can't have more than one output"
  in

  let finished = ref false in
  while not !finished do
    match !todo with
    | [] -> finished := true
    | x::q ->
      begin
        print_int x; print_newline();
        let init_len = Hashtbl.find lin_tbl IntSet.empty in 
        todo := q;
        let suivants = Array.make 128 (-1) in
        trouver_suivants (delin_v2 x delin_tbl) suivants;
        
        let arr = Vector.get a_det.transitions_ x in
        for i = 0 to 127 do
          if suivants.(i) <> -1 then
            (if (suivants.(i) >= init_len) then
              (* nouveau noeud, on ajoute a la liste de traitement,
              on l'ajoute dans l'automate et on vérifie s'il est final *)
              (todo := suivants.(i)::!todo;
              Vector.push a_det.transitions_ (Array.make 128 (-1));
              a_det.nodes <- suivants.(i)::a_det.nodes);
            (* noeud déjà existant/complétion nouveau noeud, comme on ne traite qu'une fois chaque sommets,
            on sait que les sommets trouvé sont les bons, on les remplace *)
            arr.(i) <- suivants.(i))
        done;
        Vector.set a_det.transitions_ x arr
      end
  done;

  (* gérer le cas des mots vides, qui sont donc à la fois initiaux et finaux *)
  List.iter ajouter_fin a_det.nodes;
  a_det
;;

let exec_char (a : automate_det_v2) (node : int) (c : char) : int =
  (Vector.get a.transitions_ node).(int_of_char c)
;;

let execution_mot (a : automate_det_v2) (texte : char list) : int * char list =
  let node = ref a.debut in
  let last_found = ref (-1) in
  let text_as_last = ref texte in
  let texte = ref texte in
  let text_read = ref [] in
  while !node != -1 do
    match !texte with
    | [] -> node := -1 (* forcer la fin de la boucle*)
    | c::q ->
      begin
        text_read := c::!text_read;
        node := exec_char a !node c;
        texte := q;
        if (List.exists ((==) !node) (List.map (fun (x, y) -> x) a.fin)) then
          begin 
            last_found := !node;
            text_as_last := !texte
          end;
      end;
  done;
  !last_found, !text_as_last
;;

let rec exec (a : automate_det_v2) (texte : char list) (out : token list): token list =
  match texte with
  | [] -> out
  | _ -> 
    match execution_mot a texte with
    | (-1, _) -> failwith "lexème non reconnu" 
    | (x, q) ->
      match (List.fold_left (fun a (y,t) -> if x == y then t::a else a) [] a.fin) with
      | [] -> failwith "lexème non reconnu"
      | [t] -> exec a q (t::out)
      | _ -> failwith "impossible happened"
;;

let a = {
  nodes = [0; 1; 2; 3];
  debut_l = [0];
  fin = [(1, NewLine); (3, Parenthesefermante)];
  trasitions = [(0, Some 'a', 1); (0, Some 'a', 2); (2, Some 'b', 3)];
}

let a1 = {
  nodes = [0; 1];
  debut_l = [0];
  fin = [(1, NewLine)];
  trasitions = [(0, None, 1); (1, Some 'a', 1)]
}

let a2 = {
  nodes = [0; 1];
  debut_l = [0];
  fin = [(1, NewLine)];
  trasitions = [(0, None, 1); (1, None, 1)]
}

let (a2_2 : automate_v2) = {
  nodes = [0; 1; 2];
  debut_l = [0];
  fin = [(2, NewLine)];
  transitions_ = [|[(Some 'a', 1)]; [(Some 'a', 1); (Some 'a', 2)]; []|]; 
}

let (a2_2se : automate_sans_eps_v2) = enleve_epsilon_trans a2_2;;