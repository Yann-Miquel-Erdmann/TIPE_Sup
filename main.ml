
type data_type = 
  (* types *)
  | Entier of int
  | Flotant of float
  | Imaginaire of float*float
  | Booleen of bool
  | Caractere of string

type operateur = 
  | Plus
  | Moins
  | Fois
  | Division
  | Puissance

type comparateur =  
  | PlusPetit
  | PlusGrand
  | StrictPlusPetit
  | StrictPlusGrand
  | Egal
  | NonEgal

type operateur_logique = 
  | Et
  | Ou
  | Non
  | Equivalent
  | NonEquivalent



type syntax = 
  | Allocatable	
  | Allocate	
  | Assign	
  | Assignment	
  | Block_data
  | End_block_data	
  | Call	
  | Case	
  | Character	
  | Common	
  | Complex	
  | Contains	
  | Continue	
  | Cycle	
  | Data	
  | Deallocate	
  | Default	
  | Do	
  | End_do  
  | Double_precision
  | Else	
  | Else_if
  | Elsewhere	
  | Entry	
  | Equivalence	
  | Exit	
  | External	
  | Function	
  | End_function  
  | Go_to
  | If	
  | End_if  
  | Implicit	
  | In	
  | Inout	
  | Integer	
  | Intent	
  | Interface	
  | End_interface  
  | Intrinsic	
  | Kind	
  | Len	
  | Logical	
  | Module	
  | End_module  
  | Namelist	
  | Nullify	
  | Only	
  | Operator	
  | Optional	
  | Out	
  | Parameter	
  | Pause	
  | Pointer	
  | Print
  | Private	
  | Program	
  | End_program  
  | Public	
  | Real	
  | Recursive	
  | Result	
  | Return	
  | Save	
  | Select_case	
  | End_select  
  | Stop	
  | Subroutine	
  | End_subroutine
  | Target	
  | Then	
  | Type	
  | End_type  
  | Use	
  | Where	
  | End_where  
  | While

type intrinsic_function =
  | Abs
  | AChar
  | ACos
  | AImag
  | All
  | Allocatable
  | Allocate
  | AnInt
  | Any
  | ASin
  | Associated
  | ATan
  | ATan2
  | Bessel_J0
  | Bessel_J1
  | Bessel_JN
  | Bessel_Y0
  | Bessel_Y1
  | Bessel_YN
  | Bit_Size
  | BTest
  | Cabs
  | Ceiling
  | Char
  | Cmplx
  | Conjg
  | Cos
  | Cosh
  | Count
  | Cshift
  | Dble
  | DChar
  | DImag
  | Dot_Product
  | DProd
  | DReal
  | Epsilon
  | Exp
  | Exponent
  | FDate
  | FFlush
  | FGet
  | Floor
  | FPut
  | FSeek
  | FStat
  | FTell
  | FWrite
  | GError
  | GSet
  | HostNm
  | Hypot
  | IAbs
  | IAnd
  | IBClr
  | IBits
  | IChar
  | Ieor
  | IErr
  | Image
  | Index
  | Int
  | Int2
  | Int8
  | IOr
  | ISHFT
  | ISHFTC
  | ISign
  | Kind
  | LBound
  | Len
  | Len_Trim
  | LGe
  | LGt
  | LLe
  | LLt
  | Log
  | Log10
  | Log_Gamma
  | Matmul
  | Max
  | Maxloc
  | Maxval
  | Merge
  | Min
  | Minloc
  | Minval
  | Mod
  | Modulo
  | Move_Alloc
  | NChar
  | Nearest
  | New_Line
  | NInt
  | Not
  | Null
  | Or
  | Pack
  | Precision
  | Present
  | Product
  | Radix
  | Random_Number
  | Random_Seed
  | Range
  | Real
  | Repeat
  | Reshape
  | RRSpacing
  | RShift
  | Same_Type_As
  | Scale
  | Scan
  | Selected_Char_Kind
  | Selected_Int_Kind
  | Selected_Real_Kind
  | Set_Exponent
  | Shape
  | Sign
  | Sin
  | Sinh
  | Size
  | Sngl
  | Sqrt
  | Spread
  | Sum
  | System_Clock
  | Tan
  | Tanh
  | Tiny
  | Transfer
  | Transpose
  | Trim
  | UBound
  | Unpack
  | Verify
  | Xor

type token = 
  | DataType of data_type
  | Operateur of operateur
  | Comparateur of comparateur
  | OperateurLogique of operateur_logique
  | Syntax of syntax
  | IntrinsicFunction of intrinsic_function
  | Ident of string
  | NewLine

type automaton =
| N of string*int*token*bool (* N for normal, searches for the string *)
| C of string*int*bool (* C for complex, searches the regular expression ex : (1|2|3|4|5|6|7|8|9|0) *)
;;

type dico = automaton list;;
type search = int*int;; (* first for text index the second for index in the current world and the last for the current word in dico *)

let file = open_in "test1.txt";;
let rec lire file liste = 
  let line = input_line file in
    print_string line;
    print_newline ();
  try lire file (line::liste) with End_of_file->
    close_in file;
    line::liste
;;

let rec is_one_alive (d: dico) : bool =
  match d with
  | [] -> true
  | N(_, i, _, _)::q-> i == -2 || is_one_alive q
  | C(_, i, _)::q -> i == -2 || is_one_alive q
;;

let print_bool b =
  if b then print_string "vrai"
  else print_string "false"
;;
let search (d:dico) (s:search) (c: char): dico =
  let rec sub_search d s c d2 = 
    match d, s with
    | [], _ -> d2
    | N(s1, -2, t, false)::q, (_, index)->
      if (s1.[index] != c) then
        sub_search q s c (N(s1, index-1, t, false)::d2)
      else 
        if ((index + 1) == String.length s1) then 
          sub_search q s c (N(s1, index+1, t, true)::d2)
        else 
          sub_search q s c (N(s1, -2, t, false)::d2)

    | N(s1, i, t, b)::q, (_, index)-> sub_search q s c (N(s1, i, t, b)::d2)
    | x::q, _ -> sub_search q s c (x::d2)
  in sub_search d s c []
;;

let rec print_alive (d:dico) : unit =
  match d with
  | [] -> ();
  | N(s, -2, _, _)::q -> print_string s; print_string " is alive"; print_newline(); print_alive q;
  | _::q -> print_alive q;
;;
let autoN (s:string) (t: token): automaton = N(s, -2, t, false);;
let autoC (s:string): automaton = C(s, -2, false);;

let rec string_to_char (s:string) (c : char list) (index: int): char list =
  if index == String.length s then
    begin
      print_newline ();
      List.rev c
    end
  else
    if (List.length c == 0) && (int_of_char (s.[index]) == 32) then
      string_to_char s c (index+1)
    else
      begin
        print_char s.[index];
        if (int_of_char s.[index]) >= 65 && (int_of_char s.[index]) <= 90 then
          string_to_char s ((char_of_int((int_of_char(s.[index]))+32))::c) (index+1)
        else string_to_char s (s.[index]::c) (index + 1)
      end
;;

let texte = lire file [];;

let dico = [
  autoN "allocatable" (Syntax Allocatable);
  autoN "allocate" (Syntax Allocate);
  autoN "assign" (Syntax Assign);
  autoN "assignment" (Syntax Assignment);
  autoN "block data" (Syntax Block_data);
  autoN "end block data" (Syntax End_block_data);
  autoN "call" (Syntax Call);
  autoN "case" (Syntax Case);
  autoN "character" (Syntax Character);
  autoN "common" (Syntax Common);
  autoN "complex" (Syntax Complex);
  autoN "contains" (Syntax Contains);
  autoN "continue" (Syntax Continue);
  autoN "cycle" (Syntax Cycle);
  autoN "data" (Syntax Data);
  autoN "deallocate" (Syntax Deallocate);
  autoN "default" (Syntax Default);
  autoN "do" (Syntax Do);
  autoN "do end" (Syntax End_do);
  autoN "double precision" (Syntax Double_precision);
  autoN "else" (Syntax Else);
  autoN "else if" (Syntax Else_if);
  autoN "elsewhere" (Syntax Elsewhere);
  autoN "entry" (Syntax Entry);
  autoN "equivalence" (Syntax Equivalence);
  autoN "exit" (Syntax Exit);
  autoN "external" (Syntax External);
  autoN "function" (Syntax Function);
  autoN "end function" (Syntax End_function);
  autoN "go to" (Syntax Go_to);
  autoN "goto" (Syntax Go_to);
  autoN "if" (Syntax If);
  autoN "end if" (Syntax End_if);
  autoN "implicit" (Syntax Implicit);
  autoN "in" (Syntax In);
  autoN "inout" (Syntax Inout);
  autoN "integer" (Syntax Integer);
  autoN "intent" (Syntax Intent);
  autoN "interface" (Syntax Interface);
  autoN "end interface" (Syntax End_interface);
  autoN "intrinsic" (Syntax Intrinsic);
  autoN "kind" (Syntax Kind);
  autoN "len" (Syntax Len);
  autoN "logical" (Syntax Logical);
  autoN "module" (Syntax Module);
  autoN "end module" (Syntax End_module);
  autoN "namelist" (Syntax Namelist);
  autoN "nullify" (Syntax Nullify);
  autoN "only" (Syntax Only);
  autoN "operator" (Syntax Operator);
  autoN "optional" (Syntax Optional);
  autoN "out" (Syntax Out);
  autoN "parameter" (Syntax Parameter);
  autoN "pause" (Syntax Pause);
  autoN "pointer" (Syntax Pointer);
  autoN "print" (Syntax Print);
  autoN "private" (Syntax Private);
  autoN "program" (Syntax Program);
  autoN "end program" (Syntax End_program);
  autoN "public" (Syntax Public);
  autoN "real" (Syntax Real);
  autoN "recursive" (Syntax Recursive);
  autoN "result" (Syntax Result);
  autoN "return" (Syntax Return);
  autoN "save" (Syntax Save);
  autoN "select case" (Syntax Select_case);
  autoN "end select" (Syntax End_select);
  autoN "stop" (Syntax Stop);
  autoN "subroutine" (Syntax Subroutine);
  autoN "end subroutine" (Syntax End_subroutine);
  autoN "target" (Syntax Target);
  autoN "then" (Syntax Then);
  autoN "type" (Syntax Type);
  autoN "end type" (Syntax End_type);
  autoN "use" (Syntax Use);
  autoN "where" (Syntax Where);
  autoN "end where" (Syntax End_where);
  autoN "while" (Syntax While);
];;

let text = ['e'; 'n'; 'd'; ' '; 'w'];;

(* ne renvoie pas s pour le moment mais besoin pour plusieurs mots *)
let rec test str d s =
  match str, s with
  | [], _ -> d
  | x::q, (i1, i2) -> if is_one_alive d then let d2 = search d s x in test q d2 (i1+1, i2+1) else d
;;

(* checks for the automaton that died last *)
let rec last_alive (d:dico) (t_max:token) (max:int) : token option =
  match d with
  | [] -> if max == -1 then None else Some t_max
  | N(_, i, t, true)::q -> if i > max then last_alive q t i else last_alive q t_max max
  | _::q -> last_alive q t_max max
;;

let texte = List.rev texte

(* outputs the token list of the *)
let rec analyse (s: string list) (t: token list): token list =
  match s with
  | [] -> List.rev t
  | x::q ->
    let c = string_to_char x [] 0 in
      let d2 = (test c dico (0, 0)) in
        let token = last_alive d2 NewLine (-1) in
          match token with
          | None -> analyse q (NewLine::t)
          | Some token -> analyse q (NewLine::(token::t))
;;

analyse texte [];;