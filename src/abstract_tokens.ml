type operateur = Plus | Moins | Fois | Division | Puissance | Assignation

type comparateur =
  | PlusPetit
  | PlusGrand
  | StrictPlusPetit
  | StrictPlusGrand
  | Egal
  | NonEgal

type operateur_logique = Et | Ou | Non | Equivalent | NonEquivalent

type syntax =
  | Any
  | Character
  | Complex
  | Constant
  | Do
  | Double_precision
  | Else
  | Else_if
  | Function
  | If
  | End_if
  | Out
  | Integer
  | Logical
  | Program
  | Real
  | Subroutine
  | Then
  | While
  | Call
  | Print
  | Return
  | Size
  | For
  | Step

type token =
  | Operateur of operateur
  | Comparateur of comparateur
  | OperateurLogique of operateur_logique
  | Syntax of syntax
  | NewLine
  | Space
  | Name of string
  | Integer of string
  | Floating of string
  | Double of string
  | Chaine of string
  | Commentaire of string
  | Booleen of bool
  | Virgule
  | Parentheseouvrante
  | Parenthesefermante
  | ProgramRoot
  | ToFlatten

(* paramètres et enfants confondus dans la liste *)
type ast = Noeud of token * ast list

let string_of_token (t : token) : string =
  match t with
  | Operateur Plus -> "Operateur Plus"
  | Operateur Moins -> "Operateur Moins"
  | Operateur Fois -> "Operateur Fois"
  | Operateur Division -> "Operateur Division"
  | Operateur Puissance -> "Operateur Puissance"
  | Operateur Assignation -> "Operateur Assignation"
  | Comparateur PlusPetit -> "Comparateur PlusPetit"
  | Comparateur PlusGrand -> "Comparateur PlusGrand"
  | Comparateur StrictPlusPetit -> "Comparateur StrictPlusPetit"
  | Comparateur StrictPlusGrand -> "Comparateur StrictPlusGrand"
  | Comparateur Egal -> "Comparateur Egal"
  | Comparateur NonEgal -> "Comparateur NonEgal"
  | OperateurLogique Et -> "OperateurLogique Et"
  | OperateurLogique Ou -> "OperateurLogique Ou"
  | OperateurLogique Non -> "OperateurLogique Non"
  | OperateurLogique Equivalent -> "OperateurLogique Equivalent"
  | OperateurLogique NonEquivalent -> "OperateurLogique NonEquivalent"
  | Syntax Any -> "Syntax Any"
  | Syntax Character -> "Syntax Character"
  | Syntax Complex -> "Syntax Complex"
  | Syntax Constant -> "Syntax Constant"
  | Syntax Do -> "Syntax Do"
  | Syntax Double_precision -> "Syntax Double_precision"
  | Syntax Else -> "Syntax Else"
  | Syntax Else_if -> "Syntax Else_if"
  | Syntax Function -> "Syntax Function"
  | Syntax If -> "Syntax If"
  | Syntax End_if -> "Syntax End_if"
  | Syntax Out -> "Syntax Out"
  | Syntax Integer -> "Syntax Integer"
  | Syntax Logical -> "Syntax Logical"
  | Syntax Program -> "Syntax Program"
  | Syntax Real -> "Syntax Real"
  | Syntax Subroutine -> "Syntax Subroutine"
  | Syntax Then -> "Syntax Then"
  | Syntax While -> "Syntax While"
  | Syntax Call -> "Syntax Call"
  | Syntax Print -> "Syntax Print"
  | Syntax Return -> "Syntax Return"
  | Syntax Size -> "Syntax Size"
  | Syntax For -> "Syntax For"
  | Syntax Step -> "Syntax Step"
  | NewLine -> "NewLine"
  | Space -> "Space"
  | Name s -> "Name " ^ s
  | Integer s -> "Integer " ^ s
  | Floating s -> "Floating " ^ s
  | Double s -> "Double " ^ s
  | Chaine s -> "Chaine " ^ s
  | Commentaire s -> "Commentaire " ^ s
  | Booleen b -> "Booleen " ^ string_of_bool b
  | Virgule -> "Virgule"
  | Parentheseouvrante -> "Parentheseouvrante"
  | Parenthesefermante -> "Parenthesefermante"
  | ProgramRoot -> "ProgramRoot"
  | ToFlatten -> "ToFlatten"

let print_token (t : token) : unit = print_endline (string_of_token t)
