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
  | Integer
  | Logical
  | Program
  | Real
  | While
  | Call
  | Print
  | Return
  | Size
  | For
  | Step
  (* v---only for post-processing---v *)
  | Out
  | Subroutine

type token =
  | Operateur of operateur
  | Comparateur of comparateur
  | OperateurLogique of operateur_logique
  | Syntax of syntax
  | NewLine
  | Name of string
  | Integer of string
  | Floating of string
  | Double of string
  | Chaine of string
  | Commentaire of string
  | Booleen of bool
  | Parentheseouvrante
  | Parenthesefermante
  | ProgramRoot
  | ToFlatten

(* paramètres et enfants confondus dans la liste *)
type ast = Noeud of token * ast list
