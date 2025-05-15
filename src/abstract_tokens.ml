type data_type =
  (* types *)
  | Entier of string
  | Flottant of string
  | Imaginaire of float * float
  | Booleen of bool
  | Caractere of string
  | Commentaire of string

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
  | End
  | Allocatable
  | Allocate
  | Any
  | Assign
  | Assignment
  | Block_data
  | Case
  | Character
  | Common
  | Complex
  | Contains
  | Constant
  | Data
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
  | Only
  | Operator
  | Optional
  | Out
  | Parameter
  | Pause
  | Pointer
  | Private
  | Program
  | End_program
  | Public
  | Real
  | Recursive
  | Result
  | Save
  | Select_case
  | End_select
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
  | AllocateBackspace
  | Call
  | Close
  | Continue
  | Cycle
  | Deallocate
  | Endfile
  | Error_stop
  | Event_post
  | Event_wait
  | Fail_image
  | Flush
  | Form_team
  | Format
  | Inquire
  | Lock
  | Namelist
  | Nullify
  | Open
  | Print
  | Read
  | Return
  | Rewind
  | Size
  | Stop
  | Sync_all
  | Sync_images
  | Sync_memory
  | Sync_team
  | Unlock
  | Wait
  | Write
  | Select
  | For
  | Step

type token =
  | DataType of data_type
  | Operateur of operateur
  | Comparateur of comparateur
  | OperateurLogique of operateur_logique
  | Syntax of syntax
  | Identificateur of string
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
  | QuatrePoints
  | Parentheseouvrante
  | Parenthesefermante
  | PointVirgule
  | ProgramRoot
  | ToFlatten

(* paramètres et enfants confondus dans la liste *)
type ast = Noeud of token * ast list
