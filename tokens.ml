
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

