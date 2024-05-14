type data_type = 
  (* types *)
  | Entier of string
  | Flotant of string
  | Imaginaire of float*float
  | Booleen of bool
  | Caractere of string
;;

type operateur = 
  | Plus
  | Moins
  | Fois
  | Division
  | Puissance
  | Assignation
;;

type comparateur =  
  | PlusPetit
  | PlusGrand
  | StrictPlusPetit
  | StrictPlusGrand
  | Egal
  | NonEgal
;;

type operateur_logique = 
  | Et
  | Ou
  | Non
  | Equivalent
  | NonEquivalent
;;

type syntax = 
  | End
  | Allocatable	
  | Allocate	
  | Assign	
  | Assignment	
  | Block_data	
  | Case	
  | Character	
  | Common	
  | Complex	
  | Contains	
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
  |  Call
  |  Close
  |  Continue
  |  Cycle
  |  Deallocate
  |  Endfile
  |  Error_stop
  |  Event_post
  |  Event_wait
  |  Fail_image
  |  Flush
  |  Form_team
  |  Format
  |  Inquire
  |  Lock
  |  Namelist
  |  Nullify
  |  Open
  |  Print
  |  Read
  |  Return
  |  Rewind
  |  Stop
  |  Sync_all
  |  Sync_images
  |  Sync_memory
  |  Sync_team
  |  Unlock
  |  Wait
  |  Write
;;

type intrinsic_function =
  | Abort
  | Abs
  | Access
  | Achar
  | Acos
  | Acosd
  | Acosh
  | Adjustl
  | Adjustr
  | Aimag
  | Aint
  | Alarm
  | All
  | Allocated
  | Anint
  | Any
  | Asin
  | Asind
  | Asinh
  | Associated
  | Atan
  | Atand
  | Atan2
  | Atan2d
  | Atanh
  | Atomic_add
  | Atomic_and
  | Atomic_cas
  | Atomic_define
  | Atomic_fetch_add
  | Atomic_fetch_and
  | Atomic_fetch_or
  | Atomic_fetch_xor
  | Atomic_or
  | Atomic_ref
  | Atomic_xor
  | Backtrace
  | Bessel_j0
  | Bessel_j1
  | Bessel_jn
  | Bessel_y0
  | Bessel_y1
  | Bessel_yn
  | Bge
  | Bgt
  | Bit_size
  | Ble
  | Blt
  | Btest
  | Ceiling
  | Char
  | Chdir
  | Chmod
  | Cmplx
  | Co_broadcast
  | Co_max
  | Co_min
  | Co_reduce
  | Co_sum
  | Command_argument_count
  | Complex
  | Conjg
  | Cos
  | Cosd
  | Cosh
  | Cotan
  | Cotand
  | Count
  | Cpu_time
  | Cshift
  | Ctime
  | Date_and_time
  | Dble
  | Digits
  | Dim
  | Dot_product
  | Dprod
  | Dshiftl
  | Dshiftr
  | Eoshift
  | Epsilon
  | Erf
  | Erfc
  | Erfc_scaled
  | Etime
  | Event_query
  | Execute_command_line
  | Exit
  | Exp
  | Exponent
  | Extends_type_of
  | Fdate
  | Fget
  | Fgetc
  | Findloc
  | Floor
  | Flush
  | Fnum
  | Fput
  | Fputc
  | Fraction
  | Free
  | Fseek
  | Fstat
  | Ftell
  | Gamma
  | Gerror
  | Getarg
  | Get_command
  | Get_command_argument
  | Getcwd
  | Getenv
  | Get_environment_variable
  | Getgid
  | Getlog
  | Getpid
  | Getuid
  | Gmtime
  | Hostnm
  | Huge
  | Hypot
  | Iachar
  | Iall
  | Iand
  | Iany
  | Iargc
  | Ibclr
  | Ibits
  | Ibset
  | Ichar
  | Idate
  | Ieor
  | Ieorno
  | Image_index
  | Index
  | Int
  | Int2
  | Int8
  | Ior
  | Iparity
  | Irand
  | Is_contiguous
  | Is_iostat_end
  | Is_iostat_eor
  | Isatty
  | Ishft
  | Ishftc
  | Isnan
  | Itime
  | Kill
  | Kind
  | Lbound
  | Lcobound
  | Leadz
  | Len
  | Len_trim
  | Lge
  | Lgt
  | Link
  | Lle
  | Llt
  | Lnblnk
  | Loc
  | Log
  | Log10
  | Log_gamma
  | Logical
  | Lshift
  | Lstat
  | Ltime
  | Malloc
  | Maskl
  | Maskr
  | Matmul
  | Max
  | Maxexponent
  | Maxloc
  | Maxval
  | Mclock
  | Mclock8
  | Merge
  | Merge_bits
  | Min
  | Minexponent
  | Minloc
  | Minval
  | Mod
  | Modulo
  | Move_alloc
  | Mvbits
  | Nearest
  | New_line
  | Nint
  | Norm2
  | Not
  | Null
  | Pack
  | Parity
  | Perror
  | Popcnt
  | Poppar
  | Precision
  | Present
  | Product
  | Radix
  | Ran
  | Rand
  | Random_init
  | Random_number
  | Random_seed
  | Range
  | Rank
  | Real
  | Rename
  | Repeat
  | Reshape
  | Rrspacing
  | Rshift
  | Same_type_as
  | Scale
  | Scan
  | Secnds
  | Second
  | Selected_char_kind
  | Selected_int_kind
  | Selected_real_kind
  | Set_exponent
  | Shape
  | Shifta
  | Shiftl
  | Shiftr
  | Sign
  | Signal
  | Sin
  | Sind
  | Sinh
  | Size
  | Sizeof
  | Sleep
  | Spacing
  | Spread
  | Sqrt
  | Srand
  | Stat
  | Storage_size
  | Sum
  | Symlnk
  | System
  | System_clock
  | Tan
  | Tand
  | Tanh
  | This_image
  | Time
  | Time8
  | Tiny
  | Trailz
  | Transfer
  | Transpose
  | Trim
  | Ttynam
  | Ubound
  | Ucobound
  | Umask
  | Unlink
  | Unpack
  | Verify
  | Xor
;;

type token = 
  | DataType of data_type
  | Operateur of operateur
  | Comparateur of comparateur
  | OperateurLogique of operateur_logique
  | Syntax of syntax
  | IntrinsicFunction of intrinsic_function
  | Identificateur of string
  | NewLine
  | Space
  | Name of char list
  | Integer of char list
  | Floating of char list
  | Virgule
  | QuatrePoints
  | StringDelimiter1
  | StringDelimiter2
  | Commentaire
;;

type ast = Noeud of token * (token list)
