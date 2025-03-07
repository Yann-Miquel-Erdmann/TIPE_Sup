type token_reg =
  | PowerOp
  | ConcatOp
  | NotOp
  | AndOp
  | OrOp
  | Rconr
  | Iconr
  | SconSingler
  | SconDoubler
  | Ident
  | EOSr
  | Program
  | End
  | Colon
  | Comma
  | Equal
  | Asterisk
  | RParenthesis
  | LParenthesis
  | Integer
  | Real
  | Double
  | Complex
  | Character
  | Logical
  | Kind
  | Len
  | Call
  | Continue
  | Print
  | Do
  | While
  | If
  | Else
  | Then
  | Divise
  | Plus
  | Minus
  | IsEqual
  | NotEqual
  | StrictLess
  | LessEqual
  | StrictGreater
  | GreaterEqual
  | Equivalent
  | NotEquivalent
  | Space

type token =
  | ExecutableProgram
  | ProgramUnit_star
  | StartCommentBlock
  | ProgramUnit
  | MainProgram
  | MainRange
  | BodyConstruct_star
  | ProgramStmt
  | EndProgramStmt
  | EndName_opt
  | BodyConstruct
  | SpecificationPartConstruct
  | DeclarationConstruct
  | TypeDeclarationStmt
  | Colon_Colon_opt
  | EntityDecl_Comma_star
  | EntityDecl
  | Equal_Expr_opt
  | Asterisk_CharLength_opt
  | CharLength
  | TypeParamValue
  | TypeSpec
  | KindSelector
  | ExecutableConstruct
  | ActionStmt
  | AssignmentStmt
  | CallStmt
  | ActualArg_Comma_star
  | ActualArg
  | Expr_Or_Asterisk
  | ContinueStmt
  | PrintStmt
  | Comma_OutputItemList_opt
  | FormatIdentifier
  | OutputItem
  | OutputItemList
  | DoConstruct
  | BlockDoConstruct
  | LoopControl_opt
  | EndDoStmt
  | Name_opt
  | LoopControl
  | Comma_IntRealDpExpression_opt
  | IntRealDpExpression
  | IfConstruct
  | ElseIfStmt_ExecutionPartConstruct_star_star
  | ExecutionPartConstruct_star
  | ElseStmt_ExecutionPartConstruct_star_opt
  | IfThenStmt
  | ElseIfStmt
  | ElseStmt
  | EndIfStmt
  | ExecutionPartConstruct
  | ScalarLogicalExpr
  | Expr
  | Level5Expr
  | Level5Expr_EquivOp_opt
  | EquivOperand
  | EquivOperand_OrOp_opt
  | OrOperand
  | OrOperand_AndOp_opt
  | AndOperand
  | NotOp_opt
  | Level4Expr
  | Level3Expr_RelOp_opt
  | Level3Expr
  | Level3Expr_ConcatOp_opt
  | Level2Expr
  | Level2Expr_AddOp_opt
  | AddOperand
  | AddOperand_MultOp_opt
  | MultOperand
  | PowerOp_MultOperand_opt
  | Level1Expr
  | Primary
  | UnsignedArithmeticConstant
  | ComplexConst
  | Name
  | ArrayName
  | ComponentName
  | EndName
  | DummyArgName
  | FunctionName
  | ImpliedDoVariable
  | ProgramName
  | SubroutineName
  | SubroutineNameUse
  | VariableName
  | ObjectName
  | MultOp
  | AddOp
  | Sign
  | RelOp
  | EquivOp
  | ScalarIntLiteralConstant
  | Rcon
  | Icon
  | SconSingle
  | SconDouble
  | Scon
  | EOS

let safe_token = Ident
let unparsed_tokens = [Space; ]

let assoc_tok (t : token_reg) : string =
  match t with
  | PowerOp -> "\\*\\*"
  | ConcatOp -> "//"
  | NotOp -> "\\.not\\."
  | AndOp -> "\\.and\\."
  | OrOp -> "\\.or\\."
  | Rconr -> "([0-9]+\\.[0-9]*)|(\\.[0-9]+)"
  | Iconr -> "[0-9]+"
  | SconSingler -> "['](~[']|'')*[']"
  | SconDoubler -> "[\"](~[\"]|\"\")*[\"]"
  | Ident -> "[A-Za-z][A-Za-z0-9_]*"
  | EOSr -> "((!~[\\n]*)?\\n[ ]*)+"
  | Program -> "program"
  | End -> "end"
  | Colon -> ":"
  | Comma -> ","
  | Equal -> "="
  | Asterisk -> "\\*"
  | RParenthesis -> "\\("
  | LParenthesis -> "\\)"
  | Integer -> "integer"
  | Real -> "real"
  | Double -> "double precision"
  | Complex -> "complex"
  | Character -> "character"
  | Logical -> "logical"
  | Kind -> "kind"
  | Len -> "len"
  | Call -> "call"
  | Continue -> "continue"
  | Print -> "print"
  | Do -> "do"
  | While -> "while"
  | If -> "if"
  | Else -> "else"
  | Then -> "then"
  | Divise -> "/"
  | Plus -> "\\+"
  | Minus -> "-"
  | IsEqual -> "(==)|(\\.eq\\.)"
  | NotEqual -> "(/=)|(\\.ne\\.)"
  | StrictLess -> "(<)|(\\.lt\\.)"
  | LessEqual -> "(<=)|(\\.le\\.)"
  | StrictGreater -> "(>)|(\\.gt\\.)"
  | GreaterEqual -> "(>=)|(\\.ge\\.)"
  | Equivalent -> "\\.eqv\\."
  | NotEquivalent -> "\\.neqv\\."
  | Space -> " "
