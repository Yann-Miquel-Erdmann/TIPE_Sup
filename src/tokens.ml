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
  | EndProgram
  | EndDo
  | EndIf
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
  | Parenthesis_Actual_Comma_star_or_epsilon
  | ActualArg_Comma_star
  | ActualArg
  | Expr_Or_Asterisk
  | ContinueStmt
  | PrintStmt
  | Comma_OutputItemList_opt
  | FormatIdentifier
  | OutputItemList
  | Comma_OutputItem_star
  | OutputItem
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
  | EquivOp_EquivOperand_star
  | EquivOperand
  | OrOp_OrOperand_star
  | OrOperand
  | AndOp_AndOperand_star
  | AndOperand
  | NotOp_opt
  | Level4Expr
  | RelOp_Level3Expr_star
  | Level3Expr
  | ConcatOp_Level2Expr_star
  | Level2Expr
  | AddOp_Sign_opt_AddOperand_star
  | Sign_opt_AddOperand
  | Sign_opt
  | AddOperand
  | MultOp_MultOperand_star
  | MultOperand
  | PowerOp_Level1Expr_star
  | Level1Expr
  | Primary
  | Comma_Expr_opt
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
  | EndProgram -> "end program"
  | EndDo -> "end do"
  | EndIf -> "end if"
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
