type terminal =
  | EOF
  | E
  | PowerOp
  | ConcatOp
  | NotOp
  | AndOp
  | OrOp
  | Rcon
  | Icon
  | SconSingle
  | SconDouble
  | Ident
  | EOS
  | True
  | False
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
  | Call
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

type non_terminal =
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
  | LogicalConstant
  | MultOp
  | AddOp
  | Sign
  | RelOp
  | EquivOp
  | ScalarIntLiteralConstant
  | Scon
type symbol = | Terminal of terminal | NonTerminal of non_terminal
let safe_token = Ident
let unparsed_tokens = [Space; ]

let string_of_terminal (t : terminal) : string =
  match t with
  | EOF -> "End of File"
  | E -> "Epsilon"
  | PowerOp -> "\\*\\*"
  | ConcatOp -> "//"
  | NotOp -> "\\.not\\."
  | AndOp -> "\\.and\\."
  | OrOp -> "\\.or\\."
  | Rcon -> "([0-9]+\\.[0-9]*)|(\\.[0-9]+)"
  | Icon -> "[0-9]+"
  | SconSingle -> "['](~[']|'')*[']"
  | SconDouble -> "[\"](~[\"]|\"\")*[\"]"
  | Ident -> "[A-Za-z][A-Za-z0-9_]*"
  | EOS -> "((!~[\\n]*)?\\n[ ]*)+"
  | True -> "\\.true\\."
  | False -> "\\.false\\."
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
  | Call -> "call"
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
let string_of_non_terminal (nt : non_terminal) : string =
  match nt with
  | ExecutableProgram -> "ExecutableProgram"
  | ProgramUnit_star -> "ProgramUnit_star"
  | StartCommentBlock -> "StartCommentBlock"
  | ProgramUnit -> "ProgramUnit"
  | MainProgram -> "MainProgram"
  | MainRange -> "MainRange"
  | BodyConstruct_star -> "BodyConstruct_star"
  | ProgramStmt -> "ProgramStmt"
  | EndProgramStmt -> "EndProgramStmt"
  | EndName_opt -> "EndName_opt"
  | BodyConstruct -> "BodyConstruct"
  | SpecificationPartConstruct -> "SpecificationPartConstruct"
  | DeclarationConstruct -> "DeclarationConstruct"
  | TypeDeclarationStmt -> "TypeDeclarationStmt"
  | Colon_Colon_opt -> "Colon_Colon_opt"
  | EntityDecl_Comma_star -> "EntityDecl_Comma_star"
  | EntityDecl -> "EntityDecl"
  | Equal_Expr_opt -> "Equal_Expr_opt"
  | Asterisk_CharLength_opt -> "Asterisk_CharLength_opt"
  | CharLength -> "CharLength"
  | TypeParamValue -> "TypeParamValue"
  | TypeSpec -> "TypeSpec"
  | KindSelector -> "KindSelector"
  | ExecutableConstruct -> "ExecutableConstruct"
  | ActionStmt -> "ActionStmt"
  | AssignmentStmt -> "AssignmentStmt"
  | CallStmt -> "CallStmt"
  | Parenthesis_Actual_Comma_star_or_epsilon -> "Parenthesis_Actual_Comma_star_or_epsilon"
  | ActualArg_Comma_star -> "ActualArg_Comma_star"
  | ActualArg -> "ActualArg"
  | Expr_Or_Asterisk -> "Expr_Or_Asterisk"
  | PrintStmt -> "PrintStmt"
  | Comma_OutputItemList_opt -> "Comma_OutputItemList_opt"
  | FormatIdentifier -> "FormatIdentifier"
  | OutputItemList -> "OutputItemList"
  | Comma_OutputItem_star -> "Comma_OutputItem_star"
  | OutputItem -> "OutputItem"
  | DoConstruct -> "DoConstruct"
  | BlockDoConstruct -> "BlockDoConstruct"
  | LoopControl_opt -> "LoopControl_opt"
  | EndDoStmt -> "EndDoStmt"
  | Name_opt -> "Name_opt"
  | LoopControl -> "LoopControl"
  | Comma_IntRealDpExpression_opt -> "Comma_IntRealDpExpression_opt"
  | IntRealDpExpression -> "IntRealDpExpression"
  | IfConstruct -> "IfConstruct"
  | ElseIfStmt_ExecutionPartConstruct_star_star -> "ElseIfStmt_ExecutionPartConstruct_star_star"
  | ExecutionPartConstruct_star -> "ExecutionPartConstruct_star"
  | ElseStmt_ExecutionPartConstruct_star_opt -> "ElseStmt_ExecutionPartConstruct_star_opt"
  | IfThenStmt -> "IfThenStmt"
  | ElseIfStmt -> "ElseIfStmt"
  | ElseStmt -> "ElseStmt"
  | EndIfStmt -> "EndIfStmt"
  | ExecutionPartConstruct -> "ExecutionPartConstruct"
  | ScalarLogicalExpr -> "ScalarLogicalExpr"
  | Expr -> "Expr"
  | Level5Expr -> "Level5Expr"
  | EquivOp_EquivOperand_star -> "EquivOp_EquivOperand_star"
  | EquivOperand -> "EquivOperand"
  | OrOp_OrOperand_star -> "OrOp_OrOperand_star"
  | OrOperand -> "OrOperand"
  | AndOp_AndOperand_star -> "AndOp_AndOperand_star"
  | AndOperand -> "AndOperand"
  | NotOp_opt -> "NotOp_opt"
  | Level4Expr -> "Level4Expr"
  | RelOp_Level3Expr_star -> "RelOp_Level3Expr_star"
  | Level3Expr -> "Level3Expr"
  | ConcatOp_Level2Expr_star -> "ConcatOp_Level2Expr_star"
  | Level2Expr -> "Level2Expr"
  | AddOp_Sign_opt_AddOperand_star -> "AddOp_Sign_opt_AddOperand_star"
  | Sign_opt_AddOperand -> "Sign_opt_AddOperand"
  | Sign_opt -> "Sign_opt"
  | AddOperand -> "AddOperand"
  | MultOp_MultOperand_star -> "MultOp_MultOperand_star"
  | MultOperand -> "MultOperand"
  | PowerOp_Level1Expr_star -> "PowerOp_Level1Expr_star"
  | Level1Expr -> "Level1Expr"
  | Primary -> "Primary"
  | Comma_Expr_opt -> "Comma_Expr_opt"
  | Name -> "Name"
  | ArrayName -> "ArrayName"
  | ComponentName -> "ComponentName"
  | EndName -> "EndName"
  | DummyArgName -> "DummyArgName"
  | FunctionName -> "FunctionName"
  | ImpliedDoVariable -> "ImpliedDoVariable"
  | ProgramName -> "ProgramName"
  | SubroutineName -> "SubroutineName"
  | SubroutineNameUse -> "SubroutineNameUse"
  | VariableName -> "VariableName"
  | ObjectName -> "ObjectName"
  | LogicalConstant -> "LogicalConstant"
  | MultOp -> "MultOp"
  | AddOp -> "AddOp"
  | Sign -> "Sign"
  | RelOp -> "RelOp"
  | EquivOp -> "EquivOp"
  | ScalarIntLiteralConstant -> "ScalarIntLiteralConstant"
  | Scon -> "Scon"
