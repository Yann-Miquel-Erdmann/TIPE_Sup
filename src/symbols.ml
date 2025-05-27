type terminal =
  | EOF
  | E
  | PowerOp
  | NotOp
  | AndOp
  | OrOp
  | Dcon
  | Rcon
  | Icon
  | SconSingle
  | SconDouble
  | Ident
  | EOS
  | Return
  | Result
  | Contains
  | True
  | False
  | Program
  | Function
  | Subroutine
  | EndProgram
  | EndFunction
  | EndSubroutine
  | EndDo
  | EndIf
  | Colon
  | Comma
  | Equal
  | Asterisk
  | LParenthesis
  | RParenthesis
  | Integer
  | Real
  | Double
  | Complex
  | Character
  | Logical
  | Parameter
  | Intent
  | In
  | Out
  | InOut
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
  | Recursive

type non_terminal =
  | ExecutableProgram
  | StartCommentBlock
  | Function_or_Subroutine_star_MainProgram
  | Function_or_Subroutine_star
  | Recursive_opt_Function_or_Subroutine
  | Function_or_Subroutine
  | MainProgram
  | MainRange
  | Contains_Function_opt_EndProgramStmt
  | Contains_Function
  | FunctionSubprogram_star
  | BodyConstruct_star
  | ProgramStmt
  | EndProgramStmt
  | FunctionSubprogram
  | FunctionPrefix
  | FunctionRange
  | FunctionParList
  | FunctionPar_Comma_FunctionPar_star_opt
  | Comma_FunctionPar_star
  | FunctionPar
  | FunctionResult_opt
  | EndFunctionStmt
  | SubroutineSubprogram
  | SubroutineRange
  | SubroutineParList_opt
  | SubroutinePar_Comma_SubroutinePar_star_opt
  | Comma_SubroutinePar_star
  | SubroutinePar
  | EndSubroutineStmt
  | EndName_opt
  | BodyConstruct
  | SpecificationPartConstruct
  | DeclarationConstruct
  | TypeDeclarationStmt
  | Comma_AttrSpec_star
  | AttrSpec
  | Intent_in_out
  | In_out
  | TypeDecl_Assignment
  | Comma_ObjectName_star
  | Comma_EntityDecl_star
  | EntityDecl
  | Equal_Expr_opt
  | Asterisk_CharLength_opt
  | CharLength
  | TypeParamValue
  | Expr_Or_Asterisk
  | TypeSpec
  | KindSelector_opt
  | ExecutableConstruct
  | ReturnStmt
  | ActionStmt
  | AssignmentStmt
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
  | FunctionReference_opt
  | FunctionArg_Comma_FunctionArg_star_opt_RParenthesis
  | Comma_FunctionArg_star
  | FunctionArg
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

let repr_of_terminal (t : terminal) : string =
  match t with
  | EOF -> "End of file"
  | E -> "Epsilon"
  | PowerOp -> "\\*\\*"
  | NotOp -> "\\.not\\."
  | AndOp -> "\\.and\\."
  | OrOp -> "\\.or\\."
  | Dcon -> "(([0-9]+\\.[0-9]*)|(\\.[0-9]+))(d(\\+|-)?[0-9]+)"
  | Rcon -> "(([0-9]+\\.[0-9]*)|(\\.[0-9]+))(e(\\+|-)?[0-9]+)?"
  | Icon -> "[0-9]+(e(\\+|-)?[0-9]+)?"
  | SconSingle -> "['](~[']|'')*[']"
  | SconDouble -> "[\"](~[\"]|\"\")*[\"]"
  | Ident -> "[A-Za-z][A-Za-z0-9_]*"
  | EOS -> "((!~[\\n]*)?\\n[ ]*)+"
  | Return -> "return"
  | Result -> "result"
  | Contains -> "contains"
  | True -> "\\.true\\."
  | False -> "\\.false\\."
  | Program -> "program"
  | Function -> "function"
  | Subroutine -> "subroutine"
  | EndProgram -> "end program"
  | EndFunction -> "end function"
  | EndSubroutine -> "end subroutine"
  | EndDo -> "end do"
  | EndIf -> "end if"
  | Colon -> ":"
  | Comma -> ","
  | Equal -> "="
  | Asterisk -> "\\*"
  | LParenthesis -> "\\("
  | RParenthesis -> "\\)"
  | Integer -> "integer"
  | Real -> "real"
  | Double -> "double precision"
  | Complex -> "complex"
  | Character -> "character"
  | Logical -> "logical"
  | Parameter -> "parameter"
  | Intent -> "intent"
  | In -> "in"
  | Out -> "out"
  | InOut -> "inout"
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
  | Recursive -> "recursive"

let string_of_terminal (t : terminal) : string =
    match t with
    | EOF -> "EOF"
    | E -> "E"
    | PowerOp -> "PowerOp"
    | NotOp -> "NotOp"
    | AndOp -> "AndOp"
    | OrOp -> "OrOp"
    | Dcon -> "Dcon"
    | Rcon -> "Rcon"
    | Icon -> "Icon"
    | SconSingle -> "SconSingle"
    | SconDouble -> "SconDouble"
    | Ident -> "Ident"
    | EOS -> "EOS"
    | Return -> "Return"
    | Result -> "Result"
    | Contains -> "Contains"
    | True -> "True"
    | False -> "False"
    | Program -> "Program"
    | Function -> "Function"
    | Subroutine -> "Subroutine"
    | EndProgram -> "EndProgram"
    | EndFunction -> "EndFunction"
    | EndSubroutine -> "EndSubroutine"
    | EndDo -> "EndDo"
    | EndIf -> "EndIf"
    | Colon -> "Colon"
    | Comma -> "Comma"
    | Equal -> "Equal"
    | Asterisk -> "Asterisk"
    | LParenthesis -> "LParenthesis"
    | RParenthesis -> "RParenthesis"
    | Integer -> "Integer"
    | Real -> "Real"
    | Double -> "Double"
    | Complex -> "Complex"
    | Character -> "Character"
    | Logical -> "Logical"
    | Parameter -> "Parameter"
    | Intent -> "Intent"
    | In -> "In"
    | Out -> "Out"
    | InOut -> "InOut"
    | Call -> "Call"
    | Print -> "Print"
    | Do -> "Do"
    | While -> "While"
    | If -> "If"
    | Else -> "Else"
    | Then -> "Then"
    | Divise -> "Divise"
    | Plus -> "Plus"
    | Minus -> "Minus"
    | IsEqual -> "IsEqual"
    | NotEqual -> "NotEqual"
    | StrictLess -> "StrictLess"
    | LessEqual -> "LessEqual"
    | StrictGreater -> "StrictGreater"
    | GreaterEqual -> "GreaterEqual"
    | Equivalent -> "Equivalent"
    | NotEquivalent -> "NotEquivalent"
    | Space -> "Space"
    | Recursive -> "Recursive"

let string_of_non_terminal (nt : non_terminal) : string =
  match nt with
  | ExecutableProgram -> "ExecutableProgram"
  | StartCommentBlock -> "StartCommentBlock"
  | Function_or_Subroutine_star_MainProgram -> "Function_or_Subroutine_star_MainProgram"
  | Function_or_Subroutine_star -> "Function_or_Subroutine_star"
  | Recursive_opt_Function_or_Subroutine -> "Recursive_opt_Function_or_Subroutine"
  | Function_or_Subroutine -> "Function_or_Subroutine"
  | MainProgram -> "MainProgram"
  | MainRange -> "MainRange"
  | Contains_Function_opt_EndProgramStmt -> "Contains_Function_opt_EndProgramStmt"
  | Contains_Function -> "Contains_Function"
  | FunctionSubprogram_star -> "FunctionSubprogram_star"
  | BodyConstruct_star -> "BodyConstruct_star"
  | ProgramStmt -> "ProgramStmt"
  | EndProgramStmt -> "EndProgramStmt"
  | FunctionSubprogram -> "FunctionSubprogram"
  | FunctionPrefix -> "FunctionPrefix"
  | FunctionRange -> "FunctionRange"
  | FunctionParList -> "FunctionParList"
  | FunctionPar_Comma_FunctionPar_star_opt -> "FunctionPar_Comma_FunctionPar_star_opt"
  | Comma_FunctionPar_star -> "Comma_FunctionPar_star"
  | FunctionPar -> "FunctionPar"
  | FunctionResult_opt -> "FunctionResult_opt"
  | EndFunctionStmt -> "EndFunctionStmt"
  | SubroutineSubprogram -> "SubroutineSubprogram"
  | SubroutineRange -> "SubroutineRange"
  | SubroutineParList_opt -> "SubroutineParList_opt"
  | SubroutinePar_Comma_SubroutinePar_star_opt -> "SubroutinePar_Comma_SubroutinePar_star_opt"
  | Comma_SubroutinePar_star -> "Comma_SubroutinePar_star"
  | SubroutinePar -> "SubroutinePar"
  | EndSubroutineStmt -> "EndSubroutineStmt"
  | EndName_opt -> "EndName_opt"
  | BodyConstruct -> "BodyConstruct"
  | SpecificationPartConstruct -> "SpecificationPartConstruct"
  | DeclarationConstruct -> "DeclarationConstruct"
  | TypeDeclarationStmt -> "TypeDeclarationStmt"
  | Comma_AttrSpec_star -> "Comma_AttrSpec_star"
  | AttrSpec -> "AttrSpec"
  | Intent_in_out -> "Intent_in_out"
  | In_out -> "In_out"
  | TypeDecl_Assignment -> "TypeDecl_Assignment"
  | Comma_ObjectName_star -> "Comma_ObjectName_star"
  | Comma_EntityDecl_star -> "Comma_EntityDecl_star"
  | EntityDecl -> "EntityDecl"
  | Equal_Expr_opt -> "Equal_Expr_opt"
  | Asterisk_CharLength_opt -> "Asterisk_CharLength_opt"
  | CharLength -> "CharLength"
  | TypeParamValue -> "TypeParamValue"
  | Expr_Or_Asterisk -> "Expr_Or_Asterisk"
  | TypeSpec -> "TypeSpec"
  | KindSelector_opt -> "KindSelector_opt"
  | ExecutableConstruct -> "ExecutableConstruct"
  | ReturnStmt -> "ReturnStmt"
  | ActionStmt -> "ActionStmt"
  | AssignmentStmt -> "AssignmentStmt"
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
  | FunctionReference_opt -> "FunctionReference_opt"
  | FunctionArg_Comma_FunctionArg_star_opt_RParenthesis -> "FunctionArg_Comma_FunctionArg_star_opt_RParenthesis"
  | Comma_FunctionArg_star -> "Comma_FunctionArg_star"
  | FunctionArg -> "FunctionArg"
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
