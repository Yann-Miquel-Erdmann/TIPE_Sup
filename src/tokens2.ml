type token =
  | PowerOp of string
  | ConcatOp of string
  | NotOp of string
  | AndOp of string
  | OrOp of string
  | Rcon of string
  | Icon of string
  | SconSingle of string
  | SconDouble of string
  | Ident of string
  | EOS of string
  | Program of string
  | End of string
  | Colon of string
  | Comma of string
  | Asterisk of string
  | RParenthesis of string
  | LParenthesis of string
  | Integer of string
  | Real of string
  | Double of string
  | Precision of string
  | Complex of string
  | Character of string
  | Logical of string
  | Kind of string
  | Len of string
  | Call of string
  | Continue of string
  | Print of string
  | Do of string
  | While of string
  | If of string
  | Else of string
  | Then of string
  | Divise of string
  | Plus of string
  | Minus of string
  | Equal of string
  | NotEqual of string
  | StrictLess of string
  | LessEqual of string
  | StrictGreater of string
  | GreaterEqual of string
  | Equivalent of string
  | NotEquivalent of string
  | ExecutableProgram
  | StartCommentBlock
  | ProgramUnit
  | MainProgram
  | MainRange
  | Programtmt
  | EndProgramtmt
  | BodyConstruct
  | SpecificationPartConstruct
  | DeclarationConstruct
  | TypeDeclarationStmt
  | EntityDecl
  | CharLength
  | TypeSpec
  | KindSelector
  | CharSelector
  | TypeParamValue
  | SpecificationExpr
  | ActionStmt
  | AssignmentStmt
  | CallStmt
  | ActualArg
  | ContinueStmt
  | PrintStmt
  | FormatIdentifier
  | OutputItem
  | OutputItemList
  | DoConstruct
  | BlockDoConstruct
  | EndDoStmt
  | LoopControl
  | Expression
  | Dp
  | Int
  | IfConstruct
  | IfThenStmt
  | ElseIfStmt
  | ElseStmt
  | EndIfStmt
  | ExecutableConstruct
  | ExecutionPartConstruct
  | ScalarLogicalExpr
  | Level5Expr
  | EquivOperand
  | OrOperand
  | AndOperand
  | Level4Expr
  | Level3Expr
  | Level2Expr
  | AddOperand
  | MultOperand
  | Level1Expr
  | Primary
  | UnsignedArithmeticConstant
  | Expr
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
  | Scon
;;