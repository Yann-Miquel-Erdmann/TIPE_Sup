~~ExecutableProgram ->~~
~~ProgramUnit_star ->~~
~~StartCommentBlock ->~~
~~ProgramUnit ->~~
~~MainProgram ->~~
~~MainRange ->~~
~~BodyConstruct_star ->~~
~~ProgramStmt ->~~
~~EndProgramStmt ->~~
~~EndName_opt ->~~
~~BodyConstruct ->~~
~~SpecificationPartConstruct ->~~
~~DeclarationConstruct ->~~
~~TypeDeclarationStmt ->~~
~~TypeDecl_Assignment ->~~
~~EntityDecl_Comma_star ->~~
~~EntityDecl ->~~
~~Comma_ObjectName_star ->~~
~~Equal_Expr_opt ->~~
~~Asterisk_CharLength_opt ->~~
~~CharLength ->~~
~~TypeParamValue ->~~
~~TypeSpec ->~~
~~KindSelector ->~~
~~ExecutableConstruct ->~~
~~ActionStmt ->~~
~~AssignmentStmt ->~~
~~Expr_Or_Asterisk ->~~
~~PrintStmt ->~~
~~Comma_OutputItemList_opt ->~~
~~FormatIdentifier ->~~
~~OutputItemList ->~~
~~Comma_OutputItem_star ->~~
~~OutputItem ->~~
~~DoConstruct ->~~
~~BlockDoConstruct ->~~
~~LoopControl_opt ->~~
~~EndDoStmt ->~~
~~LoopControl ->~~
~~Comma_IntRealDpExpression_opt ->~~
~~IntRealDpExpression ->~~
~~IfConstruct ->~~
~~ElseIfStmt_ExecutionPartConstruct_star_star ->~~
~~ExecutionPartConstruct_star ->~~
~~ElseStmt_ExecutionPartConstruct_star_opt ->~~
~~IfThenStmt ->~~
~~ElseIfStmt ->~~
~~ElseStmt ->~~
~~EndIfStmt ->~~
~~ExecutionPartConstruct ->~~
~~ScalarLogicalExpr ->~~

~~Expr -> ...~~
~~Level5Expr -> ...~~
~~EquivOp_EquivOperand_star -> ...~~
~~EquivOperand -> ...~~
~~OrOp_OrOperand_star -> ...~~
~~OrOperand -> ...~~
~~AndOp_AndOperand_star -> ...~~
~~AndOperand -> ...~~
~~NotOp_opt -> ...~~
~~Level4Expr -> ...~~
~~RelOp_Level3Expr_star -> ...~~
~~Level3Expr -> ...~~
~~Level2Expr -> ...~~
~~AddOp_Sign_opt_AddOperand_star -> ...~~
~~Sign_opt_AddOperand -> ...~~
~~Sign_opt -> ...~~
~~AddOperand -> ...~~
~~MultOp_MultOperand_star -> ...~~
~~MultOperand -> ...~~
~~PowerOp_Level1Expr_star -> ...~~
~~Level1Expr -> ...~~
~~Primary -> ...~~
~~Name -> ...~~
~~ArrayName -> ...~~
~~ComponentName -> ...~~
~~EndName -> ...~~
~~DummyArgName -> ...~~
~~FunctionName -> ...~~
~~ImpliedDoVariable -> ...~~
~~ProgramName -> ...~~
~~SubroutineName -> ...~~
~~SubroutineNameUse -> ...~~
~~VariableName -> ...~~
~~ObjectName -> ...~~
~~LogicalConstant -> ...~~
~~PowerOp -> ...~~
~~MultOp -> ...~~
~~AddOp -> ...~~
~~Sign -> ...~~
~~RelOp -> ...~~
~~NotOp -> OperateurLogique Non~~
~~AndOp -> OperateurLogique And~~
~~OrOp -> OperateurLogique Or~~
~~EquivOp -> ...~~
~~ScalarIntLiteralConstant -> ...~~
~~Scon -> ...~~
~~Rcon -> Floating _~~
~~Icon -> Integer _~~
~~SconSingle -> Chaine _~~
~~SconDouble -> Chaine _~~
~~Ident -> Name _~~
~~True -> DataType Boolean true~~
~~False -> DataType Boolean false~~

~~((EOS -> (Commentaire _ / Newline) => later)~~







=> after here is not relevant


~~Program -> Syntax Program~~
~~EndProgram -> Syntax End_program~~
~~EndDo -> Syntax End_do~~
Colon -> PointVirgule
~~Comma -> Virgule~~
Equal -> Syntax Assign
Asterisk -> ? (with other thing)
RParenthesis -> Parenthesefermante
LParenthesis -> Parentheseouvrante
Integer -> Syntax Integer
Real -> Syntax Real
Double -> Syntax Double_precision
Complex -> Syntax Complex
Character -> Syntax Character
Logical -> Syntax Logical
Kind -> Syntax Kind
Len -> Syntax Len
Call -> Syntax Call
Print -> Syntax Print
Do -> Syntax Do
While -> Syntax While
If -> Syntax If
Then -> Syntax Then
Else -> Syntax Else
EndIf -> Syntax End_if

Divise -> Operateur Division
Plus -> Operateur Plus
Minus -> Operateur Moins

IsEqual -> Comparateur Egal
NotEqual -> Comparateur NonEgal
StrictLess -> Comparateur StrictPlusPetit
LessEqual -> Comparateur PlusPetit
StrictGreater -> Comparateur StrictPlusGrand
GreaterEqual -> Comparateur PlusGrand
Equivalent -> OperateurLogique Equivalent
NotEquivalent -> OperateurLogique NonEquivalent)