open Grammar_functions
open Symbols
let g = { start_symbol = NonTerminal ExecutableProgram;
rules_htbl = Hashtbl.of_seq (List.to_seq [(NonTerminal ExecutableProgram,[[NonTerminal ProgramUnit;NonTerminal ProgramUnit_star;];[NonTerminal StartCommentBlock;NonTerminal ProgramUnit;NonTerminal ProgramUnit_star;];]);
(NonTerminal ProgramUnit_star,[[NonTerminal ProgramUnit;NonTerminal ProgramUnit_star;];[Terminal E;];]);
(NonTerminal StartCommentBlock,[[Terminal EOS;];]);
(NonTerminal ProgramUnit,[[NonTerminal MainProgram;];]);
(NonTerminal MainProgram,[[NonTerminal ProgramStmt;NonTerminal MainRange;];]);
(NonTerminal MainRange,[[NonTerminal BodyConstruct;NonTerminal BodyConstruct_star;NonTerminal EndProgramStmt;];[NonTerminal EndProgramStmt;];]);
(NonTerminal BodyConstruct_star,[[NonTerminal BodyConstruct;NonTerminal BodyConstruct_star;];[Terminal E;];]);
(NonTerminal ProgramStmt,[[Terminal Program;NonTerminal ProgramName;Terminal EOS;];]);
(NonTerminal EndProgramStmt,[[Terminal EndProgram;NonTerminal EndName_opt;Terminal EOS;];]);
(NonTerminal EndName_opt,[[NonTerminal EndName;];[Terminal E;];]);
(NonTerminal BodyConstruct,[[NonTerminal SpecificationPartConstruct;];[NonTerminal ExecutableConstruct;];]);
(NonTerminal SpecificationPartConstruct,[[NonTerminal DeclarationConstruct;];]);
(NonTerminal DeclarationConstruct,[[NonTerminal TypeDeclarationStmt;];]);
(NonTerminal TypeDeclarationStmt,[[NonTerminal TypeSpec;NonTerminal Colon_Colon_opt;NonTerminal EntityDecl;NonTerminal EntityDecl_Comma_star;Terminal EOS;];]);
(NonTerminal Colon_Colon_opt,[[Terminal Colon;Terminal Colon;];[Terminal E;];]);
(NonTerminal EntityDecl_Comma_star,[[NonTerminal EntityDecl;NonTerminal EntityDecl_Comma_star;];[Terminal E;];]);
(NonTerminal EntityDecl,[[NonTerminal ObjectName;NonTerminal Asterisk_CharLength_opt;NonTerminal Equal_Expr_opt;];]);
(NonTerminal Equal_Expr_opt,[[Terminal Equal;NonTerminal Expr;];[Terminal E;];]);
(NonTerminal Asterisk_CharLength_opt,[[Terminal Asterisk;NonTerminal CharLength;];[Terminal E;];]);
(NonTerminal CharLength,[[Terminal RParenthesis;NonTerminal TypeParamValue;Terminal LParenthesis;];[NonTerminal ScalarIntLiteralConstant;];]);
(NonTerminal TypeParamValue,[[NonTerminal Expr_Or_Asterisk;];]);
(NonTerminal TypeSpec,[[Terminal Integer;NonTerminal KindSelector;];[Terminal Double;];[Terminal Complex;NonTerminal KindSelector;];[Terminal Logical;NonTerminal KindSelector;];]);
(NonTerminal KindSelector,[[Terminal RParenthesis;NonTerminal Expr;Terminal LParenthesis;];]);
(NonTerminal ExecutableConstruct,[[NonTerminal ActionStmt;];[NonTerminal DoConstruct;];[NonTerminal IfConstruct;];[NonTerminal EndDoStmt;];]);
(NonTerminal ActionStmt,[[NonTerminal AssignmentStmt;];[NonTerminal PrintStmt;];]);
(NonTerminal AssignmentStmt,[[NonTerminal Name;Terminal Equal;NonTerminal Expr;Terminal EOS;];]);
(NonTerminal Expr_Or_Asterisk,[[NonTerminal Expr;];[Terminal Asterisk;];]);
(NonTerminal PrintStmt,[[Terminal Print;NonTerminal FormatIdentifier;NonTerminal Comma_OutputItemList_opt;Terminal EOS;];]);
(NonTerminal Comma_OutputItemList_opt,[[Terminal Comma;NonTerminal OutputItemList;];[Terminal E;];]);
(NonTerminal FormatIdentifier,[[Terminal Asterisk;];]);
(NonTerminal OutputItemList,[[NonTerminal OutputItem;NonTerminal Comma_OutputItem_star;];]);
(NonTerminal Comma_OutputItem_star,[[Terminal Comma;NonTerminal OutputItem;NonTerminal Comma_OutputItem_star;];[Terminal E;];]);
(NonTerminal OutputItem,[[NonTerminal Expr;];]);
(NonTerminal DoConstruct,[[NonTerminal BlockDoConstruct;];]);
(NonTerminal BlockDoConstruct,[[Terminal Do;NonTerminal LoopControl_opt;Terminal EOS;];]);
(NonTerminal LoopControl_opt,[[NonTerminal LoopControl;];[Terminal E;];]);
(NonTerminal EndDoStmt,[[Terminal EndDo;NonTerminal Name_opt;Terminal EOS;];]);
(NonTerminal Name_opt,[[NonTerminal Name;];[Terminal E;];]);
(NonTerminal LoopControl,[[Terminal While;Terminal RParenthesis;NonTerminal Expr;Terminal LParenthesis;];[NonTerminal VariableName;Terminal Equal;NonTerminal IntRealDpExpression;Terminal Comma;NonTerminal IntRealDpExpression;NonTerminal Comma_IntRealDpExpression_opt;];]);
(NonTerminal Comma_IntRealDpExpression_opt,[[Terminal Comma;NonTerminal IntRealDpExpression;];[Terminal E;];]);
(NonTerminal IntRealDpExpression,[[NonTerminal Expr;];]);
(NonTerminal IfConstruct,[[NonTerminal IfThenStmt;NonTerminal ExecutionPartConstruct_star;NonTerminal ElseIfStmt_ExecutionPartConstruct_star_star;NonTerminal ElseStmt_ExecutionPartConstruct_star_opt;NonTerminal EndIfStmt;];]);
(NonTerminal ElseIfStmt_ExecutionPartConstruct_star_star,[[NonTerminal ElseIfStmt;NonTerminal ExecutionPartConstruct_star;NonTerminal ElseIfStmt_ExecutionPartConstruct_star_star;];[Terminal E;];]);
(NonTerminal ExecutionPartConstruct_star,[[NonTerminal ExecutionPartConstruct;NonTerminal ExecutionPartConstruct_star;];[Terminal E;];]);
(NonTerminal ElseStmt_ExecutionPartConstruct_star_opt,[[NonTerminal ElseStmt;NonTerminal ExecutionPartConstruct_star;];[Terminal E;];]);
(NonTerminal IfThenStmt,[[Terminal If;Terminal RParenthesis;NonTerminal ScalarLogicalExpr;Terminal LParenthesis;Terminal Then;Terminal EOS;];]);
(NonTerminal ElseIfStmt,[[Terminal Else;Terminal If;Terminal RParenthesis;NonTerminal ScalarLogicalExpr;Terminal LParenthesis;Terminal Then;Terminal EOS;];]);
(NonTerminal ElseStmt,[[Terminal Else;Terminal EOS;];]);
(NonTerminal EndIfStmt,[[Terminal EndIf;Terminal EOS;];]);
(NonTerminal ExecutionPartConstruct,[[NonTerminal ExecutableConstruct;];]);
(NonTerminal ScalarLogicalExpr,[[NonTerminal Expr;];]);
(NonTerminal Expr,[[NonTerminal Level5Expr;];]);
(NonTerminal Level5Expr,[[NonTerminal EquivOperand;NonTerminal EquivOp_EquivOperand_star;];]);
(NonTerminal EquivOp_EquivOperand_star,[[NonTerminal EquivOp;NonTerminal EquivOperand;NonTerminal EquivOp_EquivOperand_star;];[Terminal E;];]);
(NonTerminal EquivOperand,[[NonTerminal OrOperand;NonTerminal OrOp_OrOperand_star;];]);
(NonTerminal OrOp_OrOperand_star,[[Terminal OrOp;NonTerminal OrOperand;NonTerminal OrOp_OrOperand_star;];[Terminal E;];]);
(NonTerminal OrOperand,[[NonTerminal AndOperand;NonTerminal AndOp_AndOperand_star;];]);
(NonTerminal AndOp_AndOperand_star,[[Terminal AndOp;NonTerminal AndOperand;NonTerminal AndOp_AndOperand_star;];[Terminal E;];]);
(NonTerminal AndOperand,[[NonTerminal NotOp_opt;NonTerminal Level4Expr;];]);
(NonTerminal NotOp_opt,[[Terminal NotOp;];[Terminal E;];]);
(NonTerminal Level4Expr,[[NonTerminal Level3Expr;NonTerminal RelOp_Level3Expr_star;];]);
(NonTerminal RelOp_Level3Expr_star,[[NonTerminal RelOp;NonTerminal Level3Expr;NonTerminal RelOp_Level3Expr_star;];[Terminal E;];]);
(NonTerminal Level3Expr,[[NonTerminal Level2Expr;NonTerminal ConcatOp_Level2Expr_star;];]);
(NonTerminal ConcatOp_Level2Expr_star,[[Terminal ConcatOp;NonTerminal Level2Expr;NonTerminal ConcatOp_Level2Expr_star;];[Terminal E;];]);
(NonTerminal Level2Expr,[[NonTerminal Sign_opt_AddOperand;NonTerminal AddOp_Sign_opt_AddOperand_star;];]);
(NonTerminal AddOp_Sign_opt_AddOperand_star,[[NonTerminal AddOp;NonTerminal Sign_opt_AddOperand;NonTerminal AddOp_Sign_opt_AddOperand_star;];[Terminal E;];]);
(NonTerminal Sign_opt_AddOperand,[[NonTerminal Sign_opt;NonTerminal AddOperand;];]);
(NonTerminal Sign_opt,[[NonTerminal Sign;];[Terminal E;];]);
(NonTerminal AddOperand,[[NonTerminal MultOperand;NonTerminal MultOp_MultOperand_star;];]);
(NonTerminal MultOp_MultOperand_star,[[NonTerminal MultOp;NonTerminal MultOperand;NonTerminal MultOp_MultOperand_star;];[Terminal E;];]);
(NonTerminal MultOperand,[[NonTerminal Level1Expr;NonTerminal PowerOp_Level1Expr_star;];]);
(NonTerminal PowerOp_Level1Expr_star,[[Terminal PowerOp;NonTerminal Level1Expr;NonTerminal PowerOp_Level1Expr_star;];[Terminal E;];]);
(NonTerminal Level1Expr,[[NonTerminal Primary;];]);
(NonTerminal Primary,[[Terminal Icon;];[Terminal Rcon;];[NonTerminal Name;];[Terminal RParenthesis;NonTerminal Expr;NonTerminal Comma_Expr_opt;Terminal LParenthesis;];[NonTerminal Scon;];[NonTerminal LogicalConstant;];]);
(NonTerminal Comma_Expr_opt,[[Terminal Comma;NonTerminal Expr;];[Terminal E;];]);
(NonTerminal Name,[[Terminal Ident;];]);
(NonTerminal ArrayName,[[Terminal Ident;];]);
(NonTerminal ComponentName,[[Terminal Ident;];]);
(NonTerminal EndName,[[Terminal Ident;];]);
(NonTerminal DummyArgName,[[Terminal Ident;];]);
(NonTerminal FunctionName,[[Terminal Ident;];]);
(NonTerminal ImpliedDoVariable,[[Terminal Ident;];]);
(NonTerminal ProgramName,[[Terminal Ident;];]);
(NonTerminal SubroutineName,[[Terminal Ident;];]);
(NonTerminal SubroutineNameUse,[[Terminal Ident;];]);
(NonTerminal VariableName,[[Terminal Ident;];]);
(NonTerminal ObjectName,[[Terminal Ident;];]);
(NonTerminal LogicalConstant,[[Terminal True;];[Terminal False;];]);
(NonTerminal MultOp,[[Terminal Asterisk;];[Terminal Divise;];]);
(NonTerminal AddOp,[[NonTerminal Sign;];]);
(NonTerminal Sign,[[Terminal Plus;];[Terminal Minus;];]);
(NonTerminal RelOp,[[Terminal IsEqual;];[Terminal NotEqual;];[Terminal StrictLess;];[Terminal LessEqual;];[Terminal StrictGreater;];[Terminal GreaterEqual;];]);
(NonTerminal EquivOp,[[Terminal Equivalent;];[Terminal NotEquivalent;];]);
(NonTerminal ScalarIntLiteralConstant,[[Terminal Icon;];]);
(NonTerminal Scon,[[Terminal SconSingle;];[Terminal SconDouble;];]);
])}