use crate::spec::{EnumBuilder, Node, NodesAggregator, StructBuilder};

/// The specific syntax specification of Cairo.
pub fn get_spec() -> Vec<Node> {
    NodesAggregator::default()
    .add_list("Trivia", "Trivium")
    .add_enum(
        EnumBuilder::new("Trivium")
            .node_with_explicit_kind("SingleLineComment", "TokenSingleLineComment")
            .node_with_explicit_kind("SingleLineDocComment", "TokenSingleLineDocComment")
            .node_with_explicit_kind("SingleLineInnerComment", "TokenSingleLineInnerComment")
            .node_with_explicit_kind("Whitespace", "TokenWhitespace")
            .node_with_explicit_kind("Newline", "TokenNewline")
            .node_with_explicit_kind("Skipped", "TokenSkipped")
            .node("SkippedNode"),
    )
    // --- Expressions ---
    .add_enum(EnumBuilder::new("Expr")
        .missing("Missing")
        .node("Path")
        .node_with_explicit_kind("Literal", "TerminalLiteralNumber")
        .node_with_explicit_kind("ShortString", "TerminalShortString")
        .node_with_explicit_kind("String", "TerminalString")
        .node_with_explicit_kind("False", "TerminalFalse")
        .node_with_explicit_kind("True", "TerminalTrue")
        .node("Parenthesized")
        .node("Unary")
        .node("Binary")
        .node_with_explicit_kind("Tuple", "ExprListParenthesized")
        .node("FunctionCall")
        .node("StructCtorCall")
        .node("Block")
        .node("Match")
        .node("If")
        .node("Loop")
        .node("While")
        .node("For")
        .node("Closure")
        .node("ErrorPropagate")
        .node("FieldInitShorthand")
        .node("Indexed")
        .node("InlineMacro")
        .node("FixedSizeArray")
    )
    .add_separated_list("ExprList", "Expr", "TerminalComma")
    .add_struct(StructBuilder::new("Arg")
        .node("modifiers", "ModifierList")
        .node("arg_clause", "ArgClause")
    )
    .add_enum(EnumBuilder::new("ArgClause")
        .node("Unnamed")
        .node("Named")
        .node("FieldInitShorthand")
    )
    .add_struct(StructBuilder::new("ArgClauseNamed")
        .node("name", "TerminalIdentifier")
        .node("colon", "TerminalColon")
        .node("value", "Expr")
    )
    .add_struct(StructBuilder::new("ArgClauseUnnamed")
        .node("value", "Expr")
    )
    .add_struct(StructBuilder::new("ArgClauseFieldInitShorthand")
        .node("colon", "TerminalColon")
        .node("name", "ExprFieldInitShorthand")
    )
    .add_struct(StructBuilder::new("ExprFieldInitShorthand")
        .node("name", "TerminalIdentifier")
    )
    .add_separated_list("ArgList", "Arg", "TerminalComma")
    .add_struct(StructBuilder::new("ExprMissing"))
    .add_enum(EnumBuilder::new("PathSegment").node("Simple").node("WithGenericArgs").missing("Missing"))
    .add_struct(StructBuilder::new("PathSegmentSimple").node("ident", "TerminalIdentifier"))
    .add_option("TerminalColonColon")
    .add_struct(StructBuilder::new("PathSegmentWithGenericArgs")
        .node("ident", "TerminalIdentifier")
        .node("separator", "OptionTerminalColonColon")
        .node("generic_args", "GenericArgs")
    )
    .add_struct(StructBuilder::new("PathSegmentMissing"))
    .add_separated_list("ExprPath", "PathSegment", "TerminalColonColon")
    .add_struct(StructBuilder::new("ExprParenthesized")
        .node("lparen", "TerminalLParen")
        .node("expr", "Expr")
        .node("rparen", "TerminalRParen")
    )
    .add_struct(StructBuilder::new("ExprUnary").node("op", "UnaryOperator").node("expr", "Expr"))
    .add_enum(EnumBuilder::new("UnaryOperator")
        .node_with_explicit_kind("Not", "TerminalNot")
        .node_with_explicit_kind("BitNot", "TerminalBitNot")
        .node_with_explicit_kind("Minus", "TerminalMinus")
        .node_with_explicit_kind("At", "TerminalAt")
        .node_with_explicit_kind("Desnap", "TerminalMul")
    )
    .add_struct(StructBuilder::new("ExprBinary")
        .node("lhs", "Expr")
        .node("op", "BinaryOperator")
        .node("rhs", "Expr")
    )
    .add_enum(EnumBuilder::new("BinaryOperator")
        .node_with_explicit_kind("Dot", "TerminalDot")
        .node_with_explicit_kind("Not", "TerminalNot")
        .node_with_explicit_kind("Mul", "TerminalMul")
        .node_with_explicit_kind("MulEq", "TerminalMulEq")
        .node_with_explicit_kind("Div", "TerminalDiv")
        .node_with_explicit_kind("DivEq", "TerminalDivEq")
        .node_with_explicit_kind("Mod", "TerminalMod")
        .node_with_explicit_kind("ModEq", "TerminalModEq")
        .node_with_explicit_kind("Plus", "TerminalPlus")
        .node_with_explicit_kind("PlusEq", "TerminalPlusEq")
        .node_with_explicit_kind("Minus", "TerminalMinus")
        .node_with_explicit_kind("MinusEq", "TerminalMinusEq")
        .node_with_explicit_kind("EqEq", "TerminalEqEq")
        .node_with_explicit_kind("Neq", "TerminalNeq")
        .node_with_explicit_kind("Eq", "TerminalEq")
        .node_with_explicit_kind("And", "TerminalAnd")
        .node_with_explicit_kind("AndAnd", "TerminalAndAnd")
        .node_with_explicit_kind("Or", "TerminalOr")
        .node_with_explicit_kind("OrOr", "TerminalOrOr")
        .node_with_explicit_kind("Xor", "TerminalXor")
        .node_with_explicit_kind("LE", "TerminalLE")
        .node_with_explicit_kind("GE", "TerminalGE")
        .node_with_explicit_kind("LT", "TerminalLT")
        .node_with_explicit_kind("GT", "TerminalGT")
        .node_with_explicit_kind("DotDot", "TerminalDotDot")
        .node_with_explicit_kind("DotDotEq", "TerminalDotDotEq")
    )
    .add_struct(StructBuilder::new("ExprListParenthesized")
        .node("lparen", "TerminalLParen")
        .node("expressions", "ExprList")
        .node("rparen", "TerminalRParen")
    )
    .add_struct(StructBuilder::new("ExprFunctionCall")
        .node("path", "ExprPath")
        .node("arguments", "ArgListParenthesized")
    )
    .add_struct(StructBuilder::new("ArgListParenthesized")
        .node("lparen", "TerminalLParen")
        .node("arguments", "ArgList")
        .node("rparen", "TerminalRParen")
    )
    .add_option("ArgListParenthesized")
    .add_struct(StructBuilder::new("ExprStructCtorCall")
        .node("path", "ExprPath")
        .node("arguments", "StructArgListBraced")
    )
    .add_struct(StructBuilder::new("StructArgListBraced")
        .node("lbrace", "TerminalLBrace")
        .node("arguments", "StructArgList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("ExprBlock")
        .node("lbrace", "TerminalLBrace")
        .node("statements", "StatementList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("ExprMatch")
        .node("match_kw", "TerminalMatch")
        // TODO(yuval): change to SimpleExpr
        .node("expr", "Expr")
        .node("lbrace", "TerminalLBrace")
        .node("arms", "MatchArms")
        .node("rbrace", "TerminalRBrace")
    )
    .add_separated_list("MatchArms", "MatchArm", "TerminalComma")
    .add_struct(StructBuilder::new("MatchArm")
        .node("patterns", "PatternListOr")
        .node("arrow", "TerminalMatchArrow")
        .node("expression", "Expr")
    )
    .add_struct(StructBuilder::new("ExprIf")
        .node("if_kw", "TerminalIf")
        .node("condition", "Condition")
        .node("if_block", "ExprBlock")
        .node("else_clause", "OptionElseClause")
    )
    .add_enum(EnumBuilder::new("Condition")
        .node("Let")
        .node("Expr")
    )
    .add_struct(StructBuilder::new("ConditionLet")
        .node("let_kw", "TerminalLet")
        .node("patterns", "PatternListOr")
        .node("eq", "TerminalEq")
        .node("expr", "Expr")
    )
    .add_struct(StructBuilder::new("ConditionExpr")
        .node("expr", "Expr")
    )
    .add_enum(EnumBuilder::new("BlockOrIf")
        .node_with_explicit_kind("Block", "ExprBlock")
        .node_with_explicit_kind("If", "ExprIf")
    )
    .add_struct(StructBuilder::new("ExprLoop")
        .node("loop_kw", "TerminalLoop")
        .node("body", "ExprBlock")
    )
    .add_struct(StructBuilder::new("ExprWhile")
        .node("while_kw", "TerminalWhile")
        .node("condition", "Condition")
        .node("body", "ExprBlock")
    )
    .add_struct(StructBuilder::new("ExprFor")
        .node("for_kw", "TerminalFor")
        .key_node("pattern", "Pattern")
        .key_node("identifier", "TerminalIdentifier")
        .node("expr", "Expr")
        .node("body", "ExprBlock")
    )
    .add_struct(StructBuilder::new("ElseClause")
        .node("else_kw", "TerminalElse")
        .node("else_block_or_if", "BlockOrIf")
    )
    .add_option("ElseClause")
    .add_struct(StructBuilder::new("ExprErrorPropagate").node("expr", "Expr").node("op", "TerminalQuestionMark"))
    .add_struct(StructBuilder::new("ExprIndexed")
        .node("expr", "Expr")
        .node("lbrack", "TerminalLBrack")
        .node("index_expr", "Expr")
        .node("rbrack", "TerminalRBrack")
    )
    .add_struct(StructBuilder::new("ExprInlineMacro")
        .node("path", "ExprPath")
        .node("bang", "TerminalNot")
        .node("arguments", "WrappedArgList")
    )
    .add_struct(StructBuilder::new("ExprFixedSizeArray")
        .node("lbrack", "TerminalLBrack")
        .node("exprs", "ExprList")
        .node("size", "OptionFixedSizeArraySize")
        .node("rbrack", "TerminalRBrack")
    )
    .add_struct(StructBuilder::new("FixedSizeArraySize")
        .node("semicolon", "TerminalSemicolon")
        .node("size", "Expr")
    )
    .add_option("FixedSizeArraySize")
    .add_struct(StructBuilder::new("ExprClosure")
        .node("wrapper", "ClosureParamWrapper")
        .node("ret_ty", "OptionReturnTypeClause")
        .node("optional_no_panic", "OptionTerminalNoPanic")
        .node("expr", "Expr")
    )
    .add_enum(EnumBuilder::new("ClosureParamWrapper")
        .node_with_explicit_kind("Nullary", "TerminalOrOr")
        .node("NAry")
    )
    .add_struct(StructBuilder::new("ClosureParamWrapperNAry")
        .node("leftor","TerminalOr")
        .node("params", "ParamList")
        .node("rightor", "TerminalOr")
    )
    // --- Struct ctor ---
    .add_struct(StructBuilder::new("StructArgExpr")
        .node("colon", "TerminalColon")
        .node("expr", "Expr")
    )
    .add_option("StructArgExpr")
    .add_struct(StructBuilder::new("StructArgSingle")
        .key_node("identifier", "TerminalIdentifier")
        .node("arg_expr", "OptionStructArgExpr")
    )
    .add_struct(StructBuilder::new("StructArgTail")
        .node("dotdot", "TerminalDotDot")
        .node("expression", "Expr")
    )
    .add_enum(EnumBuilder::new("StructArg")
        .node_with_explicit_kind("StructArgSingle", "StructArgSingle")
        .node_with_explicit_kind("StructArgTail", "StructArgTail")
    )
    .add_separated_list("StructArgList", "StructArg", "TerminalComma")
    .add_struct(StructBuilder::new("ArgListBraced")
        .node("lbrace", "TerminalLBrace")
        .node("arguments", "ArgList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("ArgListBracketed")
        .node("lbrack", "TerminalLBrack")
        .node("arguments", "ArgList")
        .node("rbrack", "TerminalRBrack")
    )
    .add_enum(EnumBuilder::new("WrappedArgList")
        .missing("Missing")
        .node_with_explicit_kind("BracketedArgList", "ArgListBracketed")
        .node_with_explicit_kind("ParenthesizedArgList", "ArgListParenthesized")
        .node_with_explicit_kind("BracedArgList", "ArgListBraced")
    )
    .add_struct(StructBuilder::new("WrappedArgListMissing"))
    // ---Patterns ---
    // TODO(spapini): Support "Or" patterns (e.g. 1 | 2).
    // TODO(spapini): Support tuple patterns (e.g. (x, _)).
    .add_enum(EnumBuilder::new("Pattern")
        .node_with_explicit_kind("Underscore", "TerminalUnderscore")
        .node_with_explicit_kind("Literal", "TerminalLiteralNumber")
        .node_with_explicit_kind("False", "TerminalFalse")
        .node_with_explicit_kind("True", "TerminalTrue")
        .node_with_explicit_kind("ShortString", "TerminalShortString")
        .node_with_explicit_kind("String", "TerminalString")
        .node("Identifier")
        .node("Struct")
        .node("Tuple")
        .node("Enum")
        .node("FixedSizeArray")
        .node_with_explicit_kind("Path", "ExprPath")
    )
    .add_struct(StructBuilder::new("PatternIdentifier")
        .node("modifiers", "ModifierList")
        .key_node("name", "TerminalIdentifier")
    )
    .add_struct(StructBuilder::new("PatternStruct")
        // TODO(spapini): Use SimplePath instead - which is not an expr.
        .node("path", "ExprPath")
        .node("lbrace", "TerminalLBrace")
        .node("params", "PatternStructParamList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_separated_list("PatternStructParamList", "PatternStructParam", "TerminalComma")
    .add_struct(StructBuilder::new("PatternTuple")
        .node("lparen", "TerminalLParen")
        .node("patterns", "PatternList")
        .node("rparen", "TerminalRParen")
    )
    .add_struct(StructBuilder::new("PatternFixedSizeArray")
        .node("lbrack", "TerminalLBrack")
        .node("patterns", "PatternList")
        .node("rbrack", "TerminalRBrack"))
    .add_separated_list("PatternList", "Pattern", "TerminalComma")
    .add_separated_list("PatternListOr", "Pattern", "TerminalOr")
    .add_enum(EnumBuilder::new("PatternStructParam")
        .node_with_explicit_kind("Single", "PatternIdentifier")
        .node("WithExpr")
        .node_with_explicit_kind("Tail", "TerminalDotDot")
    )
    .add_struct(StructBuilder::new("PatternStructParamWithExpr")
        .node("modifiers", "ModifierList")
        .node("name", "TerminalIdentifier")
        .node("colon", "TerminalColon")
        .node("pattern", "Pattern")
    )
    .add_struct(StructBuilder::new("PatternEnum")
        .node("path", "ExprPath")
        .node("pattern", "OptionPatternEnumInnerPattern")
    )
    .add_struct(StructBuilder::new("PatternEnumInnerPattern")
        .node("lparen", "TerminalLParen")
        .node("pattern", "Pattern")
        .node("rparen", "TerminalRParen")
    )
    .add_option("PatternEnumInnerPattern")
    // --- Type clauses ---
    // TODO(yuval): support SimpleExpr instead of Expr
    .add_struct(StructBuilder::new("TypeClause").node("colon", "TerminalColon").node("ty", "Expr"))
    .add_option("TypeClause")
    .add_struct(StructBuilder::new("ReturnTypeClause")
        .node("arrow", "TerminalArrow")
        .node("ty", "Expr")
    )
    .add_option("ReturnTypeClause")
    // --- Statements ---
    .add_enum(EnumBuilder::new("Statement")
        .missing("Missing")
        .node("Let")
        .node("Expr")
        .node("Continue")
        .node("Return")
        .node("Break")
        .node("Item")
    )
    .add_list("StatementList", "Statement")
    .add_struct(StructBuilder::new("StatementMissing"))
    .add_struct(StructBuilder::new("StatementLet")
        .node("attributes" ,"AttributeList")
        .node("let_kw", "TerminalLet")
        .key_node("pattern", "Pattern")
        .node("type_clause", "OptionTypeClause")
        .node("eq", "TerminalEq")
        .node("rhs", "Expr")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_option("TerminalSemicolon")
    .add_struct(StructBuilder::new("StatementExpr")
        .node("attributes" ,"AttributeList")
        .node("expr", "Expr")
        .node("semicolon", "OptionTerminalSemicolon")
    )
    .add_struct(StructBuilder::new("StatementContinue")
        .node("attributes" ,"AttributeList")
        .node("continue_kw", "TerminalContinue")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ExprClause")
        .node("expr", "Expr"))
    .add_option("ExprClause")
    .add_struct(StructBuilder::new("StatementReturn")
        .node("attributes" ,"AttributeList")
        .node("return_kw", "TerminalReturn")
        .node("expr_clause", "OptionExprClause")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("StatementBreak")
        .node("attributes" ,"AttributeList")
        .node("break_kw", "TerminalBreak")
        .node("expr_clause", "OptionExprClause")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("StatementItem")
        .node("item", "ModuleItem")
    )
    // --- Functions ---
    .add_struct(StructBuilder::new("Param")
        .node("modifiers", "ModifierList")
        .key_node("name", "TerminalIdentifier")
        .node("type_clause", "OptionTypeClause")
    )
    .add_list("ModifierList", "Modifier")
    .add_enum(EnumBuilder::new("Modifier")
        .node_with_explicit_kind("Ref", "TerminalRef")
        .node_with_explicit_kind("Mut", "TerminalMut")
    )
    .add_separated_list("ParamList", "Param", "TerminalComma")
    .add_struct(StructBuilder::new("ImplicitsClause")
        .node("implicits_kw", "TerminalImplicits")
        .node("lparen", "TerminalLParen")
        .node("implicits", "ImplicitsList")
        .node("rparen", "TerminalRParen")
    )
    .add_separated_list("ImplicitsList", "ExprPath", "TerminalComma")
    .add_option("ImplicitsClause")
    .add_option("TerminalNoPanic")
    .add_option("TerminalConst")
    // TODO(spapini): Add generic params.
    // This is an unnamed signature, e.g. "() -> Type".
    .add_struct(StructBuilder::new("FunctionSignature")
        .node("lparen", "TerminalLParen")
        .node("parameters", "ParamList")
        .node("rparen", "TerminalRParen")
        .node("ret_ty", "OptionReturnTypeClause")
        .node("implicits_clause", "OptionImplicitsClause")
        .node("optional_no_panic", "OptionTerminalNoPanic")
    )
    // --- Struct Members ---
    // Struct member and enum variant have the same structure.
    .add_struct(StructBuilder::new("Member")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .key_node("name", "TerminalIdentifier")
        .node("type_clause", "TypeClause")
    )
    .add_separated_list("MemberList", "Member", "TerminalComma")
    .add_struct(StructBuilder::new("Variant")
        .node("attributes" ,"AttributeList")
        .key_node("name", "TerminalIdentifier")
        .node("type_clause", "OptionTypeClause")
    )
    .add_separated_list("VariantList", "Variant", "TerminalComma")
    // --- Items ---
    .add_enum(EnumBuilder::new("ModuleItem")
        .missing("Missing")
        .node_with_explicit_kind("Constant", "ItemConstant")
        .node_with_explicit_kind("Module", "ItemModule")
        .node_with_explicit_kind("Use", "ItemUse")
        .node_with_explicit_kind("FreeFunction", "FunctionWithBody")
        .node_with_explicit_kind("ExternFunction", "ItemExternFunction")
        .node_with_explicit_kind("ExternType", "ItemExternType")
        .node_with_explicit_kind("Trait", "ItemTrait")
        .node_with_explicit_kind("Impl", "ItemImpl")
        .node_with_explicit_kind("ImplAlias", "ItemImplAlias")
        .node_with_explicit_kind("Struct", "ItemStruct")
        .node_with_explicit_kind("Enum", "ItemEnum")
        .node_with_explicit_kind("TypeAlias", "ItemTypeAlias")
        .node_with_explicit_kind("InlineMacro", "ItemInlineMacro")
        .node_with_explicit_kind("HeaderDoc", "ItemHeaderDoc")
    )
    .add_list("ModuleItemList", "ModuleItem")
    .add_struct(StructBuilder::new("ModuleItemMissing"))
    .add_struct(StructBuilder::new("Attribute")
        .node("hash", "TerminalHash")
        .node("lbrack", "TerminalLBrack")
        .node("attr", "ExprPath")
        .node("arguments", "OptionArgListParenthesized")
        .node("rbrack", "TerminalRBrack")
    )
    .add_list("AttributeList", "Attribute")
    .add_struct(StructBuilder::new("VisibilityDefault"))
    .add_struct(StructBuilder::new("VisibilityPubArgumentClause")
        .node("lparen", "TerminalLParen")
        .node("argument", "TerminalIdentifier")
        .node("rparen", "TerminalRParen")
    )
    .add_option("VisibilityPubArgumentClause")
    .add_struct(StructBuilder::new("VisibilityPub")
        .node("pub_kw", "TerminalPub")
        .node("argument_clause", "OptionVisibilityPubArgumentClause")
    )
    .add_enum(EnumBuilder::new("Visibility")
        .node("Default")
        .node("Pub")
    )
    .add_struct(StructBuilder::new("ItemModule")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("module_kw", "TerminalModule")
        .key_node("name", "TerminalIdentifier")
        .node("body", "MaybeModuleBody")
    )
    .add_enum(EnumBuilder::new("MaybeModuleBody")
        .node_with_explicit_kind("Some", "ModuleBody")
        .node_with_explicit_kind("None", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ModuleBody")
        .node("lbrace", "TerminalLBrace")
        .node("items", "ModuleItemList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("FunctionDeclaration")
        .node("optional_const", "OptionTerminalConst")
        .node("function_kw", "TerminalFunction")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("signature", "FunctionSignature")
    )
    .add_struct(StructBuilder::new("ItemConstant")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("const_kw", "TerminalConst")
        .key_node("name", "TerminalIdentifier")
        .node("type_clause", "TypeClause")
        .node("eq", "TerminalEq")
        .node("value", "Expr")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("FunctionWithBody")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
         // TODO(ilya): Use only the name as key node.
        .key_node("declaration", "FunctionDeclaration")
        .node("body", "ExprBlock")
    )
    .add_struct(StructBuilder::new("ItemExternFunction")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("extern_kw", "TerminalExtern")
         // TODO(ilya): Use only the name as key node.
        .key_node("declaration", "FunctionDeclaration")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ItemExternType")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("extern_kw", "TerminalExtern")
        .node("type_kw", "TerminalType")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ItemTrait")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("trait_kw", "TerminalTrait")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("body", "MaybeTraitBody")
    )
    .add_enum(EnumBuilder::new("MaybeTraitBody")
        .node_with_explicit_kind("Some", "TraitBody")
        .node_with_explicit_kind("None", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("TraitBody")
        .node("lbrace", "TerminalLBrace")
        .node("items", "TraitItemList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_list("TraitItemList", "TraitItem")
    .add_enum(EnumBuilder::new("TraitItem")
        .missing("Missing")
        .node("Function")
        .node("Type")
        .node("Constant")
        .node("Impl")
    )
    .add_struct(StructBuilder::new("TraitItemMissing"))
    .add_struct(StructBuilder::new("TraitItemFunction")
        .node("attributes" ,"AttributeList")
         // TODO(ilya): Use only the name as key node.
        .key_node("declaration", "FunctionDeclaration")
        .node("body", "MaybeTraitFunctionBody")
    )
    .add_struct(StructBuilder::new("TraitItemType")
        .node("attributes" ,"AttributeList")
        .node("type_kw", "TerminalType")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("TraitItemConstant")
        .node("attributes" ,"AttributeList")
        .node("const_kw", "TerminalConst")
        .key_node("name", "TerminalIdentifier")
        .node("type_clause", "TypeClause")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("TraitItemImpl")
        .node("attributes" ,"AttributeList")
        .node("impl_kw", "TerminalImpl")
        .key_node("name", "TerminalIdentifier")
        .node("colon", "TerminalColon")
        .node("trait_path", "ExprPath")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_enum(EnumBuilder::new("MaybeTraitFunctionBody")
        .node_with_explicit_kind("Some", "ExprBlock")
        .node_with_explicit_kind("None", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ItemImpl")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("impl_kw", "TerminalImpl")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("of_kw", "TerminalOf")
        .node("trait_path", "ExprPath")
        .node("body", "MaybeImplBody")
    ).add_struct(StructBuilder::new("ItemInlineMacro")
        .node("attributes" ,"AttributeList")
        .node("name", "TerminalIdentifier")
        .node("bang", "TerminalNot")
        .node("arguments", "WrappedArgList")
        .node("semicolon", "TerminalSemicolon")
    )
    // Empty struct, which is used for separating the first comments in a file or module from the
    // rest of the items. This is needed to prevent the first comments from being moved by the
    // formatter.
    .add_struct(StructBuilder::new("ItemHeaderDoc")
        .node("empty", "TerminalEmpty"))
    .add_enum(EnumBuilder::new("MaybeImplBody")
        .node_with_explicit_kind("Some", "ImplBody")
        .node_with_explicit_kind("None", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ImplBody")
            .node("lbrace", "TerminalLBrace")
            .node("items", "ImplItemList")
            .node("rbrace", "TerminalRBrace")
    )
    .add_list("ImplItemList", "ImplItem")
    .add_enum(EnumBuilder::new("ImplItem")
        .missing("Missing")
        .node_with_explicit_kind("Function", "FunctionWithBody")
        .node_with_explicit_kind("Type", "ItemTypeAlias")
        .node_with_explicit_kind("Constant", "ItemConstant")
        .node_with_explicit_kind("Impl", "ItemImplAlias")
        // These are not supported semantically.
        .node_with_explicit_kind("Module", "ItemModule")
        .node_with_explicit_kind("Use", "ItemUse")
        .node_with_explicit_kind("ExternFunction", "ItemExternFunction")
        .node_with_explicit_kind("ExternType", "ItemExternType")
        .node_with_explicit_kind("Trait", "ItemTrait")
        .node_with_explicit_kind("Struct", "ItemStruct")
        .node_with_explicit_kind("Enum", "ItemEnum")
    )
    .add_struct(StructBuilder::new("ImplItemMissing"))
    .add_struct(StructBuilder::new("ItemImplAlias")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("impl_kw", "TerminalImpl")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("eq", "TerminalEq")
        .node("impl_path", "ExprPath")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ItemStruct")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("struct_kw", "TerminalStruct")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("lbrace", "TerminalLBrace")
        .node("members", "MemberList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("ItemEnum")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("enum_kw", "TerminalEnum")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("lbrace", "TerminalLBrace")
        .node("variants", "VariantList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("ItemTypeAlias")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("type_kw", "TerminalType")
        .key_node("name", "TerminalIdentifier")
        .node("generic_params", "OptionWrappedGenericParamList")
        .node("eq", "TerminalEq")
        .node("ty", "Expr")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_struct(StructBuilder::new("ItemUse")
        .node("attributes" ,"AttributeList")
        .node("visibility", "Visibility")
        .node("use_kw", "TerminalUse")
        .key_node("use_path", "UsePath")
        .node("semicolon", "TerminalSemicolon")
    )
    .add_enum(
        EnumBuilder::new("UsePath")
        .node("Leaf")
        .node("Single")
        .node("Multi")
        .node("Star")
    )
    .add_struct(StructBuilder::new("UsePathLeaf")
        .key_node("ident", "PathSegment")
        .key_node("alias_clause", "OptionAliasClause")
    )
    .add_struct(StructBuilder::new("UsePathSingle")
        .node("ident", "PathSegment")
        .node("colon_colon", "TerminalColonColon")
        .node("use_path", "UsePath")
    )
    .add_struct(StructBuilder::new("UsePathMulti")
        .node("lbrace", "TerminalLBrace")
        .node("use_paths", "UsePathList")
        .node("rbrace", "TerminalRBrace")
    )
    .add_struct(StructBuilder::new("UsePathStar")
        .node("star", "TerminalMul")
    )
    .add_separated_list("UsePathList", "UsePath", "TerminalComma")
    .add_struct(StructBuilder::new("AliasClause")
        .node("as_kw", "TerminalAs")
        .key_node("alias", "TerminalIdentifier")
    )
    .add_option("AliasClause")
    // --- Generics ---
    .add_enum(
        EnumBuilder::new("GenericArg")
        .node("Unnamed")
        .node("Named")
    )
    .add_struct(StructBuilder::new("GenericArgNamed")
        .node("name", "TerminalIdentifier")
        .node("colon", "TerminalColon")
        .node("value", "GenericArgValue")
    )
    .add_struct(StructBuilder::new("GenericArgUnnamed")
        .node("value", "GenericArgValue")
    )
    .add_enum(
        EnumBuilder::new("GenericArgValue")
        .node("Expr")
        .node_with_explicit_kind("Underscore", "TerminalUnderscore")
    )
    .add_struct(StructBuilder::new("GenericArgValueExpr")
        .node("expr", "Expr")
    )
    .add_struct(StructBuilder::new("GenericArgs")
        .node("langle", "TerminalLT")
        .node("generic_args", "GenericArgList")
        .node("rangle", "TerminalGT")
    )
    .add_separated_list("GenericArgList", "GenericArg", "TerminalComma")
    .add_struct(
        StructBuilder::new("AssociatedItemConstraint")
        .node("item","TerminalIdentifier")
        .node("colon","TerminalColon")
        .node("value","Expr")
    )
    .add_struct(StructBuilder::new("AssociatedItemConstraints")
        .node("lbrack", "TerminalLBrack")
        .node("associated_item_constraints", "AssociatedItemConstraintList")
        .node("rbrack", "TerminalRBrack")
    )
    .add_separated_list("AssociatedItemConstraintList", "AssociatedItemConstraint", "TerminalComma")
    .add_option("AssociatedItemConstraints")
    .add_option("WrappedGenericParamList")
    .add_struct(StructBuilder::new("WrappedGenericParamList")
        .node("langle", "TerminalLT")
        .node("generic_params", "GenericParamList")
        .node("rangle", "TerminalGT")
    )
    .add_separated_list("GenericParamList", "GenericParam", "TerminalComma")
    // TODO(spapini): Remove this indirection.
    .add_enum(EnumBuilder::new("GenericParam")
        .node("Type")
        .node("Const")
        .node("ImplNamed")
        .node("ImplAnonymous")
        .node("NegativeImpl")
    )
    .add_struct(StructBuilder::new("GenericParamType")
        .key_node("name", "TerminalIdentifier")
    )
    .add_struct(StructBuilder::new("GenericParamConst")
        .node("const_kw", "TerminalConst")
        .key_node("name", "TerminalIdentifier")
        .node("colon", "TerminalColon")
        .node("ty", "Expr")
    )
    .add_struct(StructBuilder::new("GenericParamImplNamed")
        .node("impl_kw", "TerminalImpl")
        .key_node("name", "TerminalIdentifier")
        .node("colon", "TerminalColon")
        .node("trait_path", "ExprPath")
        .node("type_constrains", "OptionAssociatedItemConstraints")
    )
    .add_struct(StructBuilder::new("GenericParamImplAnonymous")
        .node("plus", "TerminalPlus")
        .node("trait_path", "ExprPath")
        .node("type_constrains", "OptionAssociatedItemConstraints")
    )
    .add_struct(StructBuilder::new("GenericParamNegativeImpl")
        .node("minus", "TerminalMinus")
        .node("trait_path", "ExprPath")
    )
    // --- Skipped nodes ---
    // A wrapper for the skipped node enum as the enum nodes (Trivium) can't have enum as a child directly.
    .add_struct(StructBuilder::new("TriviumSkippedNode")
        .node("node", "SkippedNode")
    )
    // An enum of all nodes that can be skipped while parsing, new nodes can be added here if needed to be skipped.
    .add_enum(EnumBuilder::new("SkippedNode")
        .node_with_explicit_kind("AttributeList", "AttributeList")
        .node_with_explicit_kind("VisibilityPub", "VisibilityPub")
    )
    // --- Tokens + Terminals ---
    .add_token_and_terminal("Identifier")
    .add_token_and_terminal("LiteralNumber")
    .add_token_and_terminal("ShortString")
    .add_token_and_terminal("String")
    .add_keyword_token_and_terminal("As")
    .add_keyword_token_and_terminal("Const")
    .add_keyword_token_and_terminal("Else")
    .add_keyword_token_and_terminal("Enum")
    .add_keyword_token_and_terminal("Extern")
    .add_keyword_token_and_terminal("False")
    .add_keyword_token_and_terminal("Function")
    .add_keyword_token_and_terminal("If")
    .add_keyword_token_and_terminal("While")
    .add_keyword_token_and_terminal("For")
    .add_keyword_token_and_terminal("Loop")
    .add_keyword_token_and_terminal("Impl")
    .add_keyword_token_and_terminal("Implicits")
    .add_keyword_token_and_terminal("Let")
    .add_keyword_token_and_terminal("Match")
    .add_keyword_token_and_terminal("Module")
    .add_keyword_token_and_terminal("Mut")
    .add_keyword_token_and_terminal("NoPanic")
    .add_keyword_token_and_terminal("Of")
    .add_keyword_token_and_terminal("Ref")
    .add_keyword_token_and_terminal("Continue")
    .add_keyword_token_and_terminal("Return")
    .add_keyword_token_and_terminal("Break")
    .add_keyword_token_and_terminal("Struct")
    .add_keyword_token_and_terminal("Trait")
    .add_keyword_token_and_terminal("True")
    .add_keyword_token_and_terminal("Type")
    .add_keyword_token_and_terminal("Use")
    .add_keyword_token_and_terminal("Pub")
    .add_token_and_terminal("And")
    .add_token_and_terminal("AndAnd")
    .add_token_and_terminal("Arrow")
    .add_token_and_terminal("At")
    .add_token_and_terminal("BadCharacters")
    .add_token_and_terminal("Colon")
    .add_token_and_terminal("ColonColon")
    .add_token_and_terminal("Comma")
    .add_token_and_terminal("Div")
    .add_token_and_terminal("DivEq")
    .add_token_and_terminal("Dot")
    .add_token_and_terminal("DotDot")
    .add_token_and_terminal("DotDotEq")
    .add_token_and_terminal("EndOfFile")
    .add_token_and_terminal("Eq")
    .add_token_and_terminal("EqEq")
    .add_token_and_terminal("GE")
    .add_token_and_terminal("GT")
    .add_token_and_terminal("Hash")
    .add_token_and_terminal("LBrace")
    .add_token_and_terminal("LBrack")
    .add_token_and_terminal("LE")
    .add_token_and_terminal("LParen")
    .add_token_and_terminal("LT")
    .add_token_and_terminal("MatchArrow")
    .add_token_and_terminal("Minus")
    .add_token_and_terminal("MinusEq")
    .add_token_and_terminal("Mod")
    .add_token_and_terminal("ModEq")
    .add_token_and_terminal("Mul")
    .add_token_and_terminal("MulEq")
    .add_token_and_terminal("Neq")
    .add_token_and_terminal("Not")
    .add_token_and_terminal("BitNot")
    .add_token_and_terminal("Or")
    .add_token_and_terminal("OrOr")
    .add_token_and_terminal("Plus")
    .add_token_and_terminal("PlusEq")
    .add_token_and_terminal("QuestionMark")
    .add_token_and_terminal("RBrace")
    .add_token_and_terminal("RBrack")
    .add_token_and_terminal("RParen")
    .add_token_and_terminal("Semicolon")
    .add_token_and_terminal("Underscore")
    .add_token_and_terminal("Xor")
    // --- Meta ---
    .add_struct(StructBuilder::new("SyntaxFile")
        .node("items", "ModuleItemList")
        .node("eof", "TerminalEndOfFile")
    )
    .add_token_and_terminal("Empty")
    .add_token("SingleLineComment")
    .add_token("SingleLineInnerComment")
    .add_token("SingleLineDocComment")
    .add_token("Whitespace")
    .add_token("Newline")
    .add_token("Missing")
    .add_token("Skipped")
    .add_all_tokens_enum("TokenNode")
    .get()
}
