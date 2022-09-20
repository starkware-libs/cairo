use crate::spec::{
    append_terminal_and_token, list_node, separated_list_node, token_node, EnumBuilder, Node,
    StructBuilder,
};

// The specific syntax specification of Cairo.
pub fn get_spec() -> Vec<Node> {
    let mut nodes = vec![
        // --- Terminal ---
        list_node("Trivia", "Trivium"),
        EnumBuilder::new("Trivium")
            .node_with_explicit_kind("SingleLineComment", "TokenSingleLineComment")
            .node_with_explicit_kind("Whitespace", "TokenWhitespace")
            .node_with_explicit_kind("Newline", "TokenNewline")
            .node_with_explicit_kind("Skipped", "TokenSkipped")
            .build(),
        // --- Function calls ---
        StructBuilder::new("StructArgExpr")
            .node("colon", "TerminalColon")
            .node("expr", "Expr")
            .build(),
        EnumBuilder::new("OptionStructArgExpr")
            .node("Empty")
            .node_with_explicit_kind("Some", "StructArgExpr")
            .build(),
        StructBuilder::new("OptionStructArgExprEmpty").build(),
        StructBuilder::new("StructArgSingle")
            .key_node("identifier", "TerminalIdentifier")
            .node("arg_expr", "OptionStructArgExpr")
            .build(),
        StructBuilder::new("StructArgTail")
            .node("dotdot", "TerminalDotDot")
            .node("expression", "Expr")
            .build(),
        EnumBuilder::new("StructArg")
            .node_with_explicit_kind("StructArgSingle", "StructArgSingle")
            .node_with_explicit_kind("StructArgTail", "StructArgTail")
            .build(),
        separated_list_node("StructArgList", "StructArg", "TerminalComma"),
        StructBuilder::new("ArgListBraced")
            .node("lbrace", "TerminalLBrace")
            .node("arguments", "StructArgList")
            .node("rbrace", "TerminalRBrace")
            .build(),
        // --- Expressions ---
        EnumBuilder::new("Expr")
            .missing("Missing")
            .node("Path")
            .node_with_explicit_kind("Literal", "TerminalLiteralNumber")
            .node_with_explicit_kind("False", "TerminalFalse")
            .node_with_explicit_kind("True", "TerminalTrue")
            .node("Parenthesized")
            .node("Unary")
            .node("Binary")
            .node("Tuple")
            .node("FunctionCall")
            .node("StructCtorCall")
            .node("Block")
            .node("Match")
            .build(),
        separated_list_node("ExprList", "Expr", "TerminalComma"),
        StructBuilder::new("ExprMissing").build(),
        EnumBuilder::new("OptionGenericArgs").node("Empty").node("Some").build(),
        StructBuilder::new("OptionGenericArgsEmpty").build(),
        StructBuilder::new("OptionGenericArgsSome")
            .node("langle", "TerminalLT")
            .node("generic_args", "GenericArgList")
            .node("rangle", "TerminalGT")
            .build(),
        separated_list_node("GenericArgList", "Expr", "TerminalComma"),
        EnumBuilder::new("PathSegment").missing("Ident").node("GenericArgs").build(),
        StructBuilder::new("PathSegmentIdent").node("ident", "TerminalIdentifier").build(),
        StructBuilder::new("PathSegmentGenericArgs")
            .node("generic_args", "OptionGenericArgsSome")
            .build(),
        separated_list_node("ExprPath", "PathSegment", "TerminalColonColon"),
        StructBuilder::new("ExprParenthesized")
            .node("lparen", "TerminalLParen")
            .node("expr", "Expr")
            .node("rparen", "TerminalRParen")
            .build(),
        StructBuilder::new("ExprUnary").node("op", "UnaryOperator").node("expr", "Expr").build(),
        EnumBuilder::new("UnaryOperator")
            .node_with_explicit_kind("Not", "TerminalNot")
            .node_with_explicit_kind("Minus", "TerminalMinus")
            .build(),
        StructBuilder::new("ExprBinary")
            .node("lhs", "Expr")
            .node("op", "BinaryOperator")
            .node("rhs", "Expr")
            .build(),
        EnumBuilder::new("BinaryOperator")
            .node_with_explicit_kind("Dot", "TerminalDot")
            .node_with_explicit_kind("Not", "TerminalNot")
            .node_with_explicit_kind("Mul", "TerminalMul")
            .node_with_explicit_kind("Div", "TerminalDiv")
            .node_with_explicit_kind("Plus", "TerminalPlus")
            .node_with_explicit_kind("Minus", "TerminalMinus")
            .node_with_explicit_kind("EqEq", "TerminalEqEq")
            // TODO(yuval): not yet implemented in parser.
            .node_with_explicit_kind("AndAnd", "TerminalAndAnd")
            .node_with_explicit_kind("OrOr", "TerminalOrOr")
            .node_with_explicit_kind("LE", "TerminalLE")
            .build(),
        StructBuilder::new("ExprTuple")
            .node("lparen", "TerminalLParen")
            .node("expressions", "ExprList")
            .node("rparen", "TerminalRParen")
            .build(),
        StructBuilder::new("ExprListParenthesized")
            .node("lparen", "TerminalLParen")
            .node("expressions", "ExprList")
            .node("rparen", "TerminalRParen")
            .build(),
        StructBuilder::new("ExprFunctionCall")
            .node("path", "ExprPath")
            .node("arguments", "ExprListParenthesized")
            .build(),
        StructBuilder::new("ExprStructCtorCall")
            .node("path", "ExprPath")
            .node("arguments", "ArgListBraced")
            .build(),
        StructBuilder::new("ExprBlock")
            .node("lbrace", "TerminalLBrace")
            .node("statements", "StatementList")
            .node("rbrace", "TerminalRBrace")
            .build(),
        EnumBuilder::new("Pattern")
            .node_with_explicit_kind("Underscore", "TerminalUnderscore")
            // TODO(yuval): support more options.
            .node_with_explicit_kind("Literal", "TerminalLiteralNumber")
            .build(),
        StructBuilder::new("MatchArm")
            .node("pattern", "Pattern")
            .node("arrow", "TerminalMatchArrow")
            .node("expression", "Expr")
            .build(),
        separated_list_node("MatchArms", "MatchArm", "TerminalComma"),
        StructBuilder::new("ExprMatch")
            .node("match_kw", "TerminalMatch")
            // TODO(yuval): change to SimpleExpr
            .node("expr", "Expr")
            .node("lbrace", "TerminalLBrace")
            .node("arms", "MatchArms")
            .node("rbrace", "TerminalRBrace")
            .build(),
        // --- Type clauses ---
        // TODO(yuval): support SimpleExpr instead of ExprPath
        StructBuilder::new("TypeClause").node("colon", "TerminalColon").node("ty", "Expr").build(),
        // TODO(yuval): refactor ::new_option to have the relevant kind directly as a child, like
        // here.
        EnumBuilder::new("OptionTypeClause")
            .node("Empty")
            .node_with_explicit_kind("TypeClause", "TypeClause")
            .build(),
        StructBuilder::new("OptionTypeClauseEmpty").build(),
        StructBuilder::new("ReturnTypeClause")
            .node("arrow", "TerminalArrow")
            .node("ty", "Expr")
            .build(),
        EnumBuilder::new("OptionReturnTypeClause")
            .node("Empty")
            .node_with_explicit_kind("ReturnTypeClause", "ReturnTypeClause")
            .build(),
        StructBuilder::new("OptionReturnTypeClauseEmpty").build(),
        // --- Statements ---
        EnumBuilder::new("Statement")
            .missing("Missing")
            .node("Let")
            .node("Expr")
            .node("Return")
            .build(),
        list_node("StatementList", "Statement"),
        StructBuilder::new("StatementMissing").build(),
        StructBuilder::new("StatementLet")
            .node("let_kw", "TerminalLet")
            .key_node("name", "TerminalIdentifier")
            .node("type_clause", "OptionTypeClause")
            .node("eq", "TerminalEq")
            .node("rhs", "Expr")
            .node("semicolon", "TerminalSemicolon")
            .build(),
        EnumBuilder::new("OptionSemicolon")
            .node("Empty")
            .node_with_explicit_kind("Some", "TerminalSemicolon")
            .build(),
        StructBuilder::new("OptionSemicolonEmpty").build(),
        StructBuilder::new("StatementExpr")
            .node("expr", "Expr")
            .node("semicolon", "OptionSemicolon")
            .build(),
        StructBuilder::new("StatementReturn")
            .node("return_kw", "TerminalReturn")
            .node("expr", "Expr")
            .node("semicolon", "TerminalSemicolon")
            .build(),
        // --- Parameters and Functions ---
        StructBuilder::new("Param")
            .key_node("name", "TerminalIdentifier")
            .node("type_clause", "TypeClause")
            .build(),
        separated_list_node("ParamList", "Param", "TerminalComma"),
        StructBuilder::new("ParamListParenthesized")
            .node("lparen", "TerminalLParen")
            .node("parameters", "ParamList")
            .node("rparen", "TerminalRParen")
            .build(),
        StructBuilder::new("ParamListBraced")
            .node("lbrace", "TerminalLBrace")
            .node("parameters", "ParamList")
            .node("rbrace", "TerminalRBrace")
            .build(),
        // TODO(spapini): Add generic params.
        // This is an unnamed signature, e.g. "() -> Type".
        StructBuilder::new("FunctionSignature")
            .node("lparen", "TerminalLParen")
            .node("parameters", "ParamList")
            .node("rparen", "TerminalRParen")
            .node("ret_ty", "OptionReturnTypeClause")
            .build(),
        // --- Items ---
        EnumBuilder::new("Item")
            .node("Module")
            .node("Use")
            .node("FreeFunction")
            .node("ExternFunction")
            .node("ExternType")
            .node("Trait")
            .node("Impl")
            .node("Struct")
            .node("Enum")
            .build(),
        list_node("ItemList", "Item"),
        StructBuilder::new("ItemModule")
            .node("module_kw", "TerminalModule")
            .key_node("name", "TerminalIdentifier")
            .node("semicolon", "TerminalSemicolon")
            .build(),
        StructBuilder::new("ItemFreeFunction")
            .node("function_kw", "TerminalFunction")
            .key_node("name", "TerminalIdentifier")
            .node("generic_args", "OptionGenericArgs")
            .node("signature", "FunctionSignature")
            .node("body", "ExprBlock")
            .build(),
        StructBuilder::new("ItemExternFunction")
            .node("extern_kw", "TerminalExtern")
            .node("function_kw", "TerminalFunction")
            .key_node("name", "TerminalIdentifier")
            .node("generic_args", "OptionGenericArgs")
            .node("signature", "FunctionSignature")
            .node("semicolon", "TerminalSemicolon")
            .build(),
        StructBuilder::new("ItemExternType")
            .node("extern_kw", "TerminalExtern")
            .node("type_kw", "TerminalType")
            .key_node("name", "TerminalIdentifier")
            .node("generic_args", "OptionGenericArgs")
            .node("semicolon", "TerminalSemicolon")
            .build(),
        // TODO(spapini): consider having specific ItemLists here.
        StructBuilder::new("ItemTrait")
            .node("trait_kw", "TerminalTrait")
            .key_node("name", "TerminalIdentifier")
            .node("generic_args", "OptionGenericArgs")
            .node("lbrace", "TerminalLBrace")
            .node("items", "ItemList")
            .node("rbrace", "TerminalRBrace")
            .build(),
        StructBuilder::new("ItemImpl")
            .node("impl_kw", "TerminalImpl")
            .key_node("name", "TerminalIdentifier")
            .node("generic_args", "OptionGenericArgs")
            .node("for_kw", "TerminalFor")
            .node("trait_name", "TerminalIdentifier")
            .node("lbrace", "TerminalLBrace")
            .node("items", "ItemList")
            .node("rbrace", "TerminalRBrace")
            .build(),
        StructBuilder::new("ItemStruct")
            .node("struct_kw", "TerminalStruct")
            .key_node("name", "TerminalIdentifier")
            .node("generic_args", "OptionGenericArgs")
            .node("lbrace", "TerminalLBrace")
            .node("members", "ParamList")
            .node("rbrace", "TerminalRBrace")
            .build(),
        StructBuilder::new("ItemEnum")
            .node("enum_kw", "TerminalEnum")
            .key_node("name", "TerminalIdentifier")
            .node("body", "ParamListBraced")
            .build(),
        StructBuilder::new("ItemUse")
            .node("use_kw", "TerminalUse")
            .key_node("name", "ExprPath")
            .node("semicolon", "TerminalSemicolon")
            .build(),
        // Meta.
        StructBuilder::new("SyntaxFile")
            .node("items", "ItemList")
            .node("eof", "TerminalEndOfFile")
            .build(),
        token_node("SingleLineComment"),
        token_node("Whitespace"),
        token_node("Newline"),
        token_node("Missing"),
        token_node("Skipped"),
    ];
    // Tokens + Terminals
    append_terminal_and_token(&mut nodes, "Identifier");
    append_terminal_and_token(&mut nodes, "LiteralNumber");
    append_terminal_and_token(&mut nodes, "False");
    append_terminal_and_token(&mut nodes, "True");
    append_terminal_and_token(&mut nodes, "Extern");
    append_terminal_and_token(&mut nodes, "Type");
    append_terminal_and_token(&mut nodes, "Function");
    append_terminal_and_token(&mut nodes, "Module");
    append_terminal_and_token(&mut nodes, "Enum");
    append_terminal_and_token(&mut nodes, "Struct");
    append_terminal_and_token(&mut nodes, "Trait");
    append_terminal_and_token(&mut nodes, "Impl");
    append_terminal_and_token(&mut nodes, "For");
    append_terminal_and_token(&mut nodes, "Let");
    append_terminal_and_token(&mut nodes, "Return");
    append_terminal_and_token(&mut nodes, "Match");
    append_terminal_and_token(&mut nodes, "Use");
    append_terminal_and_token(&mut nodes, "And");
    append_terminal_and_token(&mut nodes, "AndAnd");
    append_terminal_and_token(&mut nodes, "OrOr");
    append_terminal_and_token(&mut nodes, "EqEq");
    append_terminal_and_token(&mut nodes, "Neq");
    append_terminal_and_token(&mut nodes, "GE");
    append_terminal_and_token(&mut nodes, "GT");
    append_terminal_and_token(&mut nodes, "LE");
    append_terminal_and_token(&mut nodes, "LT");
    append_terminal_and_token(&mut nodes, "Not");
    append_terminal_and_token(&mut nodes, "Plus");
    append_terminal_and_token(&mut nodes, "Minus");
    append_terminal_and_token(&mut nodes, "Mul");
    append_terminal_and_token(&mut nodes, "Div");
    append_terminal_and_token(&mut nodes, "Colon");
    append_terminal_and_token(&mut nodes, "ColonColon");
    append_terminal_and_token(&mut nodes, "Comma");
    append_terminal_and_token(&mut nodes, "Dot");
    append_terminal_and_token(&mut nodes, "DotDot");
    append_terminal_and_token(&mut nodes, "Eq");
    append_terminal_and_token(&mut nodes, "Semicolon");
    append_terminal_and_token(&mut nodes, "Underscore");
    append_terminal_and_token(&mut nodes, "LBrace");
    append_terminal_and_token(&mut nodes, "RBrace");
    append_terminal_and_token(&mut nodes, "LBrack");
    append_terminal_and_token(&mut nodes, "RBrack");
    append_terminal_and_token(&mut nodes, "LParen");
    append_terminal_and_token(&mut nodes, "RParen");
    append_terminal_and_token(&mut nodes, "Arrow");
    append_terminal_and_token(&mut nodes, "MatchArrow");
    append_terminal_and_token(&mut nodes, "EndOfFile");
    append_terminal_and_token(&mut nodes, "BadCharacters");
    nodes
}
