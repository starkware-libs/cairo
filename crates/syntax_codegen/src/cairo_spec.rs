use crate::spec::{list_node, separated_list_node, EnumBuilder, Node, StructBuilder};

// The specific syntax specification of Cairo.
pub fn get_spec() -> Vec<Node> {
    let nodes = vec![
        // --- Terminal ---
        StructBuilder::new("Terminal")
            .node("leading_trivia", "Trivia")
            .token("token")
            .node("trailing_trivia", "Trivia")
            .build(),
        // TODO(yuval): consider removing the trivia from the skipped terminal and instead append
        // it to the trivia of the real terminal that the skipped terminal is a trivium of.
        StructBuilder::new("TriviumSkippedTerminal")
            .node("leading_trivia", "Trivia")
            .token("token")
            .node("trailing_trivia", "Trivia")
            .build(),
        list_node("Trivia", "Trivium"),
        EnumBuilder::new("Trivium")
            .token("SingleLineComment")
            .token("Whitespace")
            .token("Newline")
            .node("SkippedTerminal")
            .build(),
        // --- Function calls ---
        StructBuilder::new("StructArgExpr").node("colon", "Terminal").node("expr", "Expr").build(),
        EnumBuilder::new("OptionStructArgExpr")
            .node("Empty")
            .node_with_explicit_kind("Some", "StructArgExpr")
            .build(),
        StructBuilder::new("OptionStructArgExprEmpty").build(),
        StructBuilder::new("StructArgSingle")
            .node("identifier", "Identifier")
            .node("arg_expr", "OptionStructArgExpr")
            .build(),
        StructBuilder::new("StructArgTail")
            .node("dotdot", "Terminal")
            .node("expression", "Expr")
            .build(),
        EnumBuilder::new("StructArg")
            .node_with_explicit_kind("StructArgSingle", "StructArgSingle")
            .node_with_explicit_kind("StructArgTail", "StructArgTail")
            .build(),
        separated_list_node("StructArgList", "StructArg"),
        StructBuilder::new("ArgListBraced")
            .node("lbrace", "Terminal")
            .node("arguments", "StructArgList")
            .node("rbrace", "Terminal")
            .build(),
        // --- Expressions ---
        StructBuilder::new("Identifier").node("terminal", "Terminal").build(),
        EnumBuilder::new("Expr")
            .missing("Missing")
            .node("Path")
            .node("Literal")
            .node("Parenthesized")
            .node("Unary")
            .node("Binary")
            .node("Tuple")
            .node("FunctionCall")
            .node("StructCtorCall")
            .node("Block")
            .build(),
        separated_list_node("ExprList", "Expr"),
        StructBuilder::new("ExprMissing").build(),
        EnumBuilder::new_option("OptionGenericArgs").build(),
        StructBuilder::new("OptionGenericArgsNone").build(),
        // TODO(spapini): Add SimpleExpr.
        separated_list_node("OptionGenericArgsSome", "Expr"),
        StructBuilder::new("PathSegment")
            .node("ident", "Identifier")
            .node("generic_args", "OptionGenericArgs")
            .build(),
        separated_list_node("ExprPath", "PathSegment"),
        StructBuilder::new("ExprLiteral").node("terminal", "Terminal").build(),
        StructBuilder::new("ExprParenthesized")
            .node("lparen", "Terminal")
            .node("expr", "Expr")
            .node("rparen", "Terminal")
            .build(),
        StructBuilder::new("ExprUnary").node("op", "Terminal").node("expr", "Expr").build(),
        StructBuilder::new("ExprBinary")
            .node("lhs", "Expr")
            .node("op", "Terminal")
            .node("rhs", "Expr")
            .build(),
        StructBuilder::new("ExprTuple")
            .node("lparen", "Terminal")
            .node("expressions", "ExprList")
            .node("rparen", "Terminal")
            .build(),
        StructBuilder::new("ExprListParenthesized")
            .node("lparen", "Terminal")
            .node("expressions", "ExprList")
            .node("rparen", "Terminal")
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
            .node("lbrace", "Terminal")
            .node("statements", "StatementList")
            .node("rbrace", "Terminal")
            .build(),
        // --- Type clauses ---
        // TODO(yuval): support SimpleExpr instead of ExprPath
        StructBuilder::new("TypeClause").node("colon", "Terminal").node("ty", "ExprPath").build(),
        // TODO(yuval): refactor ::new_option to have the relevant kind directly as a child, like
        // here.
        EnumBuilder::new("OptionTypeClause")
            .node("Empty")
            .node_with_explicit_kind("TypeClause", "TypeClause")
            .build(),
        StructBuilder::new("OptionTypeClauseEmpty").build(),
        StructBuilder::new("ReturnTypeClause")
            .node("arrow", "Terminal")
            .node("ty", "ExprPath")
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
            .node("letkw", "Terminal")
            .node("lhs", "Identifier")
            .node("type_clause", "OptionTypeClause")
            .node("eq", "Terminal")
            .node("rhs", "Expr")
            .node("semi", "Terminal")
            .build(),
        EnumBuilder::new("OptionSemicolon")
            .node("Empty")
            .node_with_explicit_kind("Some", "Terminal")
            .build(),
        StructBuilder::new("OptionSemicolonEmpty").build(),
        StructBuilder::new("StatementExpr")
            .node("expr", "Expr")
            .node("semi", "OptionSemicolon")
            .build(),
        StructBuilder::new("StatementReturn")
            .node("returnkw", "Terminal")
            .node("expr", "Expr")
            .node("semi", "Terminal")
            .build(),
        // --- Parameters and Functions ---
        StructBuilder::new("Param")
            .node("identifier", "Terminal")
            .node("type_clause", "TypeClause")
            .build(),
        separated_list_node("ParamList", "Param"),
        StructBuilder::new("ParamListParenthesized")
            .node("lparen", "Terminal")
            .node("parameters", "ParamList")
            .node("rparen", "Terminal")
            .build(),
        StructBuilder::new("ParamListBraced")
            .node("lbrace", "Terminal")
            .node("parameters", "ParamList")
            .node("rbrace", "Terminal")
            .build(),
        // TODO(spapini): Add generic params.
        StructBuilder::new("FunctionSignature")
            .node("funckw", "Terminal")
            .node("name", "Identifier")
            .node("lparen", "Terminal")
            .node("parameters", "ParamList")
            .node("rparen", "Terminal")
            .node("ret_ty", "ReturnTypeClause")
            .build(),
        // --- Items ---
        EnumBuilder::new("Item")
            .node("Module")
            .node("Function")
            .node("FunctionSignature")
            .node("Trait")
            .node("Impl")
            .node("Struct")
            .node("Enum")
            .node("Use")
            .build(),
        list_node("ItemList", "Item"),
        StructBuilder::new("ItemModule")
            .node("modkw", "Terminal")
            .node("name", "Identifier")
            .node("semi", "Terminal")
            .build(),
        StructBuilder::new("ItemFunction")
            .node("signature", "FunctionSignature")
            .node("body", "ExprBlock")
            .build(),
        StructBuilder::new("ItemFunctionSignature")
            .node("signature", "FunctionSignature")
            .node("semi", "Terminal")
            .build(),
        // TODO(spapini): consider having specific ItemLists here.
        StructBuilder::new("ItemTrait")
            .node("traitkw", "Terminal")
            .node("name", "Identifier")
            .node("lbrace", "Terminal")
            .node("items", "ItemList")
            .node("rbrace", "Terminal")
            .build(),
        StructBuilder::new("ItemImpl")
            .node("implkw", "Terminal")
            .node("name", "Identifier")
            .node("forkw", "Terminal")
            .node("trait_name", "Identifier")
            .node("lbrace", "Terminal")
            .node("items", "ItemList")
            .node("rbrace", "Terminal")
            .build(),
        StructBuilder::new("ItemStruct")
            .node("structkw", "Terminal")
            .node("name", "Identifier")
            .node("lbrace", "Terminal")
            .node("members", "ParamList")
            .node("rbrace", "Terminal")
            .build(),
        StructBuilder::new("ItemEnum")
            .node("enumkw", "Terminal")
            .node("name", "Identifier")
            .node("body", "ParamListBraced")
            .build(),
        StructBuilder::new("ItemUse")
            .node("usekw", "Terminal")
            .node("path", "ExprPath")
            .node("semi", "Terminal")
            .build(),
        // Meta.
        StructBuilder::new("SyntaxFile").node("items", "ItemList").node("eof", "Terminal").build(),
    ];
    nodes
}
