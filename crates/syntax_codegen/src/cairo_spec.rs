use crate::spec::{list_node, separated_list_node, EnumBuilder, Node, StructBuilder};

// The specific syntax specification of Cairo.
pub fn get_spec() -> Vec<Node> {
    let nodes = vec![
        // Terminal.
        StructBuilder::new("Terminal")
            .node("leading_trivia", "Trivia")
            .token("token")
            .node("trailing_trivia", "Trivia")
            .build(),
        StructBuilder::new("SkippedTerminal")
            .node("leading_trivia", "Trivia")
            .token("token")
            .node("trailing_trivia", "Trivia")
            .build(),
        list_node("Trivia", "Trivium"),
        EnumBuilder::new("Trivium")
            .token("SingleLineComment")
            .token("Whitespace")
            .token("Newline")
            .build(),
        // Expressions.
        StructBuilder::new("Identifier").node("terminal", "Terminal").build(),
        EnumBuilder::new("Expr")
            .missing("Missing")
            .node("Path")
            .node("Literal")
            .node("Parenthesized")
            .node("Unary")
            .node("Binary")
            .node("FunctionCall")
            .node("ConstructorCall")
            .node("Tuple")
            .node("Block")
            .build(),
        separated_list_node("ExprList", "Expr"),
        StructBuilder::new("ExprMissing").build(),
        separated_list_node("ExprPath", "PathSegment"),
        StructBuilder::new("PathSegment")
            .node("ident", "Identifier")
            .node("args", "OptionGenericArgs")
            .build(),
        EnumBuilder::new_option("OptionGenericArgs").build(),
        StructBuilder::new("OptionGenericArgsNone").build(),
        // TODO(spapini): Add SimpleExpr.
        separated_list_node("OptionGenericArgsSome", "Expr"),
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
        StructBuilder::new("ExprBlock")
            .node("lbrace", "Terminal")
            .node("statements", "StatementList")
            .node("rbrace", "Terminal")
            .build(),
        // Statements.
        list_node("StatementList", "Statement"),
        EnumBuilder::new("Statement")
            .missing("Missing")
            .node("Let")
            .node("Expr")
            .node("Return")
            .build(),
        StructBuilder::new("StatementMissing").build(),
        StructBuilder::new("StatementLet")
            .node("letkw", "Terminal")
            .node("lhs", "Identifier")
            .node("typeclause", "OptionTypeClause")
            .node("eq", "Terminal")
            .node("rhs", "Expr")
            .node("semi", "Terminal")
            .build(),
        StructBuilder::new("StatementExpr")
            .node("expr", "Expr")
            .node("semi", "OptionSemicolon")
            .build(),
        EnumBuilder::new("OptionSemicolon")
            .node("Empty")
            .node_with_existing_kind("Some", "Terminal")
            .build(),
        StructBuilder::new("OptionSemicolonEmpty").build(),
        StructBuilder::new("StatementReturn")
            .node("returnkw", "Terminal")
            .node("expr", "Expr")
            .node("semi", "Terminal")
            .build(),
        // Items.
        list_node("ItemList", "Item"),
        EnumBuilder::new("Item")
            .node("Module")
            .node("Function")
            .node("Trait")
            .node("Impl")
            .node("Struct")
            .node("Enum")
            .node("Use")
            .build(),
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
        // TODO(spapini): Add generic params.
        StructBuilder::new("FunctionSignature")
            .node("funckw", "Terminal")
            .node("name", "Identifier")
            .node("parameters", "ParamListParenthesized")
            .node("ret_ty", "ReturnTypeClause")
            .build(),
        separated_list_node("ParameterList", "Parameter"),
        StructBuilder::new("Parameter")
            .node("name", "Identifier")
            .node("colon", "Terminal")
            .node("ty", "Expr")
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
            .node("members", "MemberList")
            .node("rbrace", "Terminal")
            .build(),
        StructBuilder::new("ItemEnum")
            .node("enumkw", "Terminal")
            .node("name", "Identifier")
            .node("lbrace", "Terminal")
            .node("members", "MemberList")
            .node("rbrace", "Terminal")
            .build(),
        separated_list_node("MemberList", "Member"),
        StructBuilder::new("Member")
            .node("name", "Identifier")
            .node("colon", "Terminal")
            .node("ty", "Expr")
            .build(),
        StructBuilder::new("ItemUse")
            .node("usekw", "Terminal")
            .node("path", "ExprPath")
            .node("semi", "Terminal")
            .build(),
        StructBuilder::new("ExprTuple")
            .node("lparen", "Terminal")
            .node("expressions", "ExprList")
            .node("rparen", "Terminal")
            .build(),
        StructBuilder::new("ArgExpr").node("colon", "Terminal").node("expr", "Expr").build(),
        EnumBuilder::new("OptionArgExpr")
            .node("Empty")
            .node_with_existing_kind("Some", "ArgExpr")
            .build(),
        StructBuilder::new("OptionArgExprEmpty").build(),
        StructBuilder::new("Arg")
            .node("identifier", "Identifier")
            .node("arg_expr", "OptionArgExpr")
            .build(),
        StructBuilder::new("ExprFunctionCall")
            .node("path", "ExprPath")
            .node("arguments", "ExprListParenthesized")
            .build(),
        StructBuilder::new("StructUpdateTail")
            .node("dotdot", "Terminal")
            .node("expression", "Expr")
            .build(),
        EnumBuilder::new("CtorArg")
            .node_with_existing_kind("Arg", "Arg")
            .node_with_existing_kind("StructUpdateTail", "StructUpdateTail")
            .build(),
        separated_list_node("CtorArgList", "CtorArg"),
        StructBuilder::new("ArgListBraced")
            .node("lbrace", "Terminal")
            .node("arguments", "CtorArgList")
            .node("rbrace", "Terminal")
            .build(),
        StructBuilder::new("ExprConstructorCall")
            .node("path", "ExprPath")
            .node("arguments", "ArgListBraced")
            .build(),
        StructBuilder::new("ExprListParenthesized")
            .node("lparen", "Terminal")
            .node("expressions", "ExprList")
            .node("rparen", "Terminal")
            .build(),
        // TODO(yuval): support SimpleExpr instead
        StructBuilder::new("TypeClause").node("colon", "Terminal").node("ty", "ExprPath").build(),
        // TODO(yg): Integrate the direct thing into ::new_option
        EnumBuilder::new("OptionTypeClause")
            .node("Empty")
            .node_with_existing_kind("TypeClause", "TypeClause")
            .build(),
        StructBuilder::new("OptionTypeClauseEmpty").build(),
        StructBuilder::new("ReturnTypeClause")
            .node("arrow", "Terminal")
            .node("ty", "ExprPath")
            .build(),
        EnumBuilder::new("OptionReturnTypeClause")
            .node("Empty")
            .node_with_existing_kind("ReturnTypeClause", "ReturnTypeClause")
            .build(),
        StructBuilder::new("OptionReturnTypeClauseEmpty").build(),
        StructBuilder::new("Param")
            .node("identifier", "Terminal")
            .node("typeclause", "OptionTypeClause")
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
        // Meta.
        StructBuilder::new("SyntaxFile").node("items", "ItemList").node("eof", "Terminal").build(),
    ];
    nodes
}
