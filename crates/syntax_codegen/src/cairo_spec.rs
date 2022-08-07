use crate::spec::{list_node, separated_list_node, EnumBuilder, Node, StructBuilder};

// TODO(spapini): Separate this to another file.
// The specific syntax specification of Cairo.
pub fn get_spec() -> Vec<Node> {
    let nodes = vec![
        // Terminal.
        StructBuilder::new("Terminal")
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
            .build(),
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
        separated_list_node("StatementList", "Statement"),
        EnumBuilder::new("Statement")
            .missing("Missing")
            .node("Let")
            .node("Expr")
            .node("Return")
            .build(),
        StructBuilder::new("StatementMissing").build(),
        // TODO(spapini): Optional type clause.
        StructBuilder::new("StatementLet")
            .node("letkw", "Terminal")
            .node("lhs", "Identifier")
            .node("eq", "Terminal")
            .node("rhs", "Expr")
            .build(),
        StructBuilder::new("StatementExpr").node("expr", "Expr").build(),
        StructBuilder::new("StatementReturn")
            .node("returnkw", "Terminal")
            .node("expr", "Expr")
            .build(),
        // Items.
        separated_list_node("ItemList", "Item"),
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
            .node("lparen", "Terminal")
            .node("parameters", "ParameterList")
            .node("rparen", "Terminal")
            .node("arrow", "Terminal")
            .node("ret_ty", "Expr")
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
        // Meta.
        StructBuilder::new("SyntaxFile").node("items", "ItemList").node("eof", "Terminal").build(),
    ];

    nodes
}
