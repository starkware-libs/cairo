// Representation of the AST specifications.
pub struct Node {
    pub name: String,
    pub kind: NodeKind,
}
pub enum NodeKind {
    Enum { variants: Vec<Member>, missing_variant: Option<String> },
    Struct { members: Vec<Member> },
    List { element_type: String },
    SeparatedList { element_type: String },
}
#[derive(Clone)]
pub struct Member {
    pub name: String,
    pub kind: MemberKind,
}
#[derive(Clone)]
pub enum MemberKind {
    Token,
    Node(String),
}
struct StructBuilder {
    name: String,
    members: Vec<Member>,
}
impl StructBuilder {
    fn new(name: &'static str) -> Self {
        Self { name: name.into(), members: Vec::new() }
    }
    fn node(mut self, field: &'static str, name: &'static str) -> StructBuilder {
        self.members.push(Member { name: field.into(), kind: MemberKind::Node(name.into()) });
        self
    }
    fn token(mut self, field: &'static str) -> StructBuilder {
        self.members.push(Member { name: field.into(), kind: MemberKind::Token });
        self
    }
    fn build(self) -> Node {
        Node { name: self.name, kind: NodeKind::Struct { members: self.members } }
    }
}
struct EnumBuilder {
    name: String,
    variants: Vec<Member>,
    missing_variant: Option<Member>,
}
impl EnumBuilder {
    fn new(name: &'static str) -> Self {
        Self { name: name.into(), variants: Vec::new(), missing_variant: None }
    }
    fn missing(mut self, name: &'static str) -> EnumBuilder {
        self.missing_variant =
            Some(Member { name: name.into(), kind: MemberKind::Node(name.into()) });
        self
    }
    // TODO(spapini): Separate variant name and node name, so we would get for example:
    //   Unary(ExprUnary),
    fn node(mut self, name: &'static str) -> EnumBuilder {
        self.variants.push(Member { name: name.into(), kind: MemberKind::Node(name.into()) });
        self
    }
    fn token(mut self, name: &'static str) -> EnumBuilder {
        self.variants.push(Member { name: name.into(), kind: MemberKind::Token });
        self
    }
    fn build(mut self) -> Node {
        if let Some(member) = &self.missing_variant {
            self.variants.push(member.clone());
        }
        Node {
            name: self.name,
            kind: NodeKind::Enum {
                variants: self.variants,
                missing_variant: self.missing_variant.map(|x| x.name),
            },
        }
    }
}

fn list_node(name: &'static str, element_type: &'static str) -> Node {
    Node { name: name.into(), kind: NodeKind::List { element_type: element_type.into() } }
}
fn separated_list_node(name: &'static str, element_type: &'static str) -> Node {
    Node { name: name.into(), kind: NodeKind::SeparatedList { element_type: element_type.into() } }
}

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
            .token("TriviumSingleLineComment")
            .token("TriviumWhitespace")
            .token("TriviumNewline")
            .build(),
        StructBuilder::new("TriviumSingleLineComment").token("token").build(),
        StructBuilder::new("TriviumWhitespace").token("token").build(),
        StructBuilder::new("TriviumNewline").token("token").build(),
        // Expressions.
        StructBuilder::new("Identifier").node("terminal", "Terminal").build(),
        EnumBuilder::new("Expr")
            .missing("ExprMissing")
            .node("ExprPath")
            .node("ExprLiteral")
            .node("ExprParenthesized")
            .node("ExprUnary")
            .node("ExprBinary")
            .build(),
        StructBuilder::new("ExprMissing").build(),
        separated_list_node("ExprPath", "PathSegment"),
        StructBuilder::new("PathSegment")
            .node("ident", "Identifier")
            .node("args", "OptionGenericArgs")
            .build(),
        StructBuilder::new("EmptyGenericArgs").build(),
        EnumBuilder::new("OptionGenericArgs").node("EmptyGenericArgs").node("GenericArgs").build(),
        separated_list_node("GenericArgs", "Expr"),
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
            .missing("StatementMissing")
            .node("StatementLet")
            .node("StatementExpr")
            .node("StatementReturn")
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
            .node("ItemModule")
            .node("ItemFunction")
            .node("ItemTrait")
            .node("ItemImpl")
            .node("ItemStruct")
            .node("ItemEnum")
            .node("ItemUse")
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
        StructBuilder::new("ItemFunctionPrototype")
            .node("signature", "FunctionSignature")
            .node("semi", "Terminal")
            .build(),
        // This will allow defining prototypes.
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
