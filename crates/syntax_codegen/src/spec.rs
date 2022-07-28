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
    fn node(mut self, name: &'static str, node: &'static str) -> StructBuilder {
        self.members.push(Member { name: name.into(), kind: MemberKind::Node(node.into()) });
        self
    }
    fn token(mut self, name: &'static str) -> StructBuilder {
        self.members.push(Member { name: name.into(), kind: MemberKind::Token });
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
    fn missing(mut self, node: &'static str) -> EnumBuilder {
        self.missing_variant =
            Some(Member { name: node.into(), kind: MemberKind::Node(node.into()) });
        self
    }
    fn node(mut self, node: &'static str) -> EnumBuilder {
        self.variants.push(Member { name: node.into(), kind: MemberKind::Node(node.into()) });
        self
    }
    fn token(mut self, node: &'static str) -> EnumBuilder {
        self.variants.push(Member { name: node.into(), kind: MemberKind::Token });
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

#[allow(dead_code)]
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
            .node("left", "Expr")
            .node("op", "Terminal")
            .node("right", "Expr")
            .build(),
        StructBuilder::new("ExprBlock")
            .node("left", "Terminal")
            .node("statements", "StatementList")
            .node("right", "Terminal")
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
        StructBuilder::new("Semi").node("token", "Terminal").build(),
        StructBuilder::new("ItemFunction")
            .node("signature", "FunctionSignature")
            .node("body", "ExprBlock")
            .build(),
        StructBuilder::new("ItemFunctionPrototype")
            .node("signature", "FunctionSignature")
            .node("semi", "Terminal")
            .build(),
        // This will allow defining prototypes.
        StructBuilder::new("FunctionSignature")
            .node("funckw", "Terminal")
            .node("name", "Identifier")
            .node("lparen", "Terminal")
            .node("arguments", "ArgumentList")
            .node("rparen", "Terminal")
            .node("arrow", "Terminal")
            .node("ret_ty", "Expr")
            .build(),
        separated_list_node("ArgumentList", "Argument"),
        StructBuilder::new("Argument")
            .node("name", "Identifier")
            .node("colon", "Terminal")
            .node("ty", "Expr")
            .build(),
        StructBuilder::new("ItemTrait")
            .node("kwtrait", "Terminal")
            .node("name", "Identifier")
            .node("left", "Terminal")
            .node("items", "ItemList")
            .node("right", "Terminal")
            .build(),
        StructBuilder::new("ItemImpl")
            .node("kwimpl", "Terminal")
            .node("name", "Identifier")
            .node("of", "Terminal")
            .node("kwtrait", "Identifier")
            .node("kwfor", "Terminal")
            .node("ty", "Identifier")
            .node("left", "Terminal")
            .node("items", "ItemList")
            .node("right", "Terminal")
            .build(),
        StructBuilder::new("ItemStruct")
            .node("kwstruct", "Terminal")
            .node("name", "Identifier")
            .node("left", "Terminal")
            .node("members", "MemberList")
            .node("right", "Terminal")
            .build(),
        StructBuilder::new("ItemEnum")
            .node("kwenum", "Terminal")
            .node("name", "Identifier")
            .node("left", "Terminal")
            .node("members", "MemberList")
            .node("right", "Terminal")
            .build(),
        separated_list_node("MemberList", "Member"),
        StructBuilder::new("Member")
            .node("name", "Identifier")
            .node("colon", "Terminal")
            .node("ty", "Expr")
            .build(),
        StructBuilder::new("ItemUse")
            .node("kwuse", "Terminal")
            .node("path", "ExprPath")
            .node("semi", "Terminal")
            .build(),
        // Meta.
        StructBuilder::new("SyntaxFile").node("items", "ItemList").node("eof", "Terminal").build(),
    ];

    nodes
}
