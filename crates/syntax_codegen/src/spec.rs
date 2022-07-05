// Representation of the AST specifications.

pub struct Node {
    pub name: String,
    pub kind: NodeKind,
}
pub enum NodeKind {
    Enum { variants: Vec<String>, missing_variant: String },
    Struct { members: Vec<Member> },
    List { element_type: String },
    SeparatedList { element_type: String },
}
pub struct Member {
    pub name: String,
    pub kind: MemberKind,
}
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

fn enum_node(
    name: &'static str,
    missing_variant: &'static str,
    mut variants: Vec<&'static str>,
) -> Node {
    variants.push(missing_variant);
    Node {
        name: name.into(),
        kind: NodeKind::Enum {
            missing_variant: missing_variant.into(),
            variants: variants.into_iter().map(String::from).collect(),
        },
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
        // Empty.
        StructBuilder::new("Empty").build(),
        // Terminal.
        StructBuilder::new("Terminal")
            .node("leading_trivia", "Trivia")
            .token("token")
            .node("trailing_trivia", "Trivia")
            .build(),
        list_node("Trivia", "Trivium"),
        enum_node(
            "Trivium",
            "Empty",
            vec!["TriviumSingleLineComment", "TriviumWhitespace", "TriviumNewline"],
        ),
        StructBuilder::new("TriviumSingleLineComment").token("token").build(),
        StructBuilder::new("TriviumWhitespace").token("token").build(),
        StructBuilder::new("TriviumNewline").token("token").build(),
        // Expressions.
        StructBuilder::new("Identifier").node("terminal", "Terminal").build(),
        enum_node(
            "Expr",
            "ExprMissing",
            vec!["ExprPath", "ExprLiteral", "ExprParenthesized", "ExprUnary", "ExprBinary"],
        ),
        separated_list_node("ExprPath", "PathSegment"),
        StructBuilder::new("PathSegment")
            .node("ident", "Identifier")
            .node("args", "OptionGenericArgs")
            .build(),
        enum_node("OptionGenericArgs", "Empty", vec!["GenericArgs"]),
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
        StructBuilder::new("ExprMissing").build(),
    ];

    nodes
}
