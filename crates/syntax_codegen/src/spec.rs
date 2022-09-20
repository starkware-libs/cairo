// Representation of the AST specifications.
pub struct Node {
    pub name: String,
    pub kind: NodeKind,
}
#[derive(Clone)]
pub enum NodeKind {
    Enum { variants: Vec<Variant>, missing_variant: Option<Variant> },
    Struct { members: Vec<Member> },
    Terminal { members: Vec<Member> },
    List { element_type: String },
    SeparatedList { element_type: String, separator_type: String },
    Token,
}
#[derive(Clone)]
pub struct Member {
    pub name: String,
    pub kind: String,
    /// Whether this member serves as a key in the stable pointer of this syntax node.
    /// See `syntax::node::stable_ptr`.
    pub key: bool,
}
#[derive(Clone)]
pub struct Variant {
    pub name: String,
    pub kind: String,
}

// Helpers to build AST specifications.

/// Builds spec for a token node (similar to an empty struct).
pub fn token_node(pure_name: &str) -> Node {
    Node { name: format!("Token{pure_name}"), kind: NodeKind::Token }
}
/// Builds spec for a token node (similar to an empty struct).
pub fn terminal_node(pure_name: &str) -> Node {
    StructBuilder::new_terminal(format!("Terminal{pure_name}").as_str())
        .node("leading_trivia", "Trivia")
        .node("token", format!("Token{pure_name}").as_str())
        .node("trailing_trivia", "Trivia")
        .build()
}
/// Appends a terminal and a token of the relevant names. e.g. for pure_name="Identifier" it creates
/// TokenIdentifier and TerminalIdentifier.
pub fn append_terminal_and_token(nodes: &mut Vec<Node>, pure_name: &str) {
    nodes.push(token_node(pure_name));
    nodes.push(terminal_node(pure_name));
}

/// Builds spec for a struct node.
pub struct StructBuilder {
    name: String,
    members: Vec<Member>,
    is_terminal: bool,
}
impl StructBuilder {
    pub fn new(name: &str) -> Self {
        Self { name: name.into(), members: Vec::new(), is_terminal: false }
    }
    pub fn new_terminal(name: &str) -> Self {
        Self { name: name.into(), members: Vec::new(), is_terminal: true }
    }
    pub fn node(mut self, field: &str, kind: &str) -> StructBuilder {
        self.members.push(Member { name: field.into(), kind: kind.into(), key: false });
        self
    }
    pub fn key_node(mut self, field: &str, kind: &str) -> StructBuilder {
        self.members.push(Member { name: field.into(), kind: kind.into(), key: true });
        self
    }
    pub fn build(self) -> Node {
        Node {
            name: self.name,
            kind: if self.is_terminal {
                NodeKind::Terminal { members: self.members }
            } else {
                NodeKind::Struct { members: self.members }
            },
        }
    }
}
/// Builds spec for an enum node.
pub struct EnumBuilder {
    name: String,
    variants: Vec<Variant>,
    missing_variant: Option<Variant>,
}
impl EnumBuilder {
    pub fn new(name: &str) -> Self {
        Self { name: name.into(), variants: Vec::new(), missing_variant: None }
    }
    pub fn new_option(name: &str) -> Self {
        Self { name: name.into(), variants: Vec::new(), missing_variant: None }
            .node("None")
            .node("Some")
    }
    pub fn missing(mut self, name: &str) -> EnumBuilder {
        let kind_name = self.name.clone() + name;
        self.missing_variant = Some(Variant { name: name.to_string(), kind: kind_name });
        self
    }
    pub fn node(self, name: &str) -> EnumBuilder {
        let kind_name = self.name.clone() + name;
        self.node_with_explicit_kind(name, &kind_name)
    }
    pub fn node_with_explicit_kind(mut self, name: &str, kind: &str) -> EnumBuilder {
        self.variants.push(Variant { name: name.to_string(), kind: kind.to_string() });
        self
    }
    pub fn build(mut self) -> Node {
        if let Some(member) = &self.missing_variant {
            self.variants.push(member.clone());
        }
        Node {
            name: self.name,
            kind: NodeKind::Enum { variants: self.variants, missing_variant: self.missing_variant },
        }
    }
}
/// Builds spec for a list of syntax elements.
pub fn list_node(name: &str, element_type: &str) -> Node {
    Node { name: name.into(), kind: NodeKind::List { element_type: element_type.into() } }
}
/// Builds spec for a list of syntax elements separated by a terminal.
pub fn separated_list_node(name: &str, element_type: &str, separator_type: &str) -> Node {
    Node {
        name: name.into(),
        kind: NodeKind::SeparatedList {
            element_type: element_type.into(),
            separator_type: separator_type.into(),
        },
    }
}
