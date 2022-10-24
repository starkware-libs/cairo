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

/// A tool to aggregate/gather nodes in various forms and eventually emit them as a vector.
#[derive(Default)]
pub struct NodesAggregator {
    nodes: Vec<Node>,
}
impl NodesAggregator {
    /// Gets all the aggregated nodes.
    pub fn get(self) -> Vec<Node> {
        self.nodes
    }

    /// Adds a struct node.
    pub fn add_struct(mut self, builder: StructBuilder) -> Self {
        self.nodes.push(builder.build());
        self
    }

    /// Adds an enum node.
    pub fn add_enum(mut self, builder: EnumBuilder) -> Self {
        self.nodes.push(builder.build());
        self
    }

    /// Adds a node for a list of syntax elements.
    pub fn add_list(mut self, name: &str, element_type: &str) -> Self {
        self.nodes.push(Node {
            name: name.into(),
            kind: NodeKind::List { element_type: element_type.into() },
        });
        self
    }

    /// Adds a node for a list of syntax elements separated by a terminal.
    pub fn add_separated_list(
        mut self,
        name: &str,
        element_type: &str,
        separator_type: &str,
    ) -> Self {
        self.nodes.push(Node {
            name: name.into(),
            kind: NodeKind::SeparatedList {
                element_type: element_type.into(),
                separator_type: separator_type.into(),
            },
        });
        self
    }

    /// Adds a node for a token node (similar to an empty struct).
    pub fn add_token(mut self, pure_name: &str) -> Self {
        self.nodes.push(Node { name: format!("Token{pure_name}"), kind: NodeKind::Token });
        self
    }

    /// Adds a node for a token node (similar to an empty struct).
    pub fn add_terminal(self, pure_name: &str) -> Self {
        self.add_struct(
            StructBuilder::new_terminal(format!("Terminal{pure_name}").as_str())
                .node("leading_trivia", "Trivia")
                .node("token", format!("Token{pure_name}").as_str())
                .node("trailing_trivia", "Trivia"),
        )
    }

    /// Adds a token node and a terminal node of the relevant names. e.g. for pure_name="Identifier"
    /// it creates TokenIdentifier and TerminalIdentifier.
    pub fn add_token_and_terminal(self, pure_name: &str) -> Self {
        self.add_token(pure_name).add_terminal(pure_name)
    }

    /// Adds an enum node for an option with 2 variants: empty and non-empty. Creates the empty
    /// struct to be used for the empty variant. The Type for the non-empty variant is `name`
    /// and it should exist independently of this call.
    ///
    /// For example, for name=TypeClause, creates an enum OptionTypeClause with variants
    /// Empty(OptionTypeClauseEmpty) and TypeClause(TypeClause), where OptionTypeClauseEmpty is
    /// created here and TypeClause should exist independently.
    pub fn add_option(self, name: &str) -> Self {
        self.add_enum(
            EnumBuilder::new(format!("Option{name}").as_str())
                .node("Empty")
                .node_with_explicit_kind(name, name),
        )
        .add_struct(StructBuilder::new(format!("Option{name}Empty").as_str()))
    }
}
