// Representation of the AST specifications.
pub struct Node {
    pub name: String,
    pub kind: NodeKind,
}
#[derive(Clone)]
pub enum NodeKind {
    Enum { variants: Vec<Variant>, missing_variant: Option<String> },
    Struct { members: Vec<Member> },
    List { element_type: String },
    SeparatedList { element_type: String },
}
#[derive(Clone)]
pub struct Member {
    pub name: String,
    pub kind: MemberKind,
    /// Whether this member serves as a key in the stable pointer of this syntax node.
    /// See [`syntax::node::stable_ptr`].
    pub key: bool,
}
#[derive(Clone)]
pub struct Variant {
    pub name: String,
    pub kind: MemberKind,
}
#[derive(Clone)]
pub enum MemberKind {
    Token(String),
    Node(String),
}

// Helpers to build AST specifications.

/// Builds spec for a struct node.
pub struct StructBuilder {
    name: String,
    members: Vec<Member>,
}
impl StructBuilder {
    pub fn new(name: &str) -> Self {
        Self { name: name.into(), members: Vec::new() }
    }
    pub fn node(mut self, field: &str, kind: &str) -> StructBuilder {
        self.members.push(Member {
            name: field.into(),
            kind: MemberKind::Node(kind.into()),
            key: false,
        });
        self
    }
    pub fn token(mut self, field: &str) -> StructBuilder {
        self.members.push(Member {
            name: field.into(),
            kind: MemberKind::Token("".into()),
            key: false,
        });
        self
    }
    pub fn key_node(mut self, field: &str, kind: &str) -> StructBuilder {
        self.members.push(Member {
            name: field.into(),
            kind: MemberKind::Node(kind.into()),
            key: true,
        });
        self
    }
    pub fn key_token(mut self, field: &str) -> StructBuilder {
        self.members.push(Member {
            name: field.into(),
            kind: MemberKind::Token("".into()),
            key: true,
        });
        self
    }
    pub fn build(self) -> Node {
        Node { name: self.name, kind: NodeKind::Struct { members: self.members } }
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
        let name = self.name.clone() + name;
        self.missing_variant = Some(Variant { name: name.clone(), kind: MemberKind::Node(name) });
        self
    }
    pub fn node(self, name: &str) -> EnumBuilder {
        let kind_name = self.name.clone() + name;
        self.node_with_explicit_kind(name, &kind_name)
    }
    pub fn node_with_explicit_kind(mut self, name: &str, kind: &str) -> EnumBuilder {
        self.variants
            .push(Variant { name: name.to_string(), kind: MemberKind::Node(kind.to_string()) });
        self
    }
    pub fn token(mut self, name: &str) -> EnumBuilder {
        self.variants
            .push(Variant { name: self.name.clone() + name, kind: MemberKind::Token(name.into()) });
        self
    }
    pub fn build(mut self) -> Node {
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
/// Builds spec for a list of syntax elements.
pub fn list_node(name: &str, element_type: &str) -> Node {
    Node { name: name.into(), kind: NodeKind::List { element_type: element_type.into() } }
}
/// Builds spec for a list of syntax elements separated by a terminal.
pub fn separated_list_node(name: &str, element_type: &str) -> Node {
    Node { name: name.into(), kind: NodeKind::SeparatedList { element_type: element_type.into() } }
}
