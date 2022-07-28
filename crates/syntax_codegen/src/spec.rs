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
    pub fn new(name: &'static str) -> Self {
        Self { name: name.into(), members: Vec::new() }
    }
    pub fn node(mut self, field: &'static str, kind: &'static str) -> StructBuilder {
        self.members.push(Member { name: field.into(), kind: MemberKind::Node(kind.into()) });
        self
    }
    pub fn token(mut self, field: &'static str) -> StructBuilder {
        self.members.push(Member { name: field.into(), kind: MemberKind::Token("".into()) });
        self
    }
    pub fn build(self) -> Node {
        Node { name: self.name, kind: NodeKind::Struct { members: self.members } }
    }
}
/// Builds spec for an enum node.
pub struct EnumBuilder {
    name: String,
    variants: Vec<Member>,
    missing_variant: Option<Member>,
}
impl EnumBuilder {
    pub fn new(name: &'static str) -> Self {
        Self { name: name.into(), variants: Vec::new(), missing_variant: None }
    }
    pub fn new_option(name: &'static str) -> Self {
        Self { name: name.into(), variants: Vec::new(), missing_variant: None }
            .node("None")
            .node("Some")
    }
    pub fn missing(mut self, name: &'static str) -> EnumBuilder {
        let name = self.name.clone() + name;
        self.missing_variant = Some(Member { name: name.clone(), kind: MemberKind::Node(name) });
        self
    }
    // TODO(spapini): Separate variant name and node name, so we would get for example:
    //   Unary(ExprUnary),
    pub fn node(mut self, name: &'static str) -> EnumBuilder {
        let name = self.name.clone() + name;
        self.variants.push(Member { name: name.clone(), kind: MemberKind::Node(name) });
        self
    }
    pub fn token(mut self, name: &'static str) -> EnumBuilder {
        self.variants
            .push(Member { name: self.name.clone() + name, kind: MemberKind::Token(name.into()) });
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
pub fn list_node(name: &'static str, element_type: &'static str) -> Node {
    Node { name: name.into(), kind: NodeKind::List { element_type: element_type.into() } }
}
/// Builds spec for a list of syntax elements separated by a terminal.
pub fn separated_list_node(name: &'static str, element_type: &'static str) -> Node {
    Node { name: name.into(), kind: NodeKind::SeparatedList { element_type: element_type.into() } }
}
