use super::ids::GreenId;
use super::kind::SyntaxKind;
use crate::token;

pub struct GreenNodeInternalBuilder {
    kind: SyntaxKind,
    children: Vec<GreenId>,
    width: u32,
}
impl GreenNodeInternalBuilder {
    pub fn init(kind: SyntaxKind) -> Self {
        Self { kind, children: vec![], width: 0 }
    }
    pub fn init_from(node: GreenNodeInternal) -> Self {
        Self { kind: node.kind, children: node.children, width: node.width }
    }
    pub fn set_kind(mut self, kind: SyntaxKind) -> Self {
        self.kind = kind;
        self
    }
    pub fn add_child(mut self, child: GreenId, width: u32) -> Self {
        self.children.push(child);
        self.width += width;
        self
    }
    pub fn build(self) -> GreenNodeInternal {
        GreenNodeInternal { kind: self.kind, children: self.children, width: self.width }
    }
}

/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNodeInternal {
    pub kind: SyntaxKind,
    pub children: Vec<GreenId>,
    pub width: u32,
}
impl GreenNodeInternal {
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }
    pub fn child_at(&self, i: usize) -> GreenId {
        self.children[i]
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GreenNode {
    Internal(GreenNodeInternal),
    Token(token::Token),
}
impl GreenNode {
    pub fn width(&self) -> u32 {
        match self {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }
}
