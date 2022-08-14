use super::ids::GreenId;
use super::kind::SyntaxKind;
use crate::token;

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
    pub fn children(&self) -> Vec<GreenId> {
        self.children.clone()
    }
    pub fn append_children(&mut self, children: &mut Vec<GreenId>) {
        self.children.append(children);
    }
    pub fn modify_child(&mut self, index: usize, new_child: GreenId) {
        self.children[index] = new_child;
    }
    pub fn set_kind(&mut self, kind: SyntaxKind) {
        self.kind = kind;
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
