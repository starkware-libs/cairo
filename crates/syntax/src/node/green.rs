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
