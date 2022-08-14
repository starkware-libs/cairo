use super::db::GreenInterner;
use super::green::{GreenNode, GreenNodeInternal};
use super::kind::SyntaxKind;
use crate::token::{Token, TokenKind};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenId(salsa::InternId);
impl salsa::InternKey for GreenId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}
impl GreenId {
    /// Builds a new GreenId for a GreenNodeInternal.
    // TODO(yuval): use this in ast.rs
    pub fn new_node(db: &dyn GreenInterner, kind: SyntaxKind, children: Vec<GreenId>) -> Self {
        let width: u32 = children.iter().map(|c| c.width(db)).sum();
        db.intern_green(GreenNode::Internal(GreenNodeInternal { kind, children, width }))
    }
    pub fn missing_token(db: &dyn GreenInterner) -> Self {
        db.intern_green(GreenNode::Token(Token::missing()))
    }

    pub fn width(&self, db: &dyn GreenInterner) -> u32 {
        match db.lookup_intern_green(*self) {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }

    /// Returns the TokenKind of the node, if this is a token. Otherwise (e.g. if it's an internal
    /// node), returns None.
    pub fn get_token_kind(&self, db: &dyn GreenInterner) -> Option<TokenKind> {
        if let GreenNode::Token(token) = db.lookup_intern_green(*self) {
            Some(token.kind)
        } else {
            None
        }
    }
}
