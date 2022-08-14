use super::db::GreenInterner;
use super::green::{GreenNode, GreenNodeInternal};
use super::kind::SyntaxKind;
use crate::token::TokenKind;

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
    // Builds a new GreenId for a GreenNodeInternal.
    // TODO(yuval): use this in ast.rs
    pub fn new_node(kind: SyntaxKind, children: Vec<GreenId>, db: &dyn GreenInterner) -> Self {
        let width: u32 = children.iter().map(|c| c.width(db)).sum();
        db.intern_green(GreenNode::Internal(GreenNodeInternal { kind, children, width }))
    }

    pub fn width(&self, db: &dyn GreenInterner) -> u32 {
        match db.lookup_intern_green(*self) {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }

    pub fn is_token_missing(&self, db: &dyn GreenInterner) -> bool {
        match db.lookup_intern_green(*self) {
            GreenNode::Token(token) if token.kind == TokenKind::Missing => true,
            _ => false,
        }
    }

    // Returns the TokenKind of the node, if this is a token. Otherwise (e.g. if it's an internal
    // node), returns None.
    pub fn get_token_kind(&self, db: &dyn GreenInterner) -> Option<TokenKind> {
        if let GreenNode::Token(token) = db.lookup_intern_green(*self) {
            Some(token.kind)
        } else {
            None
        }
    }
}
