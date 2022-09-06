use db_utils::define_short_id;

use super::db::SyntaxGroup;
use super::green::GreenNode;
use crate::node::stable_ptr::SyntaxStablePtr;
use crate::token::{Token, TokenKind};

define_short_id!(GreenId, GreenNode, SyntaxGroup, lookup_intern_green);
impl GreenId {
    /// Returns a green ID of a missing token.
    pub fn missing_token(db: &dyn SyntaxGroup) -> Self {
        db.intern_green(GreenNode::Token(Token::missing()))
    }

    /// Returns the width of the node of this green id.
    pub fn width(&self, db: &dyn SyntaxGroup) -> u32 {
        match db.lookup_intern_green(*self) {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }

    /// Returns the TokenKind of the node, if this is a token. Otherwise (e.g. if it's an internal
    /// node), returns None.
    pub fn get_token_kind(&self, db: &dyn SyntaxGroup) -> Option<TokenKind> {
        if let GreenNode::Token(token) = db.lookup_intern_green(*self) {
            Some(token.kind)
        } else {
            None
        }
    }
}

define_short_id!(SyntaxStablePtrId, SyntaxStablePtr, SyntaxGroup, lookup_intern_stable_ptr);
