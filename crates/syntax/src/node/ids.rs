use db_utils::define_short_id;

use super::db::SyntaxGroup;
use super::green::GreenNode;
use crate::node::stable_ptr::SyntaxStablePtr;

define_short_id!(GreenId, GreenNode, SyntaxGroup, lookup_intern_green);
impl GreenId {
    /// Returns the width of the node of this green id.
    pub fn width(&self, db: &dyn SyntaxGroup) -> u32 {
        match db.lookup_intern_green(*self).details {
            super::green::GreenNodeDetails::Token(text) => text.len() as u32,
            super::green::GreenNodeDetails::Node { width, .. } => width,
        }
    }
}

define_short_id!(SyntaxStablePtrId, SyntaxStablePtr, SyntaxGroup, lookup_intern_stable_ptr);
