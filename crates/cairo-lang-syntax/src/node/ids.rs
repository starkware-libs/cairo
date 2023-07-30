use cairo_lang_filesystem::ids::FileId;
use cairo_lang_utils::define_short_id;

use super::db::SyntaxGroup;
pub use super::green::GreenId;
use super::SyntaxNode;
use crate::node::stable_ptr::SyntaxStablePtr;

define_short_id!(SyntaxStablePtrId, SyntaxStablePtr, SyntaxGroup, lookup_intern_stable_ptr);
impl SyntaxStablePtrId {
    /// Lookups a syntax node using a stable syntax pointer.
    /// Should only be called on the root from which the stable pointer was generated.
    pub fn lookup(&self, db: &dyn SyntaxGroup) -> SyntaxNode {
        let ptr = db.lookup_intern_stable_ptr(*self);
        match ptr {
            SyntaxStablePtr::Root(file_id, green) => SyntaxNode::new_root(db, file_id, green),
            SyntaxStablePtr::Child { parent, .. } => {
                let parent = parent.lookup(db);
                for child in parent.children(db) {
                    if child.stable_ptr() == *self {
                        return child;
                    }
                }
                unreachable!();
            }
        }
    }
    pub fn file_id(&self, db: &dyn SyntaxGroup) -> FileId {
        let ptr = db.lookup_intern_stable_ptr(*self);
        match ptr {
            SyntaxStablePtr::Root(file_id, _) => file_id,
            SyntaxStablePtr::Child { parent, .. } => parent.file_id(db),
        }
    }
}
