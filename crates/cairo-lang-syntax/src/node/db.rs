use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::iter::SyntaxNodeChildIterator;
use super::stable_ptr::SyntaxStablePtr;
use super::SyntaxNode;

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> {
    #[salsa::interned]
    fn intern_green(&self, field: Arc<GreenNode>) -> GreenId;
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;

    /// Returns the children of the given node.
    fn get_children(&self, node: SyntaxNode) -> Arc<Vec<SyntaxNode>>;
}

fn get_children(db: &dyn SyntaxGroup, node: SyntaxNode) -> Arc<Vec<SyntaxNode>> {
    Arc::new(SyntaxNodeChildIterator::new(&node, db).collect())
}
