use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::stable_ptr::SyntaxStablePtr;
use super::{SyntaxNode, SyntaxNodeLongId};

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> {
    #[salsa::interned]
    fn intern_green(&self, field: Arc<GreenNode>) -> GreenId;
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;
    #[salsa::interned]
    fn intern_syntax_node(&self, field: SyntaxNodeLongId) -> SyntaxNode;

    /// Query for caching [SyntaxNode::get_children].
    fn get_children(&self, node: SyntaxNode) -> Arc<[SyntaxNode]>;
}

fn get_children(db: &dyn SyntaxGroup, node: SyntaxNode) -> Arc<[SyntaxNode]> {
    node.get_children_impl(db).into()
}
