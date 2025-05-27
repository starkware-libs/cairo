use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::stable_ptr::SyntaxStablePtr;
use super::{SyntaxNode, SyntaxNodeLongId};

// Salsa database interface.
#[cairo_lang_proc_macros::query_group]
pub trait SyntaxGroup: FilesGroup {
    #[salsa::interned]
    fn intern_green<'a>(&'a self, field: Arc<GreenNode<'a>>) -> GreenId<'a>;
    #[salsa::interned]
    fn intern_stable_ptr<'a>(&'a self, field: SyntaxStablePtr<'a>) -> SyntaxStablePtrId<'a>;
    #[salsa::interned]
    fn intern_syntax_node<'a>(&'a self, field: SyntaxNodeLongId<'a>) -> SyntaxNode<'a>;

    /// Query for caching [SyntaxNode::get_children].
    fn get_children<'a>(&'a self, node: SyntaxNode<'a>) -> Arc<Vec<SyntaxNode<'a>>>;
}

fn get_children<'a>(db: &'a dyn SyntaxGroup, node: SyntaxNode<'a>) -> Arc<Vec<SyntaxNode<'a>>> {
    node.get_children_impl(db).into()
}
