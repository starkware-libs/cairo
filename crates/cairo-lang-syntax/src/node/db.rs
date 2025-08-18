use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::stable_ptr::SyntaxStablePtr;
use super::{SyntaxNode, SyntaxNodeLongId};

// Salsa database interface.
#[cairo_lang_proc_macros::query_group]
pub trait SyntaxGroup: FilesGroup + for<'a> Upcast<'a, dyn salsa::Database> {
    #[salsa::interned]
    fn intern_green<'a>(&'a self, field: GreenNode<'a>) -> GreenId<'a>;
    #[salsa::interned]
    fn intern_stable_ptr<'a>(&'a self, field: SyntaxStablePtr<'a>) -> SyntaxStablePtrId<'a>;
    #[salsa::interned]
    fn intern_syntax_node<'a>(&'a self, field: SyntaxNodeLongId<'a>) -> SyntaxNode<'a>;

    /// Query for caching [SyntaxNode::get_children].
    #[salsa::transparent]
    fn get_children<'a>(&'a self, node: SyntaxNode<'a>) -> &'a [SyntaxNode<'a>];
}

#[salsa::tracked(returns(ref))]
fn get_children<'a>(db: &'a dyn SyntaxGroup, node: SyntaxNode<'a>) -> Vec<SyntaxNode<'a>> {
    node.get_children_impl(db)
}
