use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;

use super::SyntaxNode;

// Salsa database interface.
#[cairo_lang_proc_macros::query_group]
pub trait SyntaxGroup: FilesGroup + for<'a> Upcast<'a, dyn FilesGroup> {
    /// Query for caching [SyntaxNode::get_children].
    #[salsa::transparent]
    fn get_children<'a>(&'a self, node: SyntaxNode<'a>) -> &'a [SyntaxNode<'a>];
}

#[salsa::tracked(returns(ref))]
fn get_children<'a>(db: &'a dyn SyntaxGroup, node: SyntaxNode<'a>) -> Vec<SyntaxNode<'a>> {
    node.get_children_impl(db)
}
