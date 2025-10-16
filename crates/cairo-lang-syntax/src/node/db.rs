use cairo_lang_filesystem::ids::Tracked;
use salsa::Database;

use super::{ChildrenIter, SyntaxNode};

pub trait SyntaxGroup: Database {
    /// Query for caching [SyntaxNode::get_children].
    fn get_children<'db>(&'db self, node: SyntaxNode<'db>) -> ChildrenIter<'db> {
        get_children(self.as_dyn_database(), (), node)
    }

    /// Query for caching [SyntaxNode::get_child].
    fn get_child<'db>(&'db self, node: SyntaxNode<'db>, index: usize) -> SyntaxNode<'db> {
        get_child(self.as_dyn_database(), (), node, index)
    }
}
impl<T: Database + ?Sized> SyntaxGroup for T {}

fn get_children<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    node: SyntaxNode<'db>,
) -> ChildrenIter<'db> {
    node.get_children_impl(db)
}

fn get_child<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    node: SyntaxNode<'db>,
    index: usize,
) -> SyntaxNode<'db> {
    node.get_child_impl(db, index)
}
