use salsa::Database;

use super::SyntaxNode;

pub trait SyntaxGroup: Database {
    /// Query for caching [SyntaxNode::get_children].
    fn get_children<'db>(&'db self, node: SyntaxNode<'db>) -> &'db [SyntaxNode<'db>] {
        node.get_children(self.as_dyn_database())
    }
}
impl<T: Database + ?Sized> SyntaxGroup for T {}
