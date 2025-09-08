use salsa::Database;

use super::SyntaxNode;

pub trait SyntaxGroup: Database {
    /// Query for caching [SyntaxNode::get_children].
    fn get_children<'db>(&'db self, node: SyntaxNode<'db>) -> &'db [SyntaxNode<'db>] {
        get_children(self.as_dyn_database(), node)
    }
}
impl<T: Database + ?Sized> SyntaxGroup for T {}

#[salsa::tracked(returns(ref))]
fn get_children<'db>(db: &'db dyn Database, node: SyntaxNode<'db>) -> Vec<SyntaxNode<'db>> {
    node.get_children_impl(db)
}
