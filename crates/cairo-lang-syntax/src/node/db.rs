use cairo_lang_filesystem::ids::{FileId, Tracked};
use cairo_lang_filesystem::span::TextOffset;
use itertools::Either;
use salsa::Database;

use super::SyntaxNode;
use super::ids::GreenId;
use crate::node::new_syntax_node;

pub trait SyntaxGroup: Database {
    /// Query for caching [SyntaxNode::get_children].
    fn get_children<'db>(&'db self, node: SyntaxNode<'db>) -> &'db [SyntaxNode<'db>] {
        get_children(self.as_dyn_database(), (), node)
    }

    /// Tracked function for creating syntax nodes to have it also interned.
    fn create_syntax_node<'db>(
        &'db self,
        green: GreenId<'db>,
        offset: TextOffset,
        parent: Either<SyntaxNode<'db>, FileId<'db>>,
        index: usize,
    ) -> SyntaxNode<'db> {
        new_syntax_node(self.as_dyn_database(), green, offset, parent, index)
    }
}
impl<T: Database + ?Sized> SyntaxGroup for T {}

#[salsa::tracked(returns(ref))]
fn get_children<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    node: SyntaxNode<'db>,
) -> Vec<SyntaxNode<'db>> {
    node.get_children_impl(db)
}
