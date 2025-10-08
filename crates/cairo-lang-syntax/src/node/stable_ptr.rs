use salsa::Database;

use super::ids::GreenId;
use crate::node::SyntaxNode;

/// Stable pointer to a node in the syntax tree.
///
/// Has enough information to uniquely define a node in the AST, given the tree.
/// Has undefined behavior when used with the wrong tree.
/// This is not a real pointer in the low-level sense, just a representation of the path from the
/// root to the node.
/// Stable means that when the AST is changed, pointers of unchanged items tend to stay the same.
/// For example, if a function is changed, the pointer of an unrelated function in the AST should
/// remain the same, as much as possible.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub struct SyntaxStablePtr<'a>(SyntaxNode<'a>);

impl<'a> SyntaxStablePtr<'a> {
    pub fn new(node: SyntaxNode<'a>) -> Self {
        Self(node)
    }

    pub fn key_fields(&self, db: &'a dyn Database) -> &'a [GreenId<'a>] {
        self.0.key_fields(db)
    }
}
