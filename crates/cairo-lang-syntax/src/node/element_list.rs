use std::marker::PhantomData;

use super::SyntaxGroup;
use crate::node::{SyntaxNode, TypedSyntaxNode};

// A typed view of an element list node.
// STEP=1 means a sequence of elements (e.g. sequence of trivia elements).
// STEP=2 means a separated sequence (e.g. argument list separated by `,`).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ElementList<'a, T: TypedSyntaxNode<'a>, const STEP: usize> {
    pub node: SyntaxNode<'a>,
    phantom: PhantomData<T>,
}
impl<'a, T: TypedSyntaxNode<'a>, const STEP: usize> ElementList<'a, T, STEP> {
    pub fn new(node: SyntaxNode<'a>) -> Self {
        Self { node, phantom: PhantomData {} }
    }
    pub fn elements(&self, db: &'a dyn SyntaxGroup) -> Vec<T> {
        self.node
            .get_children(db)
            .iter()
            .step_by(STEP)
            .map(|x| T::from_syntax_node(db, *x))
            .collect()
    }
    pub fn has_tail(&'a self, db: &'a dyn SyntaxGroup) -> bool {
        self.node.get_children(db).len() % STEP != 0
    }
}
