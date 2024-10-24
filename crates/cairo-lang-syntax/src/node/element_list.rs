use std::marker::PhantomData;

use super::SyntaxGroup;
use crate::node::{SyntaxNode, TypedSyntaxNode};

// A typed view of an element list node.
// STEP=1 means a sequence of elements (e.g. sequence of trivia elements).
// STEP=2 means a separated sequence (e.g. argument list separated by `,`).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ElementList<T: TypedSyntaxNode, const STEP: usize> {
    pub node: SyntaxNode,
    phantom: PhantomData<T>,
}
impl<T: TypedSyntaxNode, const STEP: usize> ElementList<T, STEP> {
    pub fn new(node: SyntaxNode) -> Self {
        Self { node, phantom: PhantomData {} }
    }
    pub fn elements(&self, db: &dyn SyntaxGroup) -> Vec<T> {
        db.get_children(self.node.clone())
            .iter()
            .step_by(STEP)
            .map(|x| T::from_syntax_node(db, x.clone()))
            .collect()
    }
    pub fn has_tail(&self, db: &dyn SyntaxGroup) -> bool {
        db.get_children(self.node.clone()).iter().len() % STEP != 0
    }
}
