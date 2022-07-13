use std::marker::PhantomData;

use crate::node::{SyntaxNode, TypedSyntaxNode};

use super::GreenInterner;

// A typed view of an element list node.
// STEP=1 means a sequence of elements (e.g. sequence of trivia elements).
// STEP=2 means a separated sequence (e.g. argument list separated by `,`).
pub struct ElementList<T: TypedSyntaxNode, const STEP: usize> {
    node: SyntaxNode,
    phantom: PhantomData<T>,
}
impl<T: TypedSyntaxNode, const STEP: usize> ElementList<T, STEP> {
    pub fn new(node: SyntaxNode) -> Self {
        Self { node, phantom: PhantomData {} }
    }
    pub fn elements(&self, db: &dyn GreenInterner) -> Vec<T> {
        self.node
            .children(db)
            .into_iter()
            .step_by(STEP)
            .map(|x| T::from_syntax_node(db, x))
            .collect()
    }
}
