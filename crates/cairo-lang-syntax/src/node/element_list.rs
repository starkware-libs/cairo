use std::marker::PhantomData;

use salsa::Database;

use crate::node::{SyntaxNode, TypedSyntaxNode};

// A typed view of an element list node.
// STEP=1 means a sequence of elements (e.g. sequence of trivia elements).
// STEP=2 means a separated sequence (e.g. argument list separated by `,`).
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct ElementList<'db, T: TypedSyntaxNode<'db>, const STEP: usize> {
    pub node: SyntaxNode<'db>,
    phantom: PhantomData<T>,
}
impl<'db, T: TypedSyntaxNode<'db>, const STEP: usize> ElementList<'db, T, STEP> {
    pub fn new(node: SyntaxNode<'db>) -> Self {
        Self { node, phantom: PhantomData {} }
    }
}
impl<'db, T: TypedSyntaxNode<'db>> ElementList<'db, T, 1> {
    pub fn elements_vec(&self, db: &'db dyn Database) -> Vec<T> {
        self.elements(db).collect()
    }
    pub fn elements(
        &self,
        db: &'db dyn Database,
    ) -> impl ExactSizeIterator<Item = T> + DoubleEndedIterator + use<'db, T> + 'db {
        self.node.get_children(db).map(|x| T::from_syntax_node(db, x))
    }
    pub fn has_tail(&self, _db: &dyn Database) -> bool {
        false
    }
}
impl<'db, T: TypedSyntaxNode<'db>> ElementList<'db, T, 2> {
    pub fn elements_vec(&self, db: &'db dyn Database) -> Vec<T> {
        self.elements(db).collect()
    }
    pub fn elements(
        &self,
        db: &'db dyn Database,
    ) -> impl ExactSizeIterator<Item = T> + DoubleEndedIterator + use<'db, T> + 'db {
        self.node.get_children(db).step_by(2).map(|x| T::from_syntax_node(db, x))
    }
    pub fn has_tail(&self, db: &dyn Database) -> bool {
        !self.node.get_children(db).len().is_multiple_of(2)
    }
}
