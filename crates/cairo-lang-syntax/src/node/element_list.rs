use std::marker::PhantomData;
use std::sync::Arc;

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
}
impl<T: TypedSyntaxNode> ElementList<T, 1> {
    pub fn elements_vec(&self, db: &dyn SyntaxGroup) -> Vec<T> {
        self.elements(db).collect()
    }
    pub fn elements<'a>(
        &self,
        db: &'a dyn SyntaxGroup,
    ) -> impl ExactSizeIterator<Item = T> + DoubleEndedIterator + 'a {
        ElementListRawIter::new(self.node.get_children(db)).map(move |x| T::from_syntax_node(db, x))
    }
    pub fn has_tail(&self, _db: &dyn SyntaxGroup) -> bool {
        false
    }
}
impl<T: TypedSyntaxNode> ElementList<T, 2> {
    pub fn elements_vec(&self, db: &dyn SyntaxGroup) -> Vec<T> {
        self.elements(db).collect()
    }
    pub fn elements<'a>(
        &self,
        db: &'a dyn SyntaxGroup,
    ) -> impl ExactSizeIterator<Item = T> + DoubleEndedIterator + 'a {
        ElementListRawIter::new(self.node.get_children(db))
            .step_by(2)
            .map(move |x| T::from_syntax_node(db, x))
    }
    pub fn has_tail(&self, db: &dyn SyntaxGroup) -> bool {
        !self.node.get_children(db).len().is_multiple_of(2)
    }
}

/// Iterator over the raw elements of an `ElementList`.
struct ElementListRawIter {
    /// The `Arc` storing the actual node.
    _data: Arc<[SyntaxNode]>,
    /// Actual iterator over the elements.
    iter: std::slice::Iter<'static, SyntaxNode>,
}

impl ElementListRawIter {
    fn new(data: Arc<[SyntaxNode]>) -> Self {
        // We leak the Arc to get a 'static reference, and keep the Arc in the struct to avoid
        // leaks.
        let ptr: *const [SyntaxNode] = Arc::as_ptr(&data);
        let slice: &'static [SyntaxNode] = unsafe { std::mem::transmute(&*ptr) };
        let iter = slice.iter();
        Self { _data: data, iter }
    }
}

impl Iterator for ElementListRawIter {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().copied()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
impl ExactSizeIterator for ElementListRawIter {
    fn len(&self) -> usize {
        self.iter.len()
    }
}
impl DoubleEndedIterator for ElementListRawIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().copied()
    }
}
