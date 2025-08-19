use salsa::Database;

use crate::node::SyntaxNode;

/// `WalkEvent` describes tree walking process.
#[derive(Debug, Copy, Clone)]
pub enum WalkEvent<T> {
    /// Fired before traversing the node.
    Enter(T),
    /// Fired after the node is traversed.
    Leave(T),
}

impl<T> WalkEvent<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WalkEvent<U> {
        match self {
            WalkEvent::Enter(it) => WalkEvent::Enter(f(it)),
            WalkEvent::Leave(it) => WalkEvent::Leave(f(it)),
        }
    }
}

/// Traverse the subtree rooted at the current node (including the current node) in preorder,
/// excluding tokens.
pub struct Preorder<'a> {
    db: &'a dyn Database,
    // FIXME(mkaput): Is it possible to avoid allocating iterators in layers here?
    //   This code does it because without fast parent & prev/next sibling operations it has to
    //   maintain DFS trace.
    layers: Vec<PreorderLayer<'a>>,
}

struct PreorderLayer<'a> {
    start: SyntaxNode<'a>,
    children: Option<(&'a [SyntaxNode<'a>], usize)>,
}

impl<'a> Preorder<'a> {
    pub(super) fn new(start: SyntaxNode<'a>, db: &'a dyn Database) -> Self {
        let initial_layer = PreorderLayer { start, children: None };

        // NOTE(mkaput): Reserving some capacity should help amortization and thus make this
        // iterator more performant. This wasn't benchmarked though and the capacity is just an
        // educated guess, based on typical depth of syntax files in test suites.
        let mut layers = Vec::with_capacity(32);
        layers.push(initial_layer);

        Self { db, layers }
    }
}

impl<'a> Iterator for Preorder<'a> {
    type Item = WalkEvent<SyntaxNode<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        // Lack of layers to traverse means end of iteration, so early return here.
        //
        // The layer is popped here to gain exclusive ownership of it without taking exclusive
        // ownership of the layers stack.
        let mut layer = self.layers.pop()?;
        match layer.children.take() {
            None => {
                // #1: If children iterator is not initialized, this means entire iteration just
                // started, and the enter event for start node has to be emitted.
                let event = WalkEvent::Enter(layer.start);
                layer.children = Some((layer.start.get_children(self.db), 0));
                self.layers.push(layer);
                Some(event)
            }
            Some((nodes, index)) => {
                match nodes.get(index) {
                    None => {
                        // #2: If children iterator is exhausted, this means iteration of start node
                        // just finished, and the layer needs to be popped (i.e. not pushed back)
                        // and leave event for this node needs to be
                        // emitted.
                        Some(WalkEvent::Leave(layer.start))
                    }
                    Some(start) => {
                        // #3: Otherwise the iterator is just in the middle of visiting a child, so
                        // push a new layer to iterate it. To avoid
                        // recursion, step #1 is duplicated and
                        // inlined here.
                        let event = WalkEvent::Enter(*start);
                        let new_layer = PreorderLayer {
                            children: Some((start.get_children(self.db), 0)),
                            start: *start,
                        };
                        layer.children = Some((nodes, index + 1));
                        self.layers.extend([layer, new_layer]);
                        Some(event)
                    }
                }
            }
        }
    }
}
