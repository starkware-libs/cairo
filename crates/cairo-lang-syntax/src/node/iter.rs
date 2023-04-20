use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use cairo_lang_filesystem::span::TextOffset;

use crate::node::db::SyntaxGroup;
use crate::node::ids::GreenId;
use crate::node::key_fields::get_key_fields;
use crate::node::kind::SyntaxKind;
use crate::node::stable_ptr::SyntaxStablePtr;
use crate::node::{SyntaxNode, SyntaxNodeInner};

pub struct SyntaxNodeChildIterator<'db> {
    db: &'db dyn SyntaxGroup,
    node: SyntaxNode,
    green_iterator: vec::IntoIter<GreenId>,
    /// The current offset in the source file of the start of the child.
    offset: TextOffset,
    /// Mapping from (kind, key_fields) to the number of times this indexing pair has been seen.
    /// This is used to maintain the correct index for creating each StablePtr.
    /// See [`get_key_fields`].
    key_map: HashMap<(SyntaxKind, Vec<GreenId>), usize>,
}

impl<'db> Iterator for SyntaxNodeChildIterator<'db> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        let green_id = self.green_iterator.next()?;
        self.next_inner(green_id)
    }
}

impl<'db> DoubleEndedIterator for SyntaxNodeChildIterator<'db> {
    fn next_back(&mut self) -> Option<<SyntaxNodeChildIterator<'db> as Iterator>::Item> {
        let green_id = self.green_iterator.next_back()?;
        self.next_inner(green_id)
    }
}

impl<'db> ExactSizeIterator for SyntaxNodeChildIterator<'db> {
    fn len(&self) -> usize {
        self.green_iterator.len()
    }
}

impl<'db> SyntaxNodeChildIterator<'db> {
    pub(super) fn new(node: &SyntaxNode, db: &'db dyn SyntaxGroup) -> Self {
        SyntaxNodeChildIterator {
            db,
            node: node.clone(),
            green_iterator: node.green_node(db).children().into_iter(),
            offset: node.offset(),
            key_map: HashMap::new(),
        }
    }

    fn next_inner(
        &mut self,
        green_id: GreenId,
    ) -> Option<<SyntaxNodeChildIterator<'db> as Iterator>::Item> {
        let green = self.db.lookup_intern_green(green_id);
        let width = green.width();
        let kind = green.kind;
        let key_fields: Vec<GreenId> = get_key_fields(kind, green.children());
        let index = match self.key_map.entry((kind, key_fields.clone())) {
            Entry::Occupied(mut entry) => entry.insert(entry.get() + 1),
            Entry::Vacant(entry) => {
                entry.insert(1);
                0
            }
        };
        let stable_ptr = self.db.intern_stable_ptr(SyntaxStablePtr::Child {
            parent: self.node.0.stable_ptr,
            kind,
            key_fields,
            index,
        });
        // Create the SyntaxNode view for the child.
        let res = SyntaxNode(Arc::new(SyntaxNodeInner {
            green: green_id,
            offset: self.offset,
            parent: Some(self.node.clone()),
            stable_ptr,
        }));
        self.offset = self.offset.add_width(width);
        Some(res)
    }
}

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
    db: &'a dyn SyntaxGroup,
    // FIXME(mkaput): Is it possible to avoid allocating iterators in layers here?
    //   This code does it because without fast parent & prev/next sibling operations it has to
    //   maintain DFS trace.
    layers: Vec<PreorderLayer<'a>>,
}

struct PreorderLayer<'a> {
    start: SyntaxNode,
    children: Option<SyntaxNodeChildIterator<'a>>,
}

impl<'a> Preorder<'a> {
    pub(super) fn new(start: SyntaxNode, db: &'a dyn SyntaxGroup) -> Self {
        let initial_layer = PreorderLayer::<'a> { start, children: None };

        // NOTE(mkaput): Reserving some capacity should help amortization and thus make this
        // iterator more performant. This wasn't benchmarked though and the capacity is just an
        // educated guess, based on typical depth of syntax files in test suites.
        let mut layers = Vec::with_capacity(32);
        layers.push(initial_layer);

        Self { db, layers }
    }
}

impl<'a> Iterator for Preorder<'a> {
    type Item = WalkEvent<SyntaxNode>;

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
                let event = WalkEvent::Enter(layer.start.clone());
                layer.children = Some(layer.start.children(self.db));
                self.layers.push(layer);
                Some(event)
            }
            Some(mut iter) => match iter.next() {
                None => {
                    // #2: If children iterator is exhausted, this means iteration of start node
                    // just finished, and the layer needs to be popped (i.e. not pushed back) and
                    // leave event for this node needs to be emitted.
                    Some(WalkEvent::Leave(layer.start.clone()))
                }
                Some(start) => {
                    // #3: Otherwise the iterator is just in the middle of visiting a child, so push
                    // a new layer to iterate it. To avoid recursion, step #1 is duplicated and
                    // inlined here.
                    let event = WalkEvent::Enter(start.clone());
                    let new_layer =
                        PreorderLayer { children: Some(start.children(self.db)), start };
                    layer.children = Some(iter);
                    self.layers.extend([layer, new_layer]);
                    Some(event)
                }
            },
        }
    }
}
