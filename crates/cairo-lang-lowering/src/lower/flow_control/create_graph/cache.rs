use std::cell::RefCell;

use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::lower::flow_control::graph::{FlowControlGraphBuilder, NodeId};

/// Implements a simple memoization mechanism to optimize the flow control graph that is created.
///
/// The cache is used before calling `BuildNodeCallback` to avoid creating two nodes that behave
/// identically.
pub struct Cache<Input> {
    // A map from input to the cached result.
    //
    // The cache is wrapped in a `RefCell` to allow modifying it without holding a `&mut` to it
    // (which would complicate its usage).
    cache: RefCell<UnorderedHashMap<Input, NodeId>>,
}
impl<Input: std::hash::Hash + Eq + Clone> Cache<Input> {
    /// Calls the callback if this is the first time the input is seen.
    /// Returns the previous result, otherwise.
    pub fn get_or_compute<'db>(
        &self,
        callback: &dyn Fn(&mut FlowControlGraphBuilder<'db>, Input) -> NodeId,
        graph: &mut FlowControlGraphBuilder<'db>,
        input: Input,
    ) -> NodeId {
        if let Some(node_id) = self.cache.borrow().get(&input) {
            return *node_id;
        }

        let node_id = callback(graph, input.clone());
        assert!(!self.cache.borrow().contains_key(&input));
        self.cache.borrow_mut().insert(input, node_id);
        node_id
    }
}

impl<Input: std::hash::Hash + Eq + Clone> std::default::Default for Cache<Input> {
    fn default() -> Self {
        Self { cache: Default::default() }
    }
}
