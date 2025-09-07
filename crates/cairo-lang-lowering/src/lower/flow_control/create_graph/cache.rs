use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::lower::flow_control::graph::{FlowControlGraphBuilder, NodeId};

/// Implements a simple memoization mechanism to optimize the flow control graph that is created.
///
/// The cache is used before calling `BuildNodeCallback` to avoid creating two nodes that behave
/// identically.
pub struct Cache<Input> {
    /// A map from input to the cached result.
    cache: UnorderedHashMap<Input, NodeId>,
}
impl<Input: std::hash::Hash + Eq + Clone> Cache<Input> {
    /// Calls the callback if this is the first time the input is seen.
    /// Returns the previous result, otherwise.
    pub fn get_or_compute<'db>(
        &mut self,
        callback: &mut dyn FnMut(&mut FlowControlGraphBuilder<'db>, Input, String) -> NodeId,
        graph: &mut FlowControlGraphBuilder<'db>,
        input: Input,
        path: String,
    ) -> NodeId {
        if let Some(node_id) = self.cache.get(&input) {
            return *node_id;
        }

        let node_id = callback(graph, input.clone(), path);
        assert!(!self.cache.contains_key(&input));
        self.cache.insert(input, node_id);
        node_id
    }
}

impl<Input: std::hash::Hash + Eq + Clone> std::default::Default for Cache<Input> {
    fn default() -> Self {
        Self { cache: Default::default() }
    }
}
