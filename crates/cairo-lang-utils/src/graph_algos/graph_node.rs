#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
use core::hash::Hash;
use hashbrown::HashSet;

/// A trait for a node in a graph. Note that a GraphNode has to be able to provide its neighbors
/// by itself, without additional information.
pub trait GraphNode: Sized + Clone {
    /// The type used to identify the nodes in the graph.
    type NodeId: Eq + Hash + Clone;

    /// Returns a list of the node's neighbors.
    /// Must be stable for the SCC result to be stable. i.e. if the output for a node here doesn't
    /// change between different runs, the computed SCC of the node is guaranteed to also not
    /// change.
    fn get_neighbors(&self) -> Vec<Self>;

    /// Gets the node's ID.
    fn get_id(&self) -> Self::NodeId;

    /// Helper function to get the neighbors of the node, given its SCC. Default-implemented and
    /// thus can be used in simple implementations of get_neighbors_in_scc.
    fn get_neighbors_in_given_scc(&self, scc: Vec<Self::NodeId>) -> Vec<Self> {
        let mut scc_set = HashSet::with_capacity(scc.len());
        scc_set.extend(scc);

        self.get_neighbors()
            .into_iter()
            .filter(|neighbor| scc_set.contains(&neighbor.get_id()))
            .collect()
    }
}
