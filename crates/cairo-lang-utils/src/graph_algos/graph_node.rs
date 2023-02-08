use core::hash::Hash;

/// A trait for a node in the graph. Note a GraphNode has to be able to provide its neighbors
/// by itself, without additional information.
pub trait GraphNode: Sized + Clone {
    /// The type used to identify the nodes in the graph.
    type NodeId: PartialEq + Eq + Hash + Clone;

    /// Returns a list of the node's neighbors.
    /// Must be stable for the SCC result to be stable. i.e. if the output for a node here doesn't
    /// change between different runs, the computed SCC of the node is guaranteed to also not
    /// change.
    fn get_neighbors(&self) -> Vec<Self>;

    /// Gets the node's ID.
    fn get_id(&self) -> Self::NodeId;
}

/// A trait for a node in the graph that is aware of the SCCs.
pub trait SccAwareGraphNode: GraphNode + Sized + Clone {
    /// Returns a list of the node's neighbors that are in the same strongly-connected-component as
    /// this node.
    ///
    /// Must be stable for the scc result to be stable. i.e. if the output for a node
    /// here doesn't change between different runs, the computed SCC of the node is guaranteed
    /// to also not change.
    fn get_neighbors_in_scc(&self) -> Vec<Self>;

    /// Helper function to get the neighbors of the node, given its SCC. Default-implemented and
    /// thus can be used in simple implementations of get_neighbors_in_scc.
    fn get_neighbors_in_given_scc(&self, scc: Vec<Self::NodeId>) -> Vec<Self> {
        let mut neighbors_in_scc = Vec::new();
        for neighbor in self.get_neighbors() {
            if scc.contains(&neighbor.get_id()) {
                neighbors_in_scc.push(neighbor);
            }
        }
        neighbors_in_scc
    }
}
