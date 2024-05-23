#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use super::graph_node::GraphNode;
use super::strongly_connected_components::ComputeScc;

/// A node whose neighbors are only the subset of its neighbors in the full graph, which are also in
/// the same SCC (strongly-connected-component) with it.
#[derive(Clone)]
pub struct SccGraphNode<Node: ComputeScc>(Node);
impl<Node: ComputeScc> GraphNode for SccGraphNode<Node> {
    type NodeId = Node::NodeId;

    fn get_neighbors(&self) -> Vec<Self> {
        let scc = self.0.compute_scc();
        self.0.get_neighbors_in_given_scc(scc).into_iter().map(SccGraphNode::<Node>).collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.0.get_id()
    }
}
impl<Node: ComputeScc> From<Node> for SccGraphNode<Node> {
    fn from(value: Node) -> Self {
        SccGraphNode::<Node>(value)
    }
}
