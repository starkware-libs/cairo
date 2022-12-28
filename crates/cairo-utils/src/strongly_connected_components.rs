//! Logic for computing the strongly connected component of a node in a graph.

use core::hash::Hash;
use std::collections::HashMap;

#[cfg(test)]
#[path = "strongly_connected_components_test.rs"]
mod strongly_connected_components_test;

/// A trait for a node in the graph. Note a GraphNode has to be able to provide its neighbors
/// by itself, without additional information.
pub trait GraphNode: Sized + Clone {
    /// The type used to identify the nodes in the graph.
    type NodeId: PartialEq + Eq + Hash + Clone;

    /// Returns a list of the node's neighbors
    fn get_neighbors(&self) -> Vec<Self>;

    /// Gets the node's ID.
    fn get_id(&self) -> Self::NodeId;
}

/// A wrapper node to a GraphNode, to be used in the SCC algorithm. Contains the GraphNode
/// additional state for the algorithm.
#[derive(Default, PartialEq, Eq, Hash, Clone)]
struct SccAlgoNode<Node: GraphNode> {
    /// The wrapped GraphNode
    node: Node,
    /// The index of the node in the algorithm. The smaller the index, the earlier the node was
    /// reached.
    index: u32,
    /// The smallest index of a node that's reachable from this node (so far).
    lowlink: u32,
    /// Whether the node is currently on the DFS stack.
    on_stack: bool,
}

/// The context of the SCC algorithm.
struct SccAlgoContext<Node: GraphNode> {
    /// The next index to allocate to a first-seen node.
    next_index: u32,
    /// The stack of the nodes in the DFS.
    stack: Vec<Node::NodeId>,
    /// All visited nodes. If a graph node is not in the map, it wasn't yet visited.
    known_nodes: HashMap<Node::NodeId, SccAlgoNode<Node>>,
    /// The ID of the node we want to find the SCC of.
    target_node_id: Node::NodeId,
    /// The SCC of the `target_node_id`. Populated only at the end of the algorithm.
    result: Vec<Node::NodeId>,
}
impl<Node: GraphNode> SccAlgoContext<Node> {
    fn new(target_node_id: Node::NodeId) -> Self {
        SccAlgoContext::<Node> {
            next_index: 0,
            stack: Vec::new(),
            known_nodes: HashMap::new(),
            target_node_id,
            result: Vec::new(),
        }
    }
}

/// Computes the SCC (Strongly Connected Component) of the given node in its graph.
pub fn compute_scc<Node: GraphNode>(root: Node) -> Vec<Node::NodeId> {
    let mut ctx = SccAlgoContext::new(root.get_id());
    compute_scc_recursive(&mut ctx, root);
    ctx.result
}

/// The recursive call to compute the SCC of a given node.
fn compute_scc_recursive<Node: GraphNode>(ctx: &mut SccAlgoContext<Node>, current_node: Node) {
    let mut current_wrapper_node = SccAlgoNode {
        node: current_node.clone(),
        index: ctx.next_index,
        lowlink: ctx.next_index,
        on_stack: true,
    };
    let current_node_id = current_node.get_id();
    ctx.known_nodes.insert(current_node_id.clone(), current_wrapper_node.clone());
    ctx.next_index += 1;
    ctx.stack.push(current_node_id.clone());

    for neighbor in current_node.get_neighbors() {
        let neighbor_id = neighbor.get_id();
        match ctx.known_nodes.get(&neighbor_id) {
            None => {
                // neighbor was not visited yet. Visit it and maybe apply its lowlink to root.
                compute_scc_recursive(ctx, neighbor.clone());
                // Now neighbor should be in known_nodes.
                current_wrapper_node.lowlink = std::cmp::min(
                    current_wrapper_node.lowlink,
                    ctx.known_nodes[&neighbor_id].lowlink,
                );
            }
            Some(neighbor_node) => {
                if ctx.known_nodes[&neighbor_id].on_stack {
                    // This is a back edge, meaning neighbor is in current_node's SCC.
                    current_wrapper_node.lowlink =
                        std::cmp::min(current_wrapper_node.lowlink, neighbor_node.index);
                } else {
                    // If neighbor is known but not on stack, it's in a concluded dropped SCC.
                    // Ignore it.
                    continue;
                }
            }
        };

        // Update current_node in ctx.known_nodes.
        ctx.known_nodes.insert(current_node_id.clone(), current_wrapper_node.clone());
    }

    if current_wrapper_node.lowlink != current_wrapper_node.index {
        // `current_node` is not a root of an SCC. We only conclude SCCs when we reach their roots.
        return;
    }

    // `current_node` is a root of an SCC. Conclude this SCC.
    // Push the nodes from the latest to earliest in the call hierarchy, so that the reverse of the
    // SCC vector would form a valid path on the graph.
    let mut scc = Vec::new();
    while let Some(other_node_id) = ctx.stack.pop() {
        let other_node = ctx.known_nodes.get_mut(&other_node_id).unwrap();
        other_node.on_stack = false;
        scc.push(other_node_id.clone());

        // Stop once the popped node is the current node which is the root on the SCC.
        if other_node_id == current_node_id {
            break;
        }
    }

    // If this SCC is the one we are looking for, update it in ctx. Otherwise, throw this
    // SCC away.
    if current_node_id == ctx.target_node_id {
        ctx.result = scc;
    }
}
