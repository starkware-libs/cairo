use std::collections::HashSet;

use super::graph_node::SccAwareGraphNode;

#[cfg(test)]
#[path = "feedback_set_test.rs"]
mod feedback_set_test;

/// Context for the feedback-set algorithm.
struct FeedbackSetAlgoContext<Node: SccAwareGraphNode> {
    /// The accumulated feedback set so far in the process of the algorithm. In the end of the
    /// algorithm, this is also the result.
    pub feedback_set: HashSet<Node::NodeId>,
    /// Nodes that are currently during the recursion call on them. That is - if one of these is
    /// reached, it indicates it's in some cycle that was not "resolved" yet.
    pub in_flight: HashSet<Node::NodeId>,
}
impl<Node: SccAwareGraphNode> FeedbackSetAlgoContext<Node> {
    fn new() -> Self {
        FeedbackSetAlgoContext {
            feedback_set: HashSet::<Node::NodeId>::new(),
            in_flight: HashSet::<Node::NodeId>::new(),
        }
    }
}

/// Calculates the feedback set of an SCC.
pub fn calc_feedback_set<Node: SccAwareGraphNode>(node: &Node) -> HashSet<Node::NodeId> {
    let mut ctx = FeedbackSetAlgoContext::<Node>::new();
    calc_feedback_set_recursive(node, &mut ctx);
    ctx.feedback_set
}

fn calc_feedback_set_recursive<Node: SccAwareGraphNode>(
    node: &Node,
    ctx: &mut FeedbackSetAlgoContext<Node>,
) {
    let cur_node_id = node.get_id();
    ctx.in_flight.insert(cur_node_id.clone());
    for neighbor in node.get_neighbors_in_scc() {
        let neighbor_id = neighbor.get_id();
        if ctx.feedback_set.contains(&neighbor_id) {
            continue;
        } else if ctx.in_flight.contains(&neighbor_id) {
            ctx.feedback_set.insert(neighbor_id);
        } else {
            calc_feedback_set_recursive(&neighbor, ctx);
        }

        // `node` might have been added to the fset during this iteration of the loop. If so, no
        // need to continue this loop.
        if ctx.feedback_set.contains(&cur_node_id) {
            break;
        }
    }
    ctx.in_flight.remove(&cur_node_id);
}
