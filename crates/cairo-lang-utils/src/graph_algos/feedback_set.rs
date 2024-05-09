//! A feedback-vertex-set is a set of vertices whose removal leaves a graph without cycles
//! (<https://en.wikipedia.org/wiki/Feedback_vertex_set>).
//! We use this algorithm to spot the relevant places for adding `withdraw_gas` statements in the
//! resulting Sierra code - there should be a `withdraw_gas` call in every recursive call, or in
//! other words, in any cycle in the function call graph.
//! An efficient algorithm to find the minimum feedback-vertex-set in a directed graph is not known,
//! so here we implement some straight-forward algorithm that guarantees to cover all the cycles in
//! the graph, but doesn't necessarily produce the minimum size of such a set.

use std::collections::VecDeque;

use super::graph_node::GraphNode;
use super::scc_graph_node::SccGraphNode;
use super::strongly_connected_components::ComputeScc;
use crate::ordered_hash_set::OrderedHashSet;
use crate::unordered_hash_set::UnorderedHashSet;

#[cfg(test)]
#[path = "feedback_set_test.rs"]
mod feedback_set_test;

/// Context for the feedback-set algorithm.
struct FeedbackSetAlgoContext<Node: ComputeScc> {
    /// Nodes that were discovered as reachable from the current run, but possibly were not yet
    /// visited.
    pending: VecDeque<SccGraphNode<Node>>,
    /// The accumulated feedback set so far in the process of the algorithm. In the end of the
    /// algorithm, this is also the result.
    feedback_set: OrderedHashSet<Node::NodeId>,
    /// Nodes that are currently during the recursion call on them. That is - if one of these is
    /// reached, it indicates it's in some cycle that was not "resolved" yet.
    in_flight: UnorderedHashSet<Node::NodeId>,
    /// Already visited nodes in the current run.
    visited: UnorderedHashSet<Node::NodeId>,
}

/// Calculates the feedback set of an SCC.
pub fn calc_feedback_set<Node: ComputeScc>(
    node: SccGraphNode<Node>,
) -> OrderedHashSet<Node::NodeId> {
    let mut ctx = FeedbackSetAlgoContext {
        feedback_set: OrderedHashSet::default(),
        in_flight: UnorderedHashSet::default(),
        pending: VecDeque::new(),
        visited: UnorderedHashSet::default(),
    };
    ctx.pending.push_back(node);
    while let Some(node) = ctx.pending.pop_front() {
        calc_feedback_set_recursive(node, &mut ctx);
    }
    ctx.feedback_set
}

fn calc_feedback_set_recursive<Node: ComputeScc>(
    node: SccGraphNode<Node>,
    ctx: &mut FeedbackSetAlgoContext<Node>,
) {
    let cur_node_id = node.get_id();
    if ctx.visited.contains(&cur_node_id) {
        return;
    }
    ctx.visited.insert(cur_node_id.clone());
    ctx.in_flight.insert(cur_node_id.clone());
    let mut neighbors = node.get_neighbors().into_iter();
    for neighbor in neighbors.by_ref() {
        let neighbor_id = neighbor.get_id();
        if ctx.feedback_set.contains(&neighbor_id) {
            continue;
        } else if ctx.in_flight.contains(&neighbor_id) {
            ctx.feedback_set.insert(neighbor_id);
        } else {
            calc_feedback_set_recursive(neighbor, ctx);
        }

        // `node` might have been added to the fset during this iteration of the loop. If so, no
        // need to continue this loop.
        if ctx.feedback_set.contains(&cur_node_id) {
            break;
        }
    }
    ctx.pending.extend(neighbors.filter(|n| !ctx.visited.contains(&n.get_id())));
    ctx.in_flight.remove(&cur_node_id);
}
