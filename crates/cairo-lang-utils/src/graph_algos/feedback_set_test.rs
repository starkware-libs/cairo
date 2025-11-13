use cairo_lang_test_utils::test;
use itertools::chain;
use test_case::test_case;

use crate::graph_algos::feedback_set::calc_feedback_set;
use crate::graph_algos::graph_node::GraphNode;
use crate::graph_algos::strongly_connected_components::{ComputeScc, compute_scc};

// A node in the graph
#[derive(PartialEq, Eq, Hash, Clone)]
struct IntegerNode {
    id: usize,
    /// The neighbors of each node.
    graph: Vec<Vec<usize>>,
}
impl GraphNode for IntegerNode {
    type NodeId = usize;

    fn get_neighbors(&self) -> Vec<Self> {
        self.graph[self.id]
            .iter()
            .map(|neighbor_id| IntegerNode { id: *neighbor_id, graph: self.graph.clone() })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.id
    }
}
impl ComputeScc for IntegerNode {
    fn compute_scc(&self) -> Vec<Self::NodeId> {
        compute_scc(self)
    }
}

#[test]
fn test_list() {
    // Nodes 0 to 9 have only one neighbor (i -> i + 1), and 10 is a leaf.
    let mut graph: Vec<Vec<usize>> = (0..10).map(|id| vec![id + 1]).collect();
    graph.push(vec![]);

    assert!(feedback_set(0, graph).is_empty());
}

#[test]
fn test_cycle() {
    // Each node has only one neighbor. i -> i + 1 for i = 0...8, and 9 -> 0.
    let graph: Vec<Vec<usize>> = (0..10).map(|id| vec![(id + 1) % 10]).collect();
    assert_eq!(feedback_set(0, graph), [0]);
}

#[test]
fn test_root_points_to_cycle() {
    // 0 to 9 form a cycle.
    let mut graph: Vec<Vec<usize>> = (0..10).map(|id| vec![(id + 1) % 10]).collect();
    // And 10 (the root) has and edge to 0.
    graph.push(/* 10: */ vec![0]);

    // Make sure the cycle that's not in the SCC of the root is not covered.
    assert!(feedback_set(10, graph.clone()).is_empty());
    // We do find in in the other case.
    assert_eq!(feedback_set(1, graph), [1]);
}

#[test]
fn test_connected_cycles() {
    // 0 to 4 form one cycle and 5 to 9 form another cycle.
    let mut graph: Vec<Vec<usize>> =
        chain!((0..5).map(|id| vec![(id + 1) % 5]), (0..5).map(|id| vec![5 + (id + 1) % 5]))
            .collect();

    // 4 is connected to 5.
    graph[4].push(5);

    // Make sure the cycle that's not in the SCC of the root is not covered.
    assert_eq!(feedback_set(0, graph.clone()), [0]);
    // We do find in in the other case.
    assert_eq!(feedback_set(5, graph), [5]);
}

#[test]
fn test_mesh() {
    // Each node has edges to all other nodes.
    let graph = (0..5).map(|i| (0..5).filter(|j| *j != i).collect::<Vec<usize>>()).collect();
    assert_eq!(feedback_set(0, graph), [0, 1, 2, 3]);
}

// The feedback set depends on the root node we start from (but it's stable given the same root).
#[test_case(0, vec![0, 3]; "root_0")]
#[test_case(1, vec![1, 3]; "root_1")]
#[test_case(2, vec![2, 3]; "root_2")]
#[test_case(3, vec![3]; "root_3")]
#[test_case(4, vec![3]; "root_4")]
#[test_case(5, vec![3]; "root_5")]
#[test_case(6, vec![3]; "root_6")]
fn test_tangent_cycles(root: usize, expected_fset: Vec<usize>) {
    // 0 to 3 form one cycle and 3 to 6 form another cycle. Note 3 is in both.
    // 0 -> 1 -> 2 ->  3  -> 4 -> 6
    // ^______________| ^_________|
    let graph: Vec<Vec<usize>> = chain!(
        (0..3).map(|id| vec![id + 1]),
        // 3:
        [vec![0, 4]],
        (0..3).map(|id| vec![3 + (id + 2) % 4])
    )
    .collect();
    assert_eq!(feedback_set(root, graph), expected_fset);
}

// Test a graph with multiple cycles.
#[test_case(0, vec![0]; "root_0")]
#[test_case(1, vec![1, 2]; "root_1")]
#[test_case(2, vec![2, 3]; "root_2")]
#[test_case(3, vec![3]; "root_3")]
fn test_multiple_cycles(root: usize, expected_fset: Vec<usize>) {
    let graph: Vec<Vec<usize>> = vec![
        // 0:
        vec![1, 2],
        // 1:
        vec![2, 3],
        // 2:
        vec![3],
        // 3:
        vec![0],
    ];
    assert_eq!(feedback_set(root, graph), expected_fset);
}

// Test a graph and continue from self loops.
#[test_case(0, vec![1, 2]; "root_0")]
#[test_case(1, vec![1, 2]; "root_1")]
#[test_case(2, vec![2, 1]; "root_2")]
fn test_with_self_loops(root: usize, expected_fset: Vec<usize>) {
    let graph: Vec<Vec<usize>> = vec![
        // 0:
        vec![1, 2],
        // 1:
        vec![0, 1],
        // 2:
        vec![2, 0],
    ];
    assert_eq!(feedback_set(root, graph), expected_fset);
}

// Test a graph and continue from size-2 loops.
#[test_case(0, vec![0, 1, 3]; "root_0")]
#[test_case(1, vec![1, 3]; "root_1")]
#[test_case(2, vec![1, 3]; "root_2")]
#[test_case(3, vec![3, 0, 1]; "root_3")]
#[test_case(4, vec![4, 0, 1]; "root_4")]
fn test_with_size2_loops(root: usize, expected_fset: Vec<usize>) {
    let graph: Vec<Vec<usize>> = vec![
        // 0:
        vec![1, 3],
        // 1:
        vec![0, 2],
        // 2:
        vec![1],
        // 3:
        vec![4, 0],
        // 4:
        vec![3],
    ];
    assert_eq!(feedback_set(root, graph), expected_fset);
}

/// Helper for testing returning the ordered elements of the feedback set.
fn feedback_set(id: usize, graph: Vec<Vec<usize>>) -> Vec<usize> {
    calc_feedback_set(IntegerNode { id, graph }.into()).into_iter().collect()
}
