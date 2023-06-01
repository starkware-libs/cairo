#[cfg(feature = "std")]
use std::collections::HashSet;

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
#[cfg(not(feature = "std"))]
use hashbrown::HashSet;

use itertools::chain;
use test_case::test_case;
use test_log::test;

use crate::graph_algos::feedback_set::calc_feedback_set;
use crate::graph_algos::graph_node::GraphNode;
use crate::graph_algos::strongly_connected_components::{compute_scc, ComputeScc};

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

    let fset = HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: 0, graph }.into()));
    assert!(fset.is_empty());
}

#[test]
fn test_cycle() {
    // Each node has only one neighbor. i -> i + 1 for i = 0...8, and 9 -> 0.
    let graph: Vec<Vec<usize>> = (0..10).map(|id| vec![(id + 1) % 10]).collect();

    let fset = HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: 0, graph }.into()));
    assert_eq!(fset, HashSet::from([0]));
}

#[test]
fn test_root_points_to_cycle() {
    // 0 to 9 form a cycle.
    let mut graph: Vec<Vec<usize>> = (0..10).map(|id| vec![(id + 1) % 10]).collect();
    // And 10 (the root) has and edge to 0.
    graph.push(/* 10: */ vec![0]);

    // Note 10 is used as a root.
    let fset = HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: 0, graph }.into()));
    assert_eq!(fset, HashSet::from([0]));
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
    let fset = HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: 0, graph }.into()));
    assert_eq!(fset, HashSet::from([0]));
}

#[test]
fn test_mesh() {
    // Each node has edges to all other nodes.
    let graph = (0..5).map(|i| (0..5).filter(|j| *j != i).collect::<Vec<usize>>()).collect();

    let fset = HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: 0, graph }.into()));
    assert_eq!(fset, HashSet::from_iter(0..4));
}

// The feedback set depends on the root node we start from (but it's stable given the same root).
#[test_case(0, HashSet::from([0, 3]); "root_0")]
#[test_case(3, HashSet::from([3]); "root_3")]
fn test_tangent_cycles(root: usize, expected_fset: HashSet<usize>) {
    // 0 to 3 form one cycle and 3 to 6 form another cycle. Note 3 is in both.
    // 0 -> 1 -> 2 ->  3  -> 4 -> 6
    // ^______________| ^_________|
    let graph: Vec<Vec<usize>> = chain!(
        (0..3).map(|id| vec![id + 1]),
        // 3:
        vec![vec![0, 4]].into_iter(),
        (0..3).map(|id| vec![3 + (id + 2) % 4])
    )
    .collect();

    let fset =
        HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: root, graph }.into()));
    assert_eq!(fset, expected_fset);
}

// Test a graph with multiple cycles.
#[test_case(0, HashSet::from([0]); "root_0")]
#[test_case(1, HashSet::from([1, 2]); "root_1")]
#[test_case(2, HashSet::from([2, 3]); "root_2")]
#[test_case(3, HashSet::from([3]); "root_3")]
fn test_multiple_cycles(root: usize, expected_fset: HashSet<usize>) {
    let graph: Vec<Vec<usize>> = chain!(
        // 0:
        vec![vec![1, 2]].into_iter(),
        // 1:
        vec![vec![2, 3]].into_iter(),
        // 2:
        vec![vec![3]].into_iter(),
        // 3:
        vec![vec![0]].into_iter(),
    )
    .collect();

    let fset =
        HashSet::<usize>::from_iter(calc_feedback_set(&IntegerNode { id: root, graph }.into()));
    assert_eq!(fset, expected_fset);
}
