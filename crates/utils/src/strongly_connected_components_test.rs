use std::collections::HashSet;

use itertools::chain;
use test_case::test_case;
use test_log::test;

use super::GraphNode;
use crate::strongly_connected_components::compute_scc;

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

#[test]
fn test_short_list() {
    let graph = vec![/* 0: */ vec![1], /* 1: */ vec![]];

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from([0]));
}

#[test]
fn test_list() {
    // Nodes 0 to 9 have only one neighbor (i -> i + 1), and 10 is a leaf.
    let mut graph: Vec<Vec<usize>> = (0..10).into_iter().map(|id| vec![id + 1]).collect();
    graph.push(vec![]);

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from([0]));
}

#[test]
fn test_cycle() {
    // Each node has only one neighbor. i -> i + 1 for i = 0...8, and 9 -> 0.
    let graph: Vec<Vec<usize>> = (0..10).into_iter().map(|id| vec![(id + 1) % 10]).collect();

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from_iter(0..10));
}

#[test]
fn test_mesh() {
    // Each node has edges to all other nodes.
    let graph = (0..5).map(|i| (0..5).filter(|j| *j != i).collect::<Vec<usize>>()).collect();

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from_iter(0..5));
}

#[test]
fn test_list_with_back_edges() {
    let graph = vec![
        // 0:
        vec![1],
        // 1:
        vec![2],
        // 2:
        vec![0, 3],
        // 3:
        vec![1],
    ];

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from_iter(0..4));
}

#[test]
fn test_root_points_to_cycle() {
    // 0 to 9 form a cycle.
    let mut graph: Vec<Vec<usize>> = (0..10).into_iter().map(|id| vec![(id + 1) % 10]).collect();
    // And 10 (the root) has and edge to 0.
    graph.push(/* 10: */ vec![0]);

    // Note 10 is used as a root.
    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 10, graph }));
    assert_eq!(scc, HashSet::from([10]));
}

#[test_case(true; "root_in_first_cycle")]
#[test_case(false; "root_in_second_cycle")]
fn test_connected_cycles(root_in_first_cycle: bool) {
    // 0 to 4 form one cycle and 5 to 9 form another cycle.
    let mut graph: Vec<Vec<usize>> = chain!(
        (0..5).into_iter().map(|id| vec![(id + 1) % 5]),
        (0..5).into_iter().map(|id| vec![5 + (id + 1) % 5])
    )
    .collect();

    // Add an edge between 4 and 5 to connect the cycles. Determine the direction according to
    // root_in_first_cycle.
    if root_in_first_cycle {
        graph[4].push(5);
    } else {
        graph[5].push(4);
    }

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from_iter(0..5));
}

#[test]
fn test_tangent_cycles() {
    // 0 to 3 form one cycle and 3 to 6 form another cycle. Note 3 is in both.
    // 0 -> 1 -> 2 ->  3  -> 4 -> 6
    // ^______________| ^_________|
    let graph: Vec<Vec<usize>> = chain!(
        (0..3).into_iter().map(|id| vec![id + 1]),
        // 3:
        vec![vec![0, 4]].into_iter(),
        (0..3).into_iter().map(|id| vec![3 + (id + 2) % 4])
    )
    .collect();

    let scc = HashSet::<usize>::from_iter(compute_scc(IntegerNode { id: 0, graph }));
    assert_eq!(scc, HashSet::from_iter(0..7));
}
