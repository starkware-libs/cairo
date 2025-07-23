//! Utility for lowering `match` and `if` expressions.
//!
//! Lowering such expressions is done in two steps:
//! 1. Construct a flow control graph (defined in this file).
//! 2. Lower the graph.
//!
//! The flow control graph is a directed acyclic graph. Each node may point to the next node
//! (or nodes) in the graph.
//!
//! For example, `if x { 1 } else { 2 }` is represented by the following graph
//! (the root is `NodeId(2)`):
//!
//! ```plain
//! 0 - ArmExpr { expr: `1` }
//! 1 - ArmExpr { expr: `2` }
//! 2 - BooleanIf { condition: `x`, true_branch: NodeId(0), false_branch: NodeId(1) }
//! ```

use std::fmt::Debug;

use cairo_lang_semantic as semantic;

/// Unique identifier for nodes in the flow control graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

/// Boolean if condition node.
#[derive(Debug)]
pub struct BooleanIf {
    /// The condition expression.
    #[allow(dead_code)]
    pub condition: semantic::ExprId,
    /// The node to jump to if the condition is true.
    #[allow(dead_code)]
    pub true_branch: NodeId,
    /// The node to jump to if the condition is false.
    #[allow(dead_code)]
    pub false_branch: NodeId,
}

/// Terminal expression node.
#[derive(Debug)]
pub struct ArmExpr {
    /// The expression to evaluate.
    #[allow(dead_code)]
    pub expr: semantic::ExprId,
}

/// A node in the flow control graph for a match or if lowering.
pub enum FlowControlNode {
    #[allow(dead_code)]
    BooleanIf(BooleanIf),
    #[allow(dead_code)]
    ArmExpr(ArmExpr),
}

impl Debug for FlowControlNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowControlNode::BooleanIf(node) => node.fmt(f),
            FlowControlNode::ArmExpr(node) => node.fmt(f),
        }
    }
}

/// Graph of flow control nodes.
pub struct FlowControlGraph {
    /// All nodes in the graph.
    #[allow(dead_code)]
    pub nodes: Vec<FlowControlNode>,
    /// The initial node of the graph.
    #[allow(dead_code)]
    pub root: NodeId,
}

impl Debug for FlowControlGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Root: {}", self.root.0)?;
        for (i, node) in self.nodes.iter().enumerate() {
            writeln!(f, "{i} {node:?}")?;
        }
        Ok(())
    }
}
/// Builder for [FlowControlGraph].
#[derive(Default)]
pub struct FlowControlGraphBuilder {
    /// All nodes in the graph.
    nodes: Vec<FlowControlNode>,
}

impl FlowControlGraphBuilder {
    pub fn add_node(&mut self, node: FlowControlNode) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }

    pub fn finalize(self, root: NodeId) -> FlowControlGraph {
        FlowControlGraph { nodes: self.nodes, root }
    }
}
