//! Utility for lowering `match` and `if` expressions.
//!
//! Lowering such expressions is done in two steps:
//! 1. Construct a flow control graph (defined in this file).
//! 2. Lower the graph.
//!
//! The flow control graph is a directed acyclic graph. Each node may point to the next node
//! (or nodes) in the graph.
//!
//! During the construction of the graph, variables are assigned to values ([FlowControlVar]).
//! These variable are not the ones used in the lowering process ([super::super::VariableId]).
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

/// Represents a variable in the flow control graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FlowControlVar {
    idx: usize,
}

/// Unique identifier for nodes in the flow control graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

/// Boolean if condition node.
#[derive(Debug)]
pub struct BooleanIf {
    /// The condition variable.
    pub condition_var: FlowControlVar,
    /// The node to jump to if the condition is true.
    pub true_branch: NodeId,
    /// The node to jump to if the condition is false.
    pub false_branch: NodeId,
}

/// Terminal expression node.
#[derive(Debug)]
pub struct ArmExpr {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
}

/// Instructs to perform lower_expr & as_var_usage.
///
/// Used to lower the `if` condition or `match` expression and get a [FlowControlVar] that can be
/// used in [BooleanIf] or `EnumMatch`.
#[derive(Debug)]
pub struct ExprToVar {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
    /// The (output) variable to assign the result to.
    pub var_id: FlowControlVar,
    /// The next node.
    pub next: NodeId,
}

/// A node in the flow control graph for a match or if lowering.
pub enum FlowControlNode {
    BooleanIf(BooleanIf),
    ArmExpr(ArmExpr),
    ExprToVar(ExprToVar),
}

impl Debug for FlowControlNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowControlNode::BooleanIf(node) => node.fmt(f),
            FlowControlNode::ArmExpr(node) => node.fmt(f),
            FlowControlNode::ExprToVar(node) => node.fmt(f),
        }
    }
}

/// Graph of flow control nodes.
///
/// Invariant: The next nodes of a node are always before the node in [Self::nodes] (and therefore
/// have a smaller node id).
pub struct FlowControlGraph {
    /// All nodes in the graph.
    pub nodes: Vec<FlowControlNode>,
    /// The initial node of the graph.
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
    /// The number of [FlowControlVar]s allocated so far.
    n_vars: usize,
}

impl FlowControlGraphBuilder {
    /// Adds a new node to the graph. Returns the new node's id.
    pub fn add_node(&mut self, node: FlowControlNode) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }

    /// Finalizes the graph and returns the final [FlowControlGraph].
    pub fn finalize(self, root: NodeId) -> FlowControlGraph {
        FlowControlGraph { nodes: self.nodes, root }
    }

    /// Creates a new [FlowControlVar].
    pub fn new_var(&mut self) -> FlowControlVar {
        let var = FlowControlVar { idx: self.n_vars };
        self.n_vars += 1;
        var
    }
}
