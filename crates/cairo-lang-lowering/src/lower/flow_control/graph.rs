//! Utility for lowering `match` and `if` expressions.
//!
//! Lowering such expressions is done in two steps:
//! 1. Construct a flow control graph (defined in this file).
//! 2. Lower the graph.
//!
//! The flow control graph is a directed acyclic graph. Each node may point to the next node or
//! nodes in the graph.
//!
//! During the construction of the graph, variables are assigned to values ([FlowControlVar]).
//! These variable are not the ones used in the lowering process ([super::VariableId]).

// TODO: add examples of graphs.

use std::fmt::Debug;

use cairo_lang_semantic::{self as semantic, ConcreteVariant, PatternVariable};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use itertools::Itertools;

/// Represents a variable in the flow control graph.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FlowControlVar {
    idx: usize,
    ty: semantic::TypeId,
}
impl FlowControlVar {
    pub fn ty(&self) -> semantic::TypeId {
        self.ty
    }
}

impl std::fmt::Debug for FlowControlVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FlowControlVar({})", self.idx)
    }
}

/// Unique identifier for nodes in the flow control graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

/// Boolean if condition node.
#[derive(Debug)]
pub struct BooleanIf {
    /// The condition expression.
    pub condition: semantic::ExprId,
    /// The node to jump to if the condition is true.
    pub true_branch: NodeId,
    /// The node to jump to if the condition is false.
    pub false_branch: NodeId,
}

/// Enum match node.
pub struct EnumMatch {
    /// The input value to match.
    pub matched_var: FlowControlVar,
    /// The concrete enum id.
    pub concrete_enum_id: semantic::ConcreteEnumId,
    /// For each variant, the node to jump to and an output variable for the inner value.
    pub variants: Vec<(ConcreteVariant, NodeId, FlowControlVar)>,
    /// The stable pointer of the expression initiating the match.
    pub stable_ptr: SyntaxStablePtrId,
    // TODO:
    // The (output) variable for the inner value.
    // pub inner_value: FlowControlVar,
}

impl std::fmt::Debug for EnumMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "EnumMatch {{ matched_var: {:?}, variants: {}}}",
            self.matched_var,
            self.variants.iter().map(|(_, node, var)| format!("({node:?}, {var:?})")).join(", ")
        )
    }
}

/// Terminal expression node.
#[derive(Debug)]
pub struct ArmExpr {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
}

/// Instructs to perform lower_expr & as_var_usage.
///
/// Used to lower the `match` expression and get a [FlowControlVar] that can be used in [EnumMatch].
#[derive(Debug)]
pub struct ExprToVar {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
    /// The (output) variable to assign the result to.
    pub var_id: FlowControlVar,
    /// The next node.
    pub next: NodeId,
}

/// Destructure (for structs and tuples).
#[derive(Debug)]
pub struct Deconstruct {
    /// The input value to destructure (a variable of type struct or tuple).
    pub input: FlowControlVar,
    /// The (output) variables to assign the result to. The number of variables is equal to the
    /// number of fields in the struct or tuple.
    pub outputs: Vec<FlowControlVar>,
    /// The next node.
    pub next: NodeId,
}

/// Capture - assigns a [PatternVariable] to an existing [FlowControlVar] that can be later used
/// in the expressions.
#[derive(Debug)]
pub struct Capture {
    /// The input variable to capture.
    pub input: FlowControlVar,
    /// The (output) pattern variable to assign the result to.
    pub output: PatternVariable,
    /// The next node.
    pub next: NodeId,
}

/// A node in the flow control graph for a match or if lowering.
pub enum FlowControlNode {
    BooleanIf(BooleanIf),
    EnumMatch(EnumMatch),
    ArmExpr(ArmExpr),
    ExprToVar(ExprToVar),
    Deconstruct(Deconstruct),
    Capture(Capture),
    UnitResult,
}

impl FlowControlNode {
    /// Returns the next nodes that this node can transition to.
    pub fn next_nodes(&self) -> Vec<NodeId> {
        match self {
            FlowControlNode::BooleanIf(node) => {
                vec![node.true_branch, node.false_branch]
            }
            FlowControlNode::EnumMatch(node) => {
                node.variants.iter().map(|(_, node, _)| *node).collect()
            }
            FlowControlNode::ArmExpr(_) => {
                // Terminal node - no next nodes
                vec![]
            }
            FlowControlNode::ExprToVar(node) => {
                vec![node.next]
            }
            FlowControlNode::Deconstruct(node) => {
                vec![node.next]
            }
            FlowControlNode::Capture(node) => {
                vec![node.next]
            }
            FlowControlNode::UnitResult => {
                // Terminal node - no next nodes
                vec![]
            }
        }
    }
}

impl Debug for FlowControlNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowControlNode::BooleanIf(node) => node.fmt(f),
            FlowControlNode::EnumMatch(node) => node.fmt(f),
            FlowControlNode::ArmExpr(node) => node.fmt(f),
            FlowControlNode::ExprToVar(node) => node.fmt(f),
            FlowControlNode::Deconstruct(node) => node.fmt(f),
            FlowControlNode::Capture(node) => node.fmt(f),
            FlowControlNode::UnitResult => write!(f, "UnitResult"),
        }
    }
}

/// Graph of flow control nodes.
///
/// Invariant: The next nodes (see [FlowControlNode::next_nodes]) of a node are always before the
/// node in [Self::nodes] (and therefore have a smaller node id).
#[derive(Debug)]
pub struct FlowControlGraph {
    /// All nodes in the graph.
    pub nodes: Vec<FlowControlNode>,
    /// The initial node of the graph.
    pub root: NodeId,
}

/// Graph of flow control nodes.
pub struct FlowControlGraphBuilder {
    /// All nodes in the graph.
    nodes: Vec<FlowControlNode>,
    /// The number of [FlowControlVar]s allocated so far.
    n_vars: usize,
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

    pub fn new_var(&mut self, ty: semantic::TypeId) -> FlowControlVar {
        let var = FlowControlVar { idx: self.n_vars, ty };
        self.n_vars += 1;
        var
    }
}

impl Default for FlowControlGraphBuilder {
    fn default() -> Self {
        Self { nodes: Vec::new(), n_vars: 0 }
    }
}
