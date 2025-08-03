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

use cairo_lang_semantic::{self as semantic, ConcreteVariant, PatternVariable};
use itertools::Itertools;

use crate::ids::LocationId;

/// Represents a variable in the flow control graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FlowControlVar {
    idx: usize,
}
impl FlowControlVar {
    /// Returns the type of the variable.
    pub fn ty<'db>(&self, graph: &FlowControlGraph<'db>) -> semantic::TypeId<'db> {
        graph.var_types[self.idx]
    }

    /// Returns the location of the variable.
    pub fn location<'db>(&self, graph: &FlowControlGraph<'db>) -> LocationId<'db> {
        graph.var_locations[self.idx]
    }
}

/// A thin wrapper around [PatternVariable] that is used for fast comparison.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PatternVarId(usize);

impl PatternVarId {
    /// Returns the pattern variable.
    pub fn get<'db, 'a>(&self, graph: &'a FlowControlGraph<'db>) -> &'a PatternVariable<'db> {
        &graph.pattern_vars[self.0]
    }
}

/// Unique identifier for nodes in the flow control graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

/// Instructs to perform `lower_expr` & `as_var_usage`.
///
/// Used to lower the `if` condition or `match` expression and get a [FlowControlVar] that can be
/// used in [BooleanIf] or [EnumMatch].
#[derive(Debug)]
pub struct EvaluateExpr {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
    /// The (output) variable to assign the result to.
    pub var_id: FlowControlVar,
    /// The next node.
    pub next: NodeId,
}

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

/// Enum match node.
pub struct EnumMatch<'db> {
    /// The input value to match.
    pub matched_var: FlowControlVar,
    /// The concrete enum id.
    pub concrete_enum_id: semantic::ConcreteEnumId<'db>,
    /// For each variant, the node to jump to and an output variable for the inner value.
    pub variants: Vec<(ConcreteVariant<'db>, NodeId, FlowControlVar)>,
}

impl<'db> std::fmt::Debug for EnumMatch<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "EnumMatch {{ matched_var: {:?}, variants: {}}}",
            self.matched_var,
            self.variants.iter().map(|(_, node, var)| format!("({node:?}, {var:?})")).join(", ")
        )
    }
}

/// An arm (final node) that returns an expression.
#[derive(Debug)]
pub struct ArmExpr {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
}

/// Assigns a [PatternVariable] to an existing [FlowControlVar] that can be later used
/// in the expressions.
#[derive(Debug)]
pub struct BindVar {
    /// The input variable to bind.
    pub input: FlowControlVar,
    /// The (output) pattern variable to assign the result to.
    pub output: PatternVarId,
    /// The next node.
    pub next: NodeId,
}

/// A node in the flow control graph for a match or if lowering.
pub enum FlowControlNode<'db> {
    /// Evaluates an expression and assigns the result to a [FlowControlVar].
    EvaluateExpr(EvaluateExpr),
    /// Boolean if condition node.
    BooleanIf(BooleanIf),
    /// Enum match node.
    EnumMatch(EnumMatch<'db>),
    /// An arm (final node) that returns an expression.
    ArmExpr(ArmExpr),
    /// Binds a [FlowControlVar] to a pattern variable.
    BindVar(BindVar),
    /// An arm (final node) that returns a unit value - `()`.
    UnitResult,
}

impl<'db> Debug for FlowControlNode<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowControlNode::EvaluateExpr(node) => node.fmt(f),
            FlowControlNode::BooleanIf(node) => node.fmt(f),
            FlowControlNode::EnumMatch(node) => node.fmt(f),
            FlowControlNode::ArmExpr(node) => node.fmt(f),
            FlowControlNode::BindVar(node) => node.fmt(f),
            FlowControlNode::UnitResult => write!(f, "UnitResult"),
        }
    }
}

/// Graph of flow control nodes.
///
/// Invariant: The next nodes of a node are always before the node in [Self::nodes] (and therefore
/// have a smaller node id).
pub struct FlowControlGraph<'db> {
    /// All nodes in the graph.
    nodes: Vec<FlowControlNode<'db>>,
    /// The type of each [FlowControlVar].
    var_types: Vec<semantic::TypeId<'db>>,
    /// The location of each [FlowControlVar].
    var_locations: Vec<LocationId<'db>>,
    /// The pattern variables used by the [BindVar] nodes in the graph.
    pattern_vars: Vec<PatternVariable<'db>>,
}
impl<'db> FlowControlGraph<'db> {
    /// Returns the root node of the graph.
    pub fn root(&self) -> NodeId {
        // The root is always the last node.
        NodeId(self.nodes.len() - 1)
    }

    /// Returns the number of nodes in the graph.
    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    /// Returns the node with the given [NodeId].
    pub fn node(&self, id: NodeId) -> &FlowControlNode<'db> {
        &self.nodes[id.0]
    }
}

impl<'db> Debug for FlowControlGraph<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Root: {}", self.root().0)?;
        for (i, node) in self.nodes.iter().enumerate() {
            writeln!(f, "{i} {node:?}")?;
        }
        Ok(())
    }
}
/// Builder for [FlowControlGraph].
pub struct FlowControlGraphBuilder<'db> {
    graph: FlowControlGraph<'db>,
}

impl<'db> FlowControlGraphBuilder<'db> {
    /// Adds a new node to the graph. Returns the new node's id.
    pub fn add_node(&mut self, node: FlowControlNode<'db>) -> NodeId {
        let id = NodeId(self.graph.size());
        self.graph.nodes.push(node);
        id
    }

    /// Finalizes the graph and returns the final [FlowControlGraph].
    pub fn finalize(self, root: NodeId) -> FlowControlGraph<'db> {
        assert_eq!(root.0, self.graph.size() - 1, "The root must be the last node.");
        self.graph
    }

    /// Creates a new [FlowControlVar].
    pub fn new_var(
        &mut self,
        ty: semantic::TypeId<'db>,
        location: LocationId<'db>,
    ) -> FlowControlVar {
        let var = FlowControlVar { idx: self.graph.var_types.len() };
        self.graph.var_types.push(ty);
        self.graph.var_locations.push(location);
        var
    }

    /// Registers a new [PatternVariable] and returns a corresponding [PatternVarId].
    pub fn register_pattern_var(&mut self, var: PatternVariable<'db>) -> PatternVarId {
        let idx = self.graph.pattern_vars.len();
        self.graph.pattern_vars.push(var);
        PatternVarId(idx)
    }

    /// Returns the type of the given [FlowControlVar].
    pub fn var_ty(&self, input_var: FlowControlVar) -> semantic::TypeId<'db> {
        self.graph.var_types[input_var.idx]
    }
}

impl<'db> Default for FlowControlGraphBuilder<'db> {
    fn default() -> Self {
        let graph = FlowControlGraph {
            nodes: Vec::new(),
            var_types: Vec::new(),
            var_locations: Vec::new(),
            pattern_vars: Vec::new(),
        };
        Self { graph }
    }
}
