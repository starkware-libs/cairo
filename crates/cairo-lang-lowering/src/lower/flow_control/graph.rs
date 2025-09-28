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
//! These variables are not the ones used in the lowering process ([super::super::VariableId]).
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

use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_semantic::{self as semantic, ConcreteVariant, PatternVariable};
use cairo_lang_syntax::node::ast::ExprPtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;
use num_bigint::BigInt;
use num_integer::Integer;

use crate::diagnostic::{
    LoweringDiagnosticKind, LoweringDiagnostics, LoweringDiagnosticsBuilder, MatchKind,
};
use crate::ids::LocationId;
use crate::lower::context::LoweringContext;

/// Represents a variable in the flow control graph.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FlowControlVar(usize);
impl<'db> FlowControlVar {
    /// Returns the type of the variable.
    pub fn ty(&self, graph: &FlowControlGraph<'db>) -> semantic::TypeId<'db> {
        graph.var_types[self.0]
    }

    /// Returns the location of the variable.
    pub fn location(&self, graph: &FlowControlGraph<'db>) -> LocationId<'db> {
        graph.var_locations[self.0]
    }

    /// Returns the number of times the variable is used in the graph as an input variable
    /// (see [FlowControlNode::input_var]).
    pub fn times_used(&self, graph: &FlowControlGraph<'db>) -> usize {
        graph.times_used[self]
    }
}
impl Debug for FlowControlVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

/// Identifier that represents a [PatternVariable].
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

/// Matches on a BoundedInt type of the form `[0, n)` using a jump table.
#[derive(Debug)]
pub struct ValueMatch {
    /// The input value to match.
    pub matched_var: FlowControlVar,
    /// For each literal in the range `[0, n)`, the [NodeId] to jump to.
    pub nodes: Vec<NodeId>,
}

/// Checks whether a value is equal to a literal.
pub struct EqualsLiteral<'db> {
    /// The input value to check.
    pub input: FlowControlVar,
    /// The literal to check against.
    pub literal: BigInt,
    /// A stable pointer to the first instance of the literal in the patterns.
    pub stable_ptr: ExprPtr<'db>,
    /// The node to jump to if the value is equal to the literal.
    pub true_branch: NodeId,
    /// The node to jump to if the value is not equal to the literal.
    pub false_branch: NodeId,
}

impl<'db> std::fmt::Debug for EqualsLiteral<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "EqualsLiteral {{ input: {:?}, literal: {}, true_branch: {:?}, false_branch: {:?} }}",
            self.input, self.literal, self.true_branch, self.false_branch,
        )
    }
}

/// An arm (final node) that returns an expression.
#[derive(Debug)]
pub struct ArmExpr {
    /// The expression to evaluate.
    pub expr: semantic::ExprId,
}

/// An arm (final node) for the body of a while loop.
///
/// Evaluates the given expression and calls the next iteration.
#[derive(Debug)]
pub struct WhileBody<'db> {
    /// The body of a while loop.
    pub body: semantic::ExprId,
    /// The [semantic::ExprId] of the loop.
    pub loop_expr_id: semantic::ExprId,
    /// The stable pointer to the loop.
    pub loop_stable_ptr: SyntaxStablePtrId<'db>,
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

/// Upcasts a value to a larger type.
#[derive(Debug)]
pub struct Upcast {
    /// The input variable.
    pub input: FlowControlVar,
    /// The output variable.
    pub output: FlowControlVar,
    /// The next node.
    pub next: NodeId,
}

/// Downcasts a value to a smaller type.
#[derive(Debug)]
pub struct Downcast {
    /// The input variable.
    pub input: FlowControlVar,
    /// The output variable (if the value is in range).
    pub output: FlowControlVar,
    /// The next node if the value is in range.
    pub in_range: NodeId,
    /// The next node if the value is out of range.
    pub out_of_range: NodeId,
}

/// An arm (final node) that returns a tuple of bound variables for the let-else success arm.
///
/// See [crate::lower::lower_let_else::lower_let_else] for more details.
#[derive(Debug)]
pub struct LetElseSuccess<'db> {
    /// The variables to assign the result to.
    pub var_ids_and_stable_ptrs: Vec<(semantic::VarId<'db>, SyntaxStablePtrId<'db>)>,
}

/// A node in the flow control graph for a match or if lowering.
pub enum FlowControlNode<'db> {
    /// Evaluates an expression and assigns the result to a [FlowControlVar].
    EvaluateExpr(EvaluateExpr),
    /// Boolean if condition node.
    BooleanIf(BooleanIf),
    /// Enum match node.
    EnumMatch(EnumMatch<'db>),
    /// Matches on a BoundedInt type of the form `[0, n)` using a jump table.
    ValueMatch(ValueMatch),
    /// Checks whether a value is equal to a literal.
    EqualsLiteral(EqualsLiteral<'db>),
    /// An arm (final node) that returns an expression.
    ArmExpr(ArmExpr),
    /// An arm (final node) for the body of a while loop.
    WhileBody(WhileBody<'db>),
    /// Destructure a tuple to its members.
    Deconstruct(Deconstruct),
    /// Binds a [FlowControlVar] to a pattern variable.
    BindVar(BindVar),
    /// Upcasts a value to a larger type.
    Upcast(Upcast),
    /// Downcasts a value to a smaller type.
    Downcast(Downcast),
    /// An arm (final node) that returns a tuple of bound variables for the let-else success arm.
    LetElseSuccess(LetElseSuccess<'db>),
    /// An arm (final node) that returns a unit value - `()`.
    UnitResult,
    /// Represents a node that could not be properly constructed due to an error in the code.
    ///
    /// This variant allows the functions that create the graph to remain infallible by
    /// postponing the failure to lowering the graph.
    ///
    /// It carries a [DiagnosticAdded] to ensure an error was reported.
    Missing(DiagnosticAdded),
}

impl<'db> FlowControlNode<'db> {
    /// Returns the input [FlowControlVar] of the node, if exists.
    pub fn input_var(&self) -> Option<FlowControlVar> {
        match self {
            FlowControlNode::EvaluateExpr(..) => None,
            FlowControlNode::BooleanIf(node) => Some(node.condition_var),
            FlowControlNode::EnumMatch(node) => Some(node.matched_var),
            FlowControlNode::ValueMatch(node) => Some(node.matched_var),
            FlowControlNode::EqualsLiteral(node) => Some(node.input),
            FlowControlNode::ArmExpr(..) => None,
            FlowControlNode::WhileBody(..) => None,
            FlowControlNode::Deconstruct(node) => Some(node.input),
            FlowControlNode::BindVar(node) => Some(node.input),
            FlowControlNode::Upcast(node) => Some(node.input),
            FlowControlNode::Downcast(node) => Some(node.input),
            FlowControlNode::LetElseSuccess(..) => None,
            FlowControlNode::UnitResult => None,
            FlowControlNode::Missing(_) => None,
        }
    }
}

impl<'db> Debug for FlowControlNode<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlowControlNode::EvaluateExpr(node) => node.fmt(f),
            FlowControlNode::BooleanIf(node) => node.fmt(f),
            FlowControlNode::EnumMatch(node) => node.fmt(f),
            FlowControlNode::ValueMatch(node) => node.fmt(f),
            FlowControlNode::EqualsLiteral(node) => node.fmt(f),
            FlowControlNode::ArmExpr(node) => node.fmt(f),
            FlowControlNode::WhileBody(node) => node.fmt(f),
            FlowControlNode::Deconstruct(node) => node.fmt(f),
            FlowControlNode::BindVar(node) => node.fmt(f),
            FlowControlNode::Upcast(node) => node.fmt(f),
            FlowControlNode::Downcast(node) => node.fmt(f),
            FlowControlNode::LetElseSuccess(node) => node.fmt(f),
            FlowControlNode::UnitResult => write!(f, "UnitResult"),
            FlowControlNode::Missing(_) => write!(f, "Missing"),
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
    /// The kind of the expression being lowered.
    /// This is used for diagnostic reporting.
    kind: MatchKind<'db>,
    /// A map from used [FlowControlVar] to the number of times they are used as input variables
    /// (see [FlowControlNode::input_var]).
    times_used: UnorderedHashMap<FlowControlVar, usize>,
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

    /// Returns the kind of the expression being lowered.
    pub fn kind(&self) -> MatchKind<'db> {
        self.kind
    }
}

impl<'db> Debug for FlowControlGraph<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Root: {}", self.root().0)?;
        for (i, node) in self.nodes.iter().enumerate().rev() {
            writeln!(f, "{i} {node:?}")?;
        }
        Ok(())
    }
}
/// Builder for [FlowControlGraph].
pub struct FlowControlGraphBuilder<'db> {
    graph: FlowControlGraph<'db>,
    /// Diagnostics emitted during the construction of the flow control graph.
    diagnostics: LoweringDiagnostics<'db>,
}

impl<'db> FlowControlGraphBuilder<'db> {
    /// Constructs a new [FlowControlGraphBuilder].
    pub fn new(kind: MatchKind<'db>) -> Self {
        let graph = FlowControlGraph {
            nodes: Vec::new(),
            var_types: Vec::new(),
            var_locations: Vec::new(),
            pattern_vars: Vec::new(),
            kind,
            times_used: UnorderedHashMap::default(),
        };
        Self { graph, diagnostics: LoweringDiagnostics::default() }
    }

    /// Adds a new node to the graph. Returns the new node's id.
    pub fn add_node(&mut self, node: FlowControlNode<'db>) -> NodeId {
        // Mark the input variable (if exists) as used.
        if let Some(input_var) = node.input_var() {
            self.graph.times_used.entry(input_var).or_insert(0).inc();
        }
        let id = NodeId(self.graph.size());
        self.graph.nodes.push(node);
        id
    }

    /// Returns `true` if the given [FlowControlVar] is used in the graph.
    pub fn is_var_used(&self, var: FlowControlVar) -> bool {
        self.graph.times_used.contains_key(&var)
    }

    /// Finalizes the graph and returns the final [FlowControlGraph].
    ///
    /// Adds the reported diagnostics to the context.
    pub fn finalize(
        self,
        root: NodeId,
        ctx: &mut LoweringContext<'db, '_>,
    ) -> FlowControlGraph<'db> {
        assert_eq!(root.0, self.graph.size() - 1, "The root must be the last node.");
        ctx.diagnostics.extend(self.diagnostics.build());
        self.graph
    }

    /// Creates a new [FlowControlVar].
    pub fn new_var(
        &mut self,
        ty: semantic::TypeId<'db>,
        location: LocationId<'db>,
    ) -> FlowControlVar {
        let var = FlowControlVar(self.graph.var_types.len());
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
        self.graph.var_types[input_var.0]
    }

    /// Returns the location of the given [FlowControlVar].
    pub fn var_location(&self, input_var: FlowControlVar) -> LocationId<'db> {
        self.graph.var_locations[input_var.0]
    }

    /// Reports a diagnostic.
    pub fn report(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: LoweringDiagnosticKind<'db>,
    ) -> DiagnosticAdded {
        self.diagnostics.report(stable_ptr, kind)
    }

    /// Reports a diagnostic, and returns a new [FlowControlNode::Missing] node.
    pub fn report_with_missing_node(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: LoweringDiagnosticKind<'db>,
    ) -> NodeId {
        let diag_added = self.diagnostics.report(stable_ptr, kind);
        self.add_node(FlowControlNode::Missing(diag_added))
    }

    /// Returns the kind of the expression being lowered.
    pub fn kind(&self) -> MatchKind<'db> {
        self.graph.kind
    }
}
