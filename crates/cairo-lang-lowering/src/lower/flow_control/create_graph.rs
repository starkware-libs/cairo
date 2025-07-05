use cairo_lang_semantic::{self as semantic, Condition};

use super::graph::{
    ArmExpr, BooleanIf, FlowControlGraph, FlowControlGraphBuilder, FlowControlNode,
};

/// Creates a graph node for [semantic::ExprIf].
pub fn create_graph_expr_if(expr: &semantic::ExprIf) -> FlowControlGraph {
    let mut graph = FlowControlGraphBuilder::default();

    // Add the true branch.
    let true_branch = graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: expr.if_block }));

    // Add the false branch, if exists.
    let false_branch = if let Some(else_block) = expr.else_block {
        graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: else_block }))
    } else {
        graph.add_node(FlowControlNode::UnitResult)
    };

    // Handle the condition.
    let Condition::BoolExpr(condition) = expr.conditions[0] else {
        panic!("Unsupported condition"); // TODO: fix.
    };

    let root = graph.add_node(FlowControlNode::BooleanIf(BooleanIf {
        condition,
        true_branch,
        false_branch,
    }));
    graph.finalize(root)
}
