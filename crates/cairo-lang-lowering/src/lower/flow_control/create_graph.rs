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

    // Handle the conditions.
    let mut current_node = true_branch;
    for condition in expr.conditions.iter().rev() {
        current_node = match condition {
            Condition::BoolExpr(condition) => {
                graph.add_node(FlowControlNode::BooleanIf(BooleanIf {
                    condition: condition.clone(),
                    true_branch: current_node,
                    false_branch,
                }))
            }
            Condition::Let(expr, _patterns) => {
                // TODO: fix.
                graph.add_node(FlowControlNode::BooleanIf(BooleanIf {
                    condition: expr.clone(),
                    true_branch: current_node,
                    false_branch,
                }))
            }
        }
    }

    graph.finalize(current_node)
}
