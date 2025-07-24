use cairo_lang_semantic::{self as semantic, Condition};

use super::graph::{
    ArmExpr, BooleanIf, FlowControlGraph, FlowControlGraphBuilder, FlowControlNode,
};

/// Creates a graph node for [semantic::ExprIf].
#[allow(dead_code)]
pub fn create_graph_expr_if(expr: &semantic::ExprIf) -> FlowControlGraph {
    let mut graph = FlowControlGraphBuilder::default();

    // Add the `true` branch (the `if` block).
    let true_branch = graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: expr.if_block }));

    // Add the `false` branch (the `else` block), if exists.
    let false_branch = if let Some(else_block) = expr.else_block {
        graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: else_block }))
    } else {
        todo!("Else block is not supported yet.")
    };

    // Start with the `true` branch.
    // Iterate over the conditions in reverse order.
    // Each condition adds a node leading to the current node or the `false` branch.
    let mut current_node = true_branch;
    for condition in expr.conditions.iter().rev() {
        current_node = match condition {
            Condition::BoolExpr(condition) => {
                graph.add_node(FlowControlNode::BooleanIf(BooleanIf {
                    condition: *condition,
                    true_branch: current_node,
                    false_branch,
                }))
            }
            Condition::Let(..) => {
                todo!("'if let' is not supported yet.")
            }
        }
    }

    graph.finalize(current_node)
}
