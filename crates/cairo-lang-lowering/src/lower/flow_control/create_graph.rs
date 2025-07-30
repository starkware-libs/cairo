use cairo_lang_semantic::{self as semantic, Condition};
use cairo_lang_syntax::node::TypedStablePtr;
use itertools::Itertools;
use patterns::create_node_for_patterns;

use super::graph::{
    ArmExpr, BooleanIf, EvaluateExpr, FlowControlGraph, FlowControlGraphBuilder, FlowControlNode,
};
use crate::lower::context::LoweringContext;

mod filtered_patterns;
mod patterns;

/// Creates a graph node for [semantic::ExprIf].
pub fn create_graph_expr_if<'db>(
    ctx: &LoweringContext<'db, '_>,
    expr: &semantic::ExprIf<'db>,
) -> FlowControlGraph<'db> {
    let mut graph = FlowControlGraphBuilder::default();

    // Add the `true` branch (the `if` block).
    let true_branch = graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: expr.if_block }));

    // Add the `false` branch (the `else` block), if exists.
    let false_branch = if let Some(else_block) = expr.else_block {
        graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: else_block }))
    } else {
        graph.add_node(FlowControlNode::UnitResult)
    };

    // Start with the `true` branch.
    // Iterate over the conditions in reverse order.
    // Each condition adds a node leading to the current node or the `false` branch.
    let mut current_node = true_branch;
    for condition in expr.conditions.iter().rev() {
        match condition {
            Condition::BoolExpr(condition) => {
                // Create a variable for the condition.
                let condition_expr = &ctx.function_body.arenas.exprs[*condition];
                let condition_var = graph.new_var(
                    condition_expr.ty(),
                    ctx.get_location(condition_expr.stable_ptr().untyped()),
                );
                current_node = graph.add_node(FlowControlNode::BooleanIf(BooleanIf {
                    condition_var,
                    true_branch: current_node,
                    false_branch,
                }));

                current_node = graph.add_node(FlowControlNode::EvaluateExpr(EvaluateExpr {
                    expr: *condition,
                    var_id: condition_var,
                    next: current_node,
                }));
            }
            Condition::Let(expr_id, patterns) => {
                let expr = &ctx.function_body.arenas.exprs[*expr_id];

                // Create a variable for the expression.
                let expr_location = ctx.get_location(expr.stable_ptr().untyped());
                let expr_var = graph.new_var(expr.ty(), expr_location);

                let match_node_id = create_node_for_patterns(
                    ctx,
                    &mut graph,
                    expr_var,
                    &patterns
                        .iter()
                        .map(|pattern| &ctx.function_body.arenas.patterns[*pattern])
                        .collect_vec(),
                    &|_graph, pattern_indices| {
                        if pattern_indices.first().is_some() { current_node } else { false_branch }
                    },
                    expr_location,
                );

                // Create a node for lowering `expr` into `expr_var` and continue to the match.
                current_node = graph.add_node(FlowControlNode::EvaluateExpr(EvaluateExpr {
                    expr: *expr_id,
                    var_id: expr_var,
                    next: match_node_id,
                }));
            }
        }
    }

    graph.finalize(current_node)
}
