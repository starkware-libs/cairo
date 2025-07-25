use cairo_lang_semantic::{self as semantic, Condition, PatternId};
use cairo_lang_syntax::node::TypedStablePtr;
use itertools::Itertools;
use patterns::{Pattern, create_node_for_patterns};

use super::graph::{
    ArmExpr, BooleanIf, ExprToVar, FlowControlGraph, FlowControlGraphBuilder, FlowControlNode,
    NodeId,
};
use crate::lower::context::LoweringContext;

mod patterns;

/// Creates a graph node for [semantic::ExprIf].
#[allow(dead_code)]
pub fn create_graph_expr_if(
    ctx: &LoweringContext<'_, '_>,
    expr: &semantic::ExprIf,
) -> FlowControlGraph {
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
        current_node = match condition {
            Condition::BoolExpr(condition) => {
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

                graph.add_node(FlowControlNode::ExprToVar(ExprToVar {
                    expr: *condition,
                    var_id: condition_var,
                    next: current_node,
                }))
            }
            Condition::Let(expr_id, patterns) => {
                // Get the type of the expression.
                // TODO: peel_snapshots?

                let expr = &ctx.function_body.arenas.exprs[*expr_id];

                // Create a variable for the expression.
                let expr_location = ctx.get_location(expr.stable_ptr().untyped());
                let expr_var = graph.new_var(expr.ty(), expr_location);

                let match_node_id = create_node_for_patterns(
                    ctx,
                    &mut graph,
                    expr_var,
                    expr_location,
                    &patterns
                        .iter()
                        .map(|pattern| Pattern::from_semantic(ctx, *pattern))
                        .collect_vec(),
                    &|graph, pattern_indices| {
                        pattern_indices
                            .first()
                            .map(|index_and_captures| {
                                index_and_captures.wrap_node(graph, current_node)
                            })
                            .unwrap_or(false_branch)
                    },
                );

                // Create a node for lowering `expr` into `expr_var` and continue to the match.
                graph.add_node(FlowControlNode::ExprToVar(ExprToVar {
                    expr: *expr_id,
                    var_id: expr_var,
                    next: match_node_id,
                }))
            }
        }
    }

    graph.finalize(current_node)
}

/// Creates a graph node for [semantic::ExprMatch].
#[allow(dead_code)]
pub fn create_graph_expr_match(
    ctx: &LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
) -> FlowControlGraph {
    let mut graph = FlowControlGraphBuilder::default();

    let matched_expr = &ctx.function_body.arenas.exprs[expr.matched_expr];
    let matched_expr_location = ctx.get_location(matched_expr.stable_ptr().untyped());
    let matched_var = graph.new_var(matched_expr.ty(), matched_expr_location);

    // Create a list of patterns and nodes.
    let pattern_and_nodes: Vec<(PatternId, NodeId)> = expr
        .arms
        .iter()
        .flat_map(|match_arm| {
            // For each arm, create a node for the arm expression.
            let arm_node =
                graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: match_arm.expression }));
            // Then map the patterns to that node.
            match_arm.patterns.iter().map(move |pattern| (*pattern, arm_node))
        })
        .collect();

    let match_node_id = create_node_for_patterns(
        ctx,
        &mut graph,
        matched_var,
        matched_expr_location,
        &pattern_and_nodes
            .iter()
            .map(|(pattern, _)| Pattern::from_semantic(ctx, *pattern))
            .collect_vec(),
        &|graph, pattern_indices| {
            // TODO: produce diagnostics if pattern_indices is empty.
            let index_and_captures = pattern_indices.first().unwrap();
            let index = index_and_captures.index;
            index_and_captures.wrap_node(graph, pattern_and_nodes[index].1)
        },
    );

    let root = graph.add_node(FlowControlNode::ExprToVar(ExprToVar {
        expr: expr.matched_expr,
        var_id: matched_var,
        next: match_node_id,
    }));

    graph.finalize(root)
}
