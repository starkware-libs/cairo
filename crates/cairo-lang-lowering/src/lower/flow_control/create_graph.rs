use cache::Cache;
use cairo_lang_semantic::{self as semantic, Condition, PatternId};
use cairo_lang_syntax::node::TypedStablePtr;
use filtered_patterns::IndexAndBindings;
use itertools::Itertools;
use patterns::{CreateNodeParams, create_node_for_patterns, get_pattern};

use super::graph::{
    ArmExpr, BooleanIf, EvaluateExpr, FlowControlGraph, FlowControlGraphBuilder, FlowControlNode,
    NodeId,
};
use crate::diagnostic::{LoweringDiagnosticKind, MatchDiagnostic, MatchError, MatchKind};
use crate::lower::context::LoweringContext;

mod cache;
mod filtered_patterns;
mod patterns;

/// Creates a graph node for [semantic::ExprIf].
pub fn create_graph_expr_if<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprIf<'db>,
) -> FlowControlGraph<'db> {
    let mut graph = FlowControlGraphBuilder::new(MatchKind::IfLet);

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

                let mut cache = Cache::default();

                let match_node_id = create_node_for_patterns(
                    CreateNodeParams {
                        ctx,
                        graph: &mut graph,
                        patterns: &patterns
                            .iter()
                            .map(|pattern| Some(get_pattern(ctx, *pattern)))
                            .collect_vec(),
                        build_node_callback: &mut |graph, pattern_indices, path| {
                            if let Some(index_and_bindings) = pattern_indices.first() {
                                cache.get_or_compute(
                                    &mut |graph, index_and_bindings: IndexAndBindings, _path| {
                                        index_and_bindings.wrap_node(graph, current_node)
                                    },
                                    graph,
                                    index_and_bindings,
                                    path,
                                )
                            } else {
                                false_branch
                            }
                        },
                        location: expr_location,
                    },
                    expr_var,
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

    graph.finalize(current_node, ctx)
}

/// Creates a graph node for [semantic::ExprMatch].
#[allow(dead_code)]
pub fn create_graph_expr_match<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprMatch<'db>,
) -> FlowControlGraph<'db> {
    let mut graph = FlowControlGraphBuilder::new(MatchKind::Match);

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

    let mut cache = Cache::default();

    let match_node_id = create_node_for_patterns(
        CreateNodeParams {
            ctx,
            graph: &mut graph,
            patterns: &pattern_and_nodes
                .iter()
                .map(|(pattern, _)| Some(get_pattern(ctx, *pattern)))
                .collect_vec(),
            build_node_callback: &mut |graph, pattern_indices, path| {
                // Get the first arm that matches.
                let Some(index_and_bindings) = pattern_indices.first() else {
                    // If no arm is available, report a non-exhaustive match error.
                    let kind = LoweringDiagnosticKind::MatchError(MatchError {
                        kind: MatchKind::Match,
                        error: MatchDiagnostic::MissingMatchArm(path),
                    });
                    return graph.report_with_missing_node(expr.stable_ptr.untyped(), kind);
                };

                cache.get_or_compute(
                    &mut |graph, index_and_bindings: IndexAndBindings, _path| {
                        let index = index_and_bindings.index();
                        index_and_bindings.wrap_node(graph, pattern_and_nodes[index].1)
                    },
                    graph,
                    index_and_bindings,
                    path,
                )
            },
            location: matched_expr_location,
        },
        matched_var,
    );

    let root = graph.add_node(FlowControlNode::EvaluateExpr(EvaluateExpr {
        expr: expr.matched_expr,
        var_id: matched_var,
        next: match_node_id,
    }));

    graph.finalize(root, ctx)
}
