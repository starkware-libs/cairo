use cache::Cache;
use cairo_lang_semantic::{self as semantic, Condition, ExprId, PatternId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use filtered_patterns::{FilteredPatterns, IndexAndBindings};
use itertools::{Itertools, zip_eq};
use patterns::{CreateNodeParams, create_node_for_patterns, get_pattern};

use super::graph::{
    ArmExpr, BooleanIf, EvaluateExpr, FlowControlGraph, FlowControlGraphBuilder, FlowControlNode,
    FlowControlVar, LetElseSuccess, NodeId, PatternVarId, RefreshVar, WhileBody,
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
pub fn create_graph_expr_match<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprMatch<'db>,
) -> FlowControlGraph<'db> {
    let mut graph = FlowControlGraphBuilder::new(MatchKind::Match);

    let matched_expr = &ctx.function_body.arenas.exprs[expr.matched_expr];
    let matched_expr_location = ctx.get_location(matched_expr.stable_ptr().untyped());
    let matched_var = graph.new_var(matched_expr.ty(), matched_expr_location);

    // Create a list of patterns, nodes and guards.
    let pattern_and_nodes: Vec<(PatternId, NodeId, Option<ExprId>)> = expr
        .arms
        .iter()
        .flat_map(|match_arm| {
            // For each arm, create a node for the arm expression.
            let arm_node =
                graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: match_arm.expression }));
            // Then map the patterns to that node.
            match_arm.patterns.iter().map(move |pattern| (*pattern, arm_node, match_arm.guard))
        })
        .collect();

    let mut cache = Cache::default();
    let mut guarded_cache = Cache::default();

    let match_node_id = create_node_for_patterns(
        CreateNodeParams {
            ctx,
            graph: &mut graph,
            patterns: &pattern_and_nodes
                .iter()
                .map(|(pattern, ..)| Some(get_pattern(ctx, *pattern)))
                .collect_vec(),
            build_node_callback: &mut |graph, pattern_indices, path| {
                // Get the first arm that matches.
                let Some(index_and_bindings) = pattern_indices.clone().first() else {
                    // If no arm is available, report a non-exhaustive match error.
                    let kind = LoweringDiagnosticKind::MatchError(MatchError {
                        kind: MatchKind::Match,
                        error: MatchDiagnostic::NonExhaustiveMatch(path),
                    });
                    return graph.report_with_missing_node(expr.stable_ptr.untyped(), kind);
                };

                // If the arm has a guard, the arms that follow it may be selected as well, so the
                // node depends on the entire list of accepted patterns.
                if pattern_and_nodes[index_and_bindings.index()].2.is_some() {
                    return guarded_cache.get_or_compute(
                        &mut |graph, pattern_indices: FilteredPatterns, path| {
                            create_guarded_arms_node(
                                ctx,
                                graph,
                                expr,
                                &pattern_and_nodes,
                                pattern_indices,
                                path,
                            )
                        },
                        graph,
                        pattern_indices,
                        path,
                    );
                }

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

/// Creates a node for a match whose first accepted pattern belongs to an arm with a guard.
///
/// The candidates are tried in order. For each candidate, the bindings of its pattern are applied
/// and its guard is evaluated. If the guard holds, the arm is chosen; otherwise, the next
/// candidate is tried. A candidate without a guard is always chosen, so the candidates that follow
/// it are not reachable. If all the candidates fail, the match is non-exhaustive.
///
/// A guard may replace the lowered variables of the pattern variables it reads (for example, by
/// taking a snapshot of them). Therefore, after a guard fails, the bindings of the next candidates
/// are taken from the pattern variables of the failed candidate rather than from the original
/// variables.
fn create_guarded_arms_node<'db>(
    ctx: &LoweringContext<'db, '_>,
    graph: &mut FlowControlGraphBuilder<'db>,
    expr: &semantic::ExprMatch<'db>,
    pattern_and_nodes: &[(PatternId, NodeId, Option<ExprId>)],
    pattern_indices: FilteredPatterns,
    path: String,
) -> NodeId {
    let mut candidates = pattern_indices.into_vec();
    let n_reachable = candidates
        .iter()
        .position(|candidate| pattern_and_nodes[candidate.index()].2.is_none())
        .map_or(candidates.len(), |idx| idx + 1);
    candidates.truncate(n_reachable);

    // For each guarded candidate, the variables to refresh once its guard fails.
    let mut refreshes: Vec<Vec<(PatternVarId, FlowControlVar)>> = vec![];
    let mut replacements: UnorderedHashMap<FlowControlVar, FlowControlVar> = Default::default();
    for candidate in candidates.iter_mut() {
        let original_inputs = candidate.bindings().iter().map(|(input, _)| *input).collect_vec();
        *candidate =
            candidate.clone().map_inputs(|input| *replacements.get(&input).unwrap_or(&input));
        let mut candidate_refreshes = vec![];
        if pattern_and_nodes[candidate.index()].2.is_some() {
            for (original_input, (input, pattern_var)) in
                zip_eq(original_inputs, candidate.bindings().iter().cloned())
            {
                let output = graph.new_var(graph.var_ty(input), graph.var_location(input));
                replacements.insert(original_input, output);
                candidate_refreshes.push((pattern_var, output));
            }
        }
        refreshes.push(candidate_refreshes);
    }

    // The node to continue to if none of the candidates processed so far is chosen.
    let mut fallthrough: Option<NodeId> = None;
    for (candidate, candidate_refreshes) in candidates.into_iter().zip(refreshes).rev() {
        let (_, arm_node, guard) = pattern_and_nodes[candidate.index()];
        let Some(guard) = guard else {
            fallthrough = Some(candidate.wrap_node(graph, arm_node));
            continue;
        };
        let mut false_branch = fallthrough.unwrap_or_else(|| {
            let kind = LoweringDiagnosticKind::MatchError(MatchError {
                kind: MatchKind::Match,
                error: MatchDiagnostic::NonExhaustiveMatch(path.clone()),
            });
            graph.report_with_missing_node(expr.stable_ptr.untyped(), kind)
        });
        for (source, output) in candidate_refreshes {
            false_branch = graph.add_node(FlowControlNode::RefreshVar(RefreshVar {
                source,
                output,
                next: false_branch,
            }));
        }

        let guard_expr = &ctx.function_body.arenas.exprs[guard];
        let guard_var =
            graph.new_var(guard_expr.ty(), ctx.get_location(guard_expr.stable_ptr().untyped()));
        let if_node = graph.add_node(FlowControlNode::BooleanIf(BooleanIf {
            condition_var: guard_var,
            true_branch: arm_node,
            false_branch,
        }));
        let evaluate_node = graph.add_node(FlowControlNode::EvaluateExpr(EvaluateExpr {
            expr: guard,
            var_id: guard_var,
            next: if_node,
        }));
        fallthrough = Some(candidate.wrap_node(graph, evaluate_node));
    }

    fallthrough.expect("A guarded arm is always the first candidate.")
}

/// Creates a graph node for a let-else statement.
///
/// See [crate::lower::lower_let_else::lower_let_else] for more details.
pub fn create_graph_expr_let_else<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    pattern: PatternId,
    expr_id: ExprId,
    else_clause: ExprId,
    var_ids_and_stable_ptrs: Vec<(semantic::VarId<'db>, SyntaxStablePtrId<'db>)>,
) -> FlowControlGraph<'db> {
    let mut graph = FlowControlGraphBuilder::new(MatchKind::IfLet);

    // Add the `true` branch (the `if` block).
    let true_branch =
        graph.add_node(FlowControlNode::LetElseSuccess(LetElseSuccess { var_ids_and_stable_ptrs }));

    // Add the `false` branch (the `else` block).
    let false_branch = graph.add_node(FlowControlNode::ArmExpr(ArmExpr { expr: else_clause }));

    let expr = &ctx.function_body.arenas.exprs[expr_id];

    // Create a variable for the expression.
    let expr_location = ctx.get_location(expr.stable_ptr().untyped());
    let expr_var = graph.new_var(expr.ty(), expr_location);

    let match_node_id = create_node_for_patterns(
        CreateNodeParams {
            ctx,
            graph: &mut graph,
            patterns: &[Some(get_pattern(ctx, pattern))],
            build_node_callback: &mut |graph, pattern_indices, _path| {
                if let Some(index_and_bindings) = pattern_indices.first() {
                    index_and_bindings.wrap_node(graph, true_branch)
                } else {
                    false_branch
                }
            },
            location: expr_location,
        },
        expr_var,
    );

    // Create a node for lowering `expr_id` into `expr_var` and continue to the match.
    let root = graph.add_node(FlowControlNode::EvaluateExpr(EvaluateExpr {
        expr: expr_id,
        var_id: expr_var,
        next: match_node_id,
    }));

    graph.finalize(root, ctx)
}

/// Creates a graph node for a while-let statement.
pub fn create_graph_expr_while_let<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    patterns: &[PatternId],
    expr_id: ExprId,
    body: ExprId,
    loop_expr_id: ExprId,
    loop_stable_ptr: SyntaxStablePtrId<'db>,
) -> FlowControlGraph<'db> {
    let mut graph =
        FlowControlGraphBuilder::new(MatchKind::WhileLet(loop_expr_id, loop_stable_ptr));

    // Add the `true` branch (the `while` body).
    let true_branch = graph.add_node(FlowControlNode::WhileBody(WhileBody {
        body,
        loop_expr_id,
        loop_stable_ptr,
    }));

    // Add the `false` branch.
    let false_branch = graph.add_node(FlowControlNode::UnitResult);

    let expr = &ctx.function_body.arenas.exprs[expr_id];

    // Create a variable for the expression.
    let expr_location = ctx.get_location(expr.stable_ptr().untyped());
    let expr_var = graph.new_var(expr.ty(), expr_location);

    let match_node_id = create_node_for_patterns(
        CreateNodeParams {
            ctx,
            graph: &mut graph,
            patterns: &patterns
                .iter()
                .map(|pattern| Some(get_pattern(ctx, *pattern)))
                .collect_vec(),
            build_node_callback: &mut |graph, pattern_indices, _path| {
                if let Some(index_and_bindings) = pattern_indices.first() {
                    index_and_bindings.wrap_node(graph, true_branch)
                } else {
                    false_branch
                }
            },
            location: expr_location,
        },
        expr_var,
    );

    // Create a node for lowering `expr_id` into `expr_var` and continue to the match.
    let root = graph.add_node(FlowControlNode::EvaluateExpr(EvaluateExpr {
        expr: expr_id,
        var_id: expr_var,
        next: match_node_id,
    }));

    graph.finalize(root, ctx)
}
