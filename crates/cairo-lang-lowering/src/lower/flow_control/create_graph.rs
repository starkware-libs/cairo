use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::types::peel_snapshots;
use cairo_lang_semantic::{
    self as semantic, ConcreteEnumId, ConcreteTypeId, Condition,
    PatternEnumVariant, PatternId, PatternTuple, TypeId, TypeLongId,
};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::iterators::zip_eq3;
use itertools::Itertools;

use super::graph::{
    ArmExpr, BooleanIf, Deconstruct, EnumMatch, ExprToVar, FlowControlGraph,
    FlowControlGraphBuilder, FlowControlNode, FlowControlVar, NodeId,
};
use crate::lower::context::LoweringContext;

#[derive(Clone)]
enum Pattern {
    Semantic(semantic::PatternId),
    Otherwise
}

/// Creates a graph node for [semantic::ExprIf].
#[allow(dead_code)]
pub fn create_graph_expr_if(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprIf,
) -> FlowControlGraph {
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
            Condition::Let(expr_id, patterns) => {
                // Get the type of the expression.
                // TODO: peel_snapshots?

                let expr = &ctx.function_body.arenas.exprs[*expr_id];

                // Create a variable for the expression.
                let expr_var = graph.new_var(expr.ty());

                let match_node_id = lower_patterns(
                    ctx,
                    &mut graph,
                    expr_var,
                    expr.stable_ptr().untyped(),
                    &patterns.iter().map(|pattern| Pattern::Semantic(*pattern)).collect_vec(),
                    &|_ctx, _graph, pattern_indices| {
                        if pattern_indices.len() >= 1 { current_node } else { false_branch }
                    },
                );

                // Create a node for lowering `expr` into `expr_var` and continue to the match.
                graph.add_node(FlowControlNode::ExprToVar(ExprToVar {
                    expr: expr_id.clone(),
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
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
) -> FlowControlGraph {
    let mut graph = FlowControlGraphBuilder::default();

    let matched_expr = &ctx.function_body.arenas.exprs[expr.matched_expr];
    let matched_var = graph.new_var(matched_expr.ty());

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

    let match_node_id = lower_patterns(
        ctx,
        &mut graph,
        matched_var,
        matched_expr.stable_ptr().untyped(),
        &pattern_and_nodes.iter().map(|(pattern, _)| Pattern::Semantic(*pattern)).collect_vec(),
        &|_ctx, _graph, pattern_indices|
        {
            // TODO: produce diagnostics if pattern_indices is empty.
            pattern_and_nodes[pattern_indices[0]].1
        }
    );

    let root = graph.add_node(FlowControlNode::ExprToVar(ExprToVar {
        expr: expr.matched_expr.clone(),
        var_id: matched_var,
        next: match_node_id,
    }));

    graph.finalize(root)
}

/// Given a list of patterns and the nodes to go to if the pattern matches,
/// returns a new graph node to handle the patterns.
///
/// `stable_ptr` is the stable pointer of the expression initiating the match.
///
/// If `default` is provided, the patterns do not need to be exhaustive, and the default node
/// will be used if no pattern matches.
///
/// TODO: doc build_node_callback.
fn lower_patterns(
    ctx: &mut LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    stable_ptr: SyntaxStablePtrId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(
        &mut LoweringContext<'_, '_>,
        &mut FlowControlGraphBuilder,
        Vec<usize>,
    ) -> NodeId,
) -> NodeId {
    // TODO: Check for semantic::Pattern::Otherwise in addition to Pattern::Otherwise.
    if patterns.iter().all(|pattern| pattern_is_any(ctx, pattern)) {
        return build_node_callback(ctx, graph, (0..patterns.len()).collect_vec());
    }

    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, input_var.ty());
    // TODO: Handle n_snapshots.
    match long_ty {
        TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) => lower_patterns_enum(
            ctx,
            graph,
            input_var,
            concrete_enum_id,
            stable_ptr,
            patterns,
            build_node_callback,
        ),
        TypeLongId::Tuple(types) => lower_patterns_tuple(
            ctx,
            graph,
            input_var,
            &types,
            stable_ptr,
            patterns,
            build_node_callback,
        ),
        _ => todo!("{:?}", long_ty),
    }
}

fn lower_patterns_enum(
    ctx: &mut LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    concrete_enum_id: ConcreteEnumId,
    stable_ptr: SyntaxStablePtrId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(
        &mut LoweringContext<'_, '_>,
        &mut FlowControlGraphBuilder,
        Vec<usize>,
    ) -> NodeId,
) -> NodeId {
    let concrete_variants = ctx.db.concrete_enum_variants(concrete_enum_id).unwrap(); // TODO: Fix unwrap.

    // Maps variant index to the list of the indices of the patterns that match it.
    let mut variant_to_pattern_indices: Vec<Vec<usize>> = vec![vec![]; concrete_variants.len()];
    // TODO: doc.
    let mut variant_to_inner_patterns: Vec<Vec<Pattern>> = vec![vec![]; concrete_variants.len()];
    for (idx, pattern) in patterns.iter().enumerate() {
        match pattern {
            Pattern::Semantic(semantic_pattern) => match ctx.function_body.arenas.patterns[*semantic_pattern] {
                semantic::Pattern::EnumVariant(PatternEnumVariant { variant, inner_pattern, .. }) => {
                    variant_to_pattern_indices[variant.idx].push(idx);
                    variant_to_inner_patterns[variant.idx].push(
                        inner_pattern.map(|inner_pattern| Pattern::Semantic(inner_pattern))
                        .unwrap_or_else(|| Pattern::Otherwise));
                }
                semantic::Pattern::Otherwise(..) => {
                    for pattern_indices in variant_to_pattern_indices.iter_mut() {
                        pattern_indices.push(idx);
                    }
                    for inner_patterns in variant_to_inner_patterns.iter_mut() {
                        inner_patterns.push(Pattern::Otherwise);
                    }
                }
                _ => todo!(),
            }
            Pattern::Otherwise => {
                // TODO: avoid code duplication.
                for pattern_indices in variant_to_pattern_indices.iter_mut() {
                    pattern_indices.push(idx);
                }
                for inner_patterns in variant_to_inner_patterns.iter_mut() {
                    inner_patterns.push(Pattern::Otherwise);
                }
            }
        }
    }

    // Create the inner nodes.
    let variants = zip_eq3(concrete_variants, variant_to_pattern_indices, variant_to_inner_patterns)
        .map(|(concrete_variant, pattern_indices, inner_patterns)| {
            let inner_var = graph.new_var(concrete_variant.ty);
            let node = lower_patterns(ctx,
                graph,
                inner_var,
                stable_ptr, // TODO: Check.
                &inner_patterns,
                &|ctx, graph, pattern_indices_inner| {
                    build_node_callback(ctx, graph, pattern_indices_inner.iter().map(|idx| pattern_indices[*idx]).collect_vec())
                }
            );
            ( concrete_variant, node, inner_var )
        })
        .collect(); // TODO: fix unwrap.

    // Create a node for the match.
    graph.add_node(FlowControlNode::EnumMatch(EnumMatch {
        matched_var: input_var,
        concrete_enum_id,
        variants,
        stable_ptr,
    }))
}

fn lower_patterns_tuple(
    ctx: &mut LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    types: &Vec<TypeId>,
    stable_ptr: SyntaxStablePtrId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(
        &mut LoweringContext<'_, '_>,
        &mut FlowControlGraphBuilder,
        Vec<usize>,
    ) -> NodeId,
) -> NodeId {
    let inner_vars = types.iter().map(|ty| graph.new_var(*ty)).collect_vec();

    let node = lower_patterns_tuple_inner(
        ctx,
        graph,
        &inner_vars,
        types,
        stable_ptr,
        patterns,
        build_node_callback,
        0,
    );

    // Deconstruct the input variable.
    graph.add_node(FlowControlNode::Deconstruct(Deconstruct {
        input: input_var,
        outputs: inner_vars,
        next: node,
    }))
}



// TODO: doc item_idx.
fn lower_patterns_tuple_inner(
    ctx: &mut LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    inner_vars: &Vec<FlowControlVar>,
    types: &Vec<TypeId>,
    stable_ptr: SyntaxStablePtrId,
    patterns: &[Pattern],
    build_node_callback: &dyn Fn(
        &mut LoweringContext<'_, '_>,
        &mut FlowControlGraphBuilder,
        Vec<usize>,
    ) -> NodeId,
    item_idx: usize,
) -> NodeId {
    if item_idx == types.len() {
        return build_node_callback( ctx, graph, (0..patterns.len()).collect_vec() );
    }

    // TODO: Zero length tuple.
    let patterns_on_current_item = patterns
        .iter()
        .map(|pattern| {
            match pattern {
                Pattern::Semantic(semantic_pattern) => match &ctx.function_body.arenas.patterns[*semantic_pattern] {
                    semantic::Pattern::Tuple(PatternTuple { field_patterns, .. }) => Pattern::Semantic(field_patterns[item_idx]),
                    semantic::Pattern::Otherwise(..) => Pattern::Otherwise,
                    _ => todo!(),
                }
                Pattern::Otherwise => Pattern::Otherwise,
            }
        })
        .collect_vec();

    lower_patterns(
        ctx,
        graph,
        inner_vars[item_idx],
        stable_ptr, // TODO: Check,
        &patterns_on_current_item,
        &|ctx, graph, pattern_indices| {
            lower_patterns_tuple_inner(
                ctx,
                graph,
                inner_vars,
                types,
                stable_ptr,
                &pattern_indices.iter().map(|idx| patterns[*idx].clone()).collect_vec(),
                &|ctx, graph, pattern_indices_inner| {
                    build_node_callback( ctx, graph, pattern_indices_inner.iter().map(|idx| pattern_indices[*idx]).collect_vec() )
                },
                item_idx + 1,
            )
        },
    )
}

/// Returns true if all patterns in the slice are `Pattern::Otherwise`.
fn pattern_is_any(ctx: &LoweringContext<'_, '_>, pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Semantic(semantic_pattern) => match &ctx.function_body.arenas.patterns[*semantic_pattern] {
            semantic::Pattern::Otherwise(..) => true,
            semantic::Pattern::Variable(..) => true,
            _ => false,
        }
        Pattern::Otherwise => true,
    }
}
