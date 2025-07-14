use bincode::config::Varint;
use cairo_lang_defs::ids::VariantId;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::types::peel_snapshots;
use cairo_lang_semantic::{
    self as semantic, ConcreteTypeId, Condition, Pattern, PatternEnumVariant, PatternId,
    PatternOtherwise, TypeLongId,
};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::zip_eq;

use super::graph::{
    ArmExpr, BooleanIf, EnumMatch, ExprToVar, FlowControlGraph, FlowControlGraphBuilder,
    FlowControlNode, FlowControlVar, NodeId,
};
use crate::lower::context::LoweringContext;

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

                // Create a list of patterns and nodes.
                let pattern_and_nodes: Vec<(PatternId, NodeId)> =
                    patterns.iter().map(|pattern| (pattern.clone(), current_node)).collect();

                let match_node_id = lower_patterns(
                    ctx,
                    &mut graph,
                    expr_var,
                    expr.stable_ptr().untyped(),
                    &pattern_and_nodes,
                    Some(false_branch),
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
        &pattern_and_nodes,
        None,
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
fn lower_patterns(
    ctx: &mut LoweringContext<'_, '_>,
    graph: &mut FlowControlGraphBuilder,
    input_var: FlowControlVar,
    stable_ptr: SyntaxStablePtrId,
    patterns: &[(PatternId, NodeId)],
    default_node: Option<NodeId>,
) -> NodeId {
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, input_var.ty());
    // TODO: Handle n_snapshots.
    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else { todo!() };

    let concrete_variants = ctx.db.concrete_enum_variants(concrete_enum_id).unwrap(); // TODO: Fix unwrap.

    let mut variants: Vec<Option<NodeId>> = vec![None; concrete_variants.len()];
    for (pattern, node_id) in patterns {
        match ctx.function_body.arenas.patterns[*pattern] {
            Pattern::EnumVariant(PatternEnumVariant { variant, inner_pattern, .. }) => {
                // TODO: handle inner_pattern.
                if variants[variant.idx].is_none() {
                    variants[variant.idx] = Some(*node_id);
                } else {
                    // TODO: Produce diagnostics.
                }
            }
            Pattern::Otherwise(PatternOtherwise { .. }) => {
                for variant in variants.iter_mut() {
                    variant.get_or_insert(*node_id);
                }
            }
            _ => {
                todo!()
            }
        }
    }

    // Fill default.
    let variants = zip_eq(concrete_variants, variants)
        .map(|(concrete_variant, variant_node)| {
            (
                concrete_variant,
                variant_node.or(default_node).unwrap(),
                graph.new_var(concrete_variant.ty),
            )
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
