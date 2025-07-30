//! Functions for lowering nodes of a [super::FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::{MatchArmSelector, corelib};

use super::LowerGraphContext;
use crate::lower::context::VarRequest;
use crate::lower::flow_control::graph::{
    ArmExpr, BooleanIf, EvaluateExpr, FlowControlNode, NodeId,
};
use crate::lower::{lower_expr_to_var_usage, lower_tail_expr};
use crate::{MatchArm, MatchEnumInfo, MatchInfo};

/// Lowers the node with the given [NodeId].
pub fn lower_node(ctx: &mut LowerGraphContext<'_, '_, '_>, id: NodeId) -> Maybe<()> {
    match &ctx.graph.nodes[id.0] {
        FlowControlNode::EvaluateExpr(node) => lower_evaluate_expr(ctx, id, node),
        FlowControlNode::BooleanIf(node) => lower_boolean_if(ctx, id, node),
        FlowControlNode::ArmExpr(node) => lower_arm_expr(ctx, id, node),
    }
}

/// Lowers an [EvaluateExpr] node.
fn lower_evaluate_expr<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &EvaluateExpr<'db>,
) -> Maybe<()> {
    let mut builder = ctx.start_builder(id);

    // TODO(lior): Replace unwrap with a proper error handling.
    let lowered_var = lower_expr_to_var_usage(ctx.ctx, &mut builder, node.expr).unwrap();
    ctx.register_var(node.var_id, lowered_var);
    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(())
}

/// Lowers a [BooleanIf] node.
fn lower_boolean_if(
    ctx: &mut LowerGraphContext<'_, '_, '_>,
    id: NodeId,
    node: &BooleanIf,
) -> Maybe<()> {
    let db = ctx.ctx.db;

    let builder = ctx.start_builder(id);

    // Allocate block ids for the two branches.
    let true_branch_block_id = ctx.assign_child_block_id(node.true_branch, &builder);
    let false_branch_block_id = ctx.assign_child_block_id(node.false_branch, &builder);

    // Create dummy variables for the unit type inside the bool's variants.
    let unit_ty = corelib::unit_ty(db);
    let true_branch_var_id = ctx.ctx.new_var(VarRequest { ty: unit_ty, location: ctx.location });
    let false_branch_var_id = ctx.ctx.new_var(VarRequest { ty: unit_ty, location: ctx.location });

    // Finalize the block.
    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(db),
        input: ctx.vars[&node.condition_var],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::false_variant(db)),
                block_id: false_branch_block_id,
                var_ids: vec![false_branch_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::true_variant(db)),
                block_id: true_branch_block_id,
                var_ids: vec![true_branch_var_id],
            },
        ],
        location: ctx.location,
    });

    ctx.finalize_with_match(id, builder, match_info);
    Ok(())
}

/// Lowers an [ArmExpr] node.
fn lower_arm_expr<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &ArmExpr<'db>,
) -> Maybe<()> {
    let builder = ctx.start_builder(id);
    let sealed_block = lower_tail_expr(ctx.ctx, builder, node.expr)?;
    ctx.add_sealed_block(sealed_block);
    Ok(())
}
