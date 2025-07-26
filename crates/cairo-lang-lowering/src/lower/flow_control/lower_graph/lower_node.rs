//! Functions for lowering nodes of a [super::FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::{MatchArmSelector, corelib};

use super::{BlockFinalization, LowerGraphContext};
use crate::lower::context::VarRequest;
use crate::lower::flow_control::graph::{ArmExpr, BooleanIf, FlowControlNode, NodeId};
use crate::lower::{lower_expr_to_var_usage, lower_tail_expr};
use crate::{BlockEnd, MatchArm, MatchEnumInfo, MatchInfo};

/// Lowers the node with the given [NodeId].
pub fn lower_node(ctx: &mut LowerGraphContext<'_, '_, '_>, id: NodeId) -> Maybe<()> {
    let block_end = match &ctx.graph.nodes[id.0] {
        FlowControlNode::BooleanIf(node) => lower_boolean_if(ctx, id, node),
        FlowControlNode::ArmExpr(node) => lower_arm_expr(ctx, id, node),
    }?;

    // Update `block_finalizations` for the current node.
    assert!(ctx.block_finalizations.insert(id, block_end).is_none());
    Ok(())
}

/// Lowers a [BooleanIf] node.
fn lower_boolean_if(
    ctx: &mut LowerGraphContext<'_, '_, '_>,
    id: NodeId,
    node: &BooleanIf,
) -> Maybe<BlockFinalization> {
    let db = ctx.ctx.db;

    let mut builder = ctx.start_builder(id);

    let lowered_condition = lower_expr_to_var_usage(ctx.ctx, &mut builder, node.condition);
    // TODO(lior): Replace unwrap with a proper error handling.
    let condition_var = lowered_condition.unwrap();

    // Allocate/retrieve block ids for the two branches.
    let true_branch_block_id = ctx.assign_child_block_id(node.true_branch, &builder);
    let false_branch_block_id = ctx.assign_child_block_id(node.false_branch, &builder);

    // Create dummy variables for the unit type inside the bool's variants.
    let unit_ty = corelib::unit_ty(db);
    let true_branch_var_id = ctx.ctx.new_var(VarRequest { ty: unit_ty, location: ctx.location });
    let false_branch_var_id = ctx.ctx.new_var(VarRequest { ty: unit_ty, location: ctx.location });

    // Finalize the block.
    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(db),
        input: condition_var,
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

    Ok(BlockFinalization::End(builder, BlockEnd::Match { info: match_info }))
}

/// Lowers an [ArmExpr] node.
fn lower_arm_expr(
    ctx: &mut LowerGraphContext<'_, '_, '_>,
    id: NodeId,
    node: &ArmExpr,
) -> Maybe<BlockFinalization> {
    let builder = ctx.start_builder(id);
    let sealed_block = lower_tail_expr(ctx.ctx, builder, node.expr)?;
    Ok(BlockFinalization::Sealed(sealed_block))
}
