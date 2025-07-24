//! Functions for lowering nodes of a [super::FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::{self as semantic, MatchArmSelector, corelib};
use cairo_lang_syntax::node::TypedStablePtr;

use super::{BlockFinalization, LowerGraphContext};
use crate::ids::LocationId;
use crate::lower::context::{LoweringContext, VarRequest};
use crate::lower::flow_control::graph::{ArmExpr, BooleanIf, ExprToVar, FlowControlNode, NodeId};
use crate::lower::{lower_expr_to_var_usage, lower_tail_expr};
use crate::{BlockEnd, MatchArm, MatchEnumInfo, MatchInfo};

/// Lowers the node with the given [NodeId].
pub fn lower_node(ctx: &mut LowerGraphContext<'_, '_, '_>, id: NodeId) -> Maybe<()> {
    let block_end = match &ctx.graph.nodes[id.0] {
        FlowControlNode::BooleanIf(node) => lower_boolean_if(ctx, id, node),
        FlowControlNode::ArmExpr(node) => lower_arm_expr(ctx, id, node),
        FlowControlNode::ExprToVar(node) => lower_expr_to_var(ctx, id, node),
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

    let condition_var = ctx.vars[&node.condition_var];
    let condition_location = condition_var.location;
    let builder = ctx.start_builder(id, condition_location);

    // Allocate/retrieve block ids for the two branches.
    let true_branch_block_id = ctx.assign_child_block_id(node.true_branch, &builder);
    let false_branch_block_id = ctx.assign_child_block_id(node.false_branch, &builder);

    // Create dummy variables for the unit type inside the bool's variants.
    let unit_ty = corelib::unit_ty(db);
    let true_branch_var_id =
        ctx.ctx.new_var(VarRequest { ty: unit_ty, location: condition_location });
    let false_branch_var_id =
        ctx.ctx.new_var(VarRequest { ty: unit_ty, location: condition_location });

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
        location: condition_location,
    });

    Ok(BlockFinalization::End(builder, BlockEnd::Match { info: match_info }, condition_location))
}

/// Lowers an [ArmExpr] node.
fn lower_arm_expr(
    ctx: &mut LowerGraphContext<'_, '_, '_>,
    id: NodeId,
    node: &ArmExpr,
) -> Maybe<BlockFinalization> {
    let builder = ctx.start_builder(id, get_expr_location(ctx.ctx, &node.expr));
    let sealed_block = lower_tail_expr(ctx.ctx, builder, node.expr)?;
    Ok(BlockFinalization::Sealed(sealed_block))
}

/// Lowers an [ExprToVar] node.
fn lower_expr_to_var(
    ctx: &mut LowerGraphContext<'_, '_, '_>,
    id: NodeId,
    node: &ExprToVar,
) -> Maybe<BlockFinalization> {
    let mut builder = ctx.start_builder(id, get_expr_location(ctx.ctx, &node.expr));

    // TODO(lior): Replace unwrap with a proper error handling.
    let lowered_var = lower_expr_to_var_usage(ctx.ctx, &mut builder, node.expr).unwrap();
    ctx.register_var(node.var_id, lowered_var);
    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(BlockFinalization::Skip)
}

/// Returns the location of the given expression.
fn get_expr_location(ctx: &LoweringContext<'_, '_>, expr: &semantic::ExprId) -> LocationId {
    let condition_expr = &ctx.function_body.arenas.exprs[*expr];
    let stable_ptr = condition_expr.stable_ptr().untyped();
    ctx.get_location(stable_ptr)
}
