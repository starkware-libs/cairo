//! Functions for lowering nodes of a [super::FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::{self as semantic, MatchArmSelector, corelib};
use cairo_lang_syntax::node::TypedStablePtr;

use super::{BlockFinalization, LowerGraphContext};
use crate::ids::LocationId;
use crate::lower::context::{LoweringContext, VarRequest};
use crate::lower::flow_control::graph::{
    ArmExpr, BooleanIf, Capture, EnumMatch, ExprToVar, FlowControlNode, NodeId,
};
use crate::lower::{
    lower_expr, lower_expr_to_var_usage, lower_tail_expr, lowered_expr_to_block_scope_end,
};
use crate::{BlockEnd, MatchArm, MatchEnumInfo, MatchInfo, VarUsage};

// TODO: Should we use `LoweringResult`?
/// Lowers the node with the given [NodeId].
pub fn lower_node(ctx: &mut LowerGraphContext<'_, '_, '_>, id: NodeId) -> Maybe<()> {
    let block_end = match &ctx.graph.nodes[id.0] {
        FlowControlNode::BooleanIf(node) => lower_boolean_if(ctx, id, node),
        FlowControlNode::ArmExpr(node) => lower_arm_expr(ctx, id, node),
        FlowControlNode::ExprToVar(node) => lower_expr_to_var(ctx, id, node),
        FlowControlNode::EnumMatch(node) => lower_enum_match(ctx, id, node),
        FlowControlNode::Capture(node) => lower_capture(ctx, id, node),
        _ => todo!(),
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

    let Some(builder) = ctx.start_builder(id, node.condition_var.location()) else {
        return Ok(BlockFinalization::Missing);
    };

    let condition_var = ctx.vars[&node.condition_var];
    let condition_location = condition_var.location;

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
    let Some(builder) = ctx.start_builder(id, get_expr_location(ctx.ctx, &node.expr)) else {
        return Ok(BlockFinalization::Missing);
    };

    let sealed_block = lower_tail_expr(ctx.ctx, builder, node.expr)?;
    Ok(BlockFinalization::Sealed(sealed_block))
}

fn lower_expr_to_var(
    ctx: &mut LowerGraphContext<'_, '_, '_>,
    id: NodeId,
    node: &ExprToVar,
) -> Maybe<BlockFinalization> {
    let Some(mut builder) = ctx.start_builder(id, get_expr_location(ctx.ctx, &node.expr)) else {
        return Ok(BlockFinalization::Missing);
    };

    let lowered_expr = lower_expr_to_var_usage(ctx.ctx, &mut builder, node.expr);
    match lowered_expr {
        Ok(lowered_var) => {
            ctx.register_var(node.var_id, lowered_var);
            ctx.pass_builder_to_child(id, node.next, builder);
            Ok(BlockFinalization::Skip)
        }
        Err(err) => Ok(BlockFinalization::JumpsOutside(builder, err)),
    }
}

fn lower_enum_match(
    ctx: &mut LowerGraphContext,
    id: NodeId,
    node: &EnumMatch,
) -> Maybe<BlockFinalization> {
    let match_location = node.matched_var.location();

    let Some(builder) = ctx.start_builder(id, match_location) else {
        return Ok(BlockFinalization::Missing);
    };

    let arms: Vec<MatchArm> = node
        .variants
        .iter()
        .map(|(concrete_variant, variant_node, inner_var_id)| {
            let location = match_location; // TODO: Fix.
            let input_var = ctx.ctx.new_var(VarRequest {
                ty: concrete_variant.ty, // TODO: wrap in snapshots
                location,
            });
            let var_usage = VarUsage { var_id: input_var, location };
            ctx.register_var(*inner_var_id, var_usage);
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(concrete_variant.clone()),
                block_id: ctx.assign_child_block_id(*variant_node, &builder),
                var_ids: vec![input_var],
            }
        })
        .collect();

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: node.concrete_enum_id,
        input: ctx.vars[&node.matched_var],
        arms,
        location: match_location,
    });

    Ok(BlockFinalization::End(builder, BlockEnd::Match { info: match_info }, match_location))
}

/// Lowers a [Capture] node.
fn lower_capture(
    ctx: &mut LowerGraphContext,
    id: NodeId,
    node: &Capture,
) -> Maybe<BlockFinalization> {
    // TODO: Check location.
    let Some(mut builder) = ctx.start_builder(id, node.input.location()) else {
        return Ok(BlockFinalization::Missing);
    };

    let sem_var = semantic::Binding::LocalVar(node.output.var.clone());
    // Deposit the owned variable in the semantic variables store.
    let var = ctx.vars[&node.input].var_id;
    // TODO: Check if the following is needed.
    // Override variable location.
    // ctx.ctx.variables.variables[var].location =
    // ctx.ctx.get_location(node.output.stable_ptr.untyped());
    builder.put_semantic(sem_var.id(), var);
    ctx.ctx.semantic_defs.insert(sem_var.id(), sem_var);

    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(BlockFinalization::Skip)
}

/// Returns the location of the given expression.
fn get_expr_location(ctx: &LoweringContext<'_, '_>, expr: &semantic::ExprId) -> LocationId {
    let condition_expr = &ctx.function_body.arenas.exprs[*expr];
    let stable_ptr = condition_expr.stable_ptr().untyped();
    ctx.get_location(stable_ptr)
}
