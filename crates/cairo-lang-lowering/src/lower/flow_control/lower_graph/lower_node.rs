//! Functions for lowering nodes of a [super::FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::{self as semantic, MatchArmSelector, corelib};
use cairo_lang_syntax::node::TypedStablePtr;
use itertools::zip_eq;

use super::LowerGraphContext;
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{LoweredExpr, VarRequest};
use crate::lower::flow_control::graph::{
    ArmExpr, BindVar, BooleanIf, Deconstruct, EnumMatch, EvaluateExpr, FlowControlNode, NodeId
};
use crate::lower::{generators, lower_expr_to_var_usage, lower_tail_expr, lowered_expr_to_block_scope_end};
use crate::{MatchArm, MatchEnumInfo, MatchInfo, VarUsage};

/// Lowers the node with the given [NodeId].
pub fn lower_node(ctx: &mut LowerGraphContext<'_, '_, '_>, id: NodeId) -> Maybe<()> {
    let Some(builder) = ctx.get_builder_if_reachable(id) else {
        return Ok(());
    };

    match ctx.graph.node(id) {
        FlowControlNode::EvaluateExpr(node) => lower_evaluate_expr(ctx, id, node, builder),
        FlowControlNode::BooleanIf(node) => lower_boolean_if(ctx, id, node, builder),
        FlowControlNode::ArmExpr(node) => lower_arm_expr(ctx, node, builder),
        FlowControlNode::UnitResult => lower_unit_result(ctx, builder),
        FlowControlNode::EnumMatch(node) => lower_enum_match(ctx, id, node, builder),
        FlowControlNode::BindVar(node) => lower_bind_var(ctx, id, node, builder),
        FlowControlNode::Deconstruct(node) => lower_deconstruct(ctx, id, node, builder),
    }
}

/// Lowers an [EvaluateExpr] node.
fn lower_evaluate_expr<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &EvaluateExpr,
    mut builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let lowered_expr = lower_expr_to_var_usage(ctx.ctx, &mut builder, node.expr);
    match lowered_expr {
        Ok(lowered_var) => {
            ctx.register_var(node.var_id, lowered_var);
            ctx.pass_builder_to_child(id, node.next, builder);
            Ok(())
        }
        Err(err) => ctx.block_doesnt_continue(id, builder, err),
    }
}

/// Lowers a [BooleanIf] node.
fn lower_boolean_if<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &BooleanIf,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let db = ctx.ctx.db;

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
    node: &ArmExpr,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let sealed_block = lower_tail_expr(ctx.ctx, builder, node.expr)?;
    ctx.add_sealed_block(sealed_block);
    Ok(())
}

/// Lowers a `UnitResult` node.
fn lower_unit_result<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let sealed_block = lowered_expr_to_block_scope_end(
        ctx.ctx,
        builder,
        Ok(LoweredExpr::Tuple { exprs: vec![], location: ctx.location }),
    )?;
    ctx.add_sealed_block(sealed_block);

    Ok(())
}

/// Lowers an [EnumMatch] node.
fn lower_enum_match<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &EnumMatch<'db>,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let match_location = node.matched_var.location(ctx.graph);

    let arms: Vec<MatchArm<'db>> = node
        .variants
        .iter()
        .map(|(concrete_variant, variant_node, flow_control_var)| {
            let var_ty = flow_control_var.ty(ctx.graph);
            let var_location = flow_control_var.location(ctx.graph);
            // Create a variable for the variant inner value.
            let variant_var = ctx.ctx.new_var(VarRequest { ty: var_ty, location: var_location });
            let var_usage = VarUsage { var_id: variant_var, location: var_location };
            ctx.register_var(*flow_control_var, var_usage);
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(*concrete_variant),
                block_id: ctx.assign_child_block_id(*variant_node, &builder),
                var_ids: vec![variant_var],
            }
        })
        .collect();

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: node.concrete_enum_id,
        input: ctx.vars[&node.matched_var],
        arms,
        location: match_location,
    });

    ctx.finalize_with_match(id, builder, match_info);
    Ok(())
}

/// Lowers a [BindVar] node.
fn lower_bind_var<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &BindVar,
    mut builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let pattern_variable = node.output.get(ctx.graph);
    let var_id = ctx.vars[&node.input].var_id;

    // Override variable location to with the location of the variable in the pattern.
    // TODO(lior): Consider using the location of the first instance of the pattern binding instead
    //   of overriding each time `BindVar` is visited.
    ctx.ctx.variables.variables[var_id].location =
        ctx.ctx.get_location(pattern_variable.stable_ptr.untyped());

    // Bind the semantic variable to the lowered variable, and update `semantic_defs` in the
    // `EncapsulatingLoweringContext`.
    let sem_var = semantic::Binding::LocalVar(pattern_variable.var.clone());
    builder.put_semantic(sem_var.id(), var_id);
    ctx.ctx.semantic_defs.insert(sem_var.id(), sem_var);

    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(())
}

/// Lowers a [Deconstruct] node.
fn lower_deconstruct<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &Deconstruct,
    mut builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let var_requests = node
        .outputs
        .iter()
        .map(|output| VarRequest { ty: output.ty(ctx.graph), location: output.location(ctx.graph) })
        .collect();

    let variable_ids =
        generators::StructDestructure { input: ctx.vars[&node.input], var_reqs: var_requests }
            .add(ctx.ctx, &mut builder.statements);

    for (var_id, output) in zip_eq(variable_ids, &node.outputs) {
        ctx.register_var(
            *output,
            VarUsage {
                var_id,
                // The variable is used immediately after the destructure, so the usage
                // location is the same as the definition location.
                location: ctx.ctx.variables[var_id].location,
            },
        );
    }

    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(())
}
