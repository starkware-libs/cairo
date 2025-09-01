//! Functions for lowering nodes of a [super::FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::unit_ty;
use cairo_lang_semantic::{
    self as semantic, GenericArgumentId, MatchArmSelector, ValueSelectorArm, corelib,
};
use cairo_lang_syntax::node::TypedStablePtr;
use itertools::zip_eq;
use num_bigint::BigInt;

use super::LowerGraphContext;
use crate::diagnostic::{
    LoweringDiagnosticKind, LoweringDiagnosticsBuilder, MatchDiagnostic, MatchError,
};
use crate::ids::SemanticFunctionIdEx;
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{LoweredExpr, VarRequest};
use crate::lower::flow_control::graph::{
    ArmExpr, BindVar, BooleanIf, Deconstruct, Downcast, EnumMatch, EqualsLiteral, EvaluateExpr,
    FlowControlNode, NodeId, Upcast, ValueMatch,
};
use crate::lower::{
    generators, lower_expr, lower_expr_literal_to_var_usage, lower_expr_to_var_usage,
};
use crate::{MatchArm, MatchEnumInfo, MatchEnumValue, MatchExternInfo, MatchInfo, VarUsage};

/// Lowers the node with the given [NodeId].
pub fn lower_node(ctx: &mut LowerGraphContext<'_, '_, '_>, id: NodeId) -> Maybe<()> {
    let Some(builder) = ctx.get_builder_if_reachable(id) else {
        // If an [ArmExpr] node is unreachable, report an error.
        if let FlowControlNode::ArmExpr(node) = ctx.graph.node(id) {
            let stable_ptr = ctx.ctx.function_body.arenas.exprs[node.expr].stable_ptr();

            let match_error = LoweringDiagnosticKind::MatchError(MatchError {
                kind: ctx.graph.kind(),
                error: MatchDiagnostic::UnreachableMatchArm,
            });
            ctx.ctx.diagnostics.report(stable_ptr, match_error);
        }

        return Ok(());
    };

    match ctx.graph.node(id) {
        FlowControlNode::EvaluateExpr(node) => lower_evaluate_expr(ctx, id, node, builder),
        FlowControlNode::BooleanIf(node) => lower_boolean_if(ctx, id, node, builder),
        FlowControlNode::ArmExpr(node) => lower_arm_expr(ctx, id, node, builder),
        FlowControlNode::UnitResult => lower_unit_result(ctx, id, builder),
        FlowControlNode::EnumMatch(node) => lower_enum_match(ctx, id, node, builder),
        FlowControlNode::ValueMatch(node) => lower_value_match(ctx, id, node, builder),
        FlowControlNode::EqualsLiteral(node) => lower_equals_literal(ctx, id, node, builder),
        FlowControlNode::BindVar(node) => lower_bind_var(ctx, id, node, builder),
        FlowControlNode::Deconstruct(node) => lower_deconstruct(ctx, id, node, builder),
        FlowControlNode::Upcast(node) => lower_upcast(ctx, id, node, builder),
        FlowControlNode::Downcast(node) => lower_downcast(ctx, id, node, builder),
        FlowControlNode::Missing(diag_added) => Err(*diag_added),
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
    id: NodeId,
    node: &ArmExpr,
    mut builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let lowered_expr = lower_expr(ctx.ctx, &mut builder, node.expr);
    ctx.finalize_with_arm(id, builder, lowered_expr)?;
    Ok(())
}

/// Lowers a `UnitResult` node.
fn lower_unit_result<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    ctx.finalize_with_arm(
        id,
        builder,
        Ok(LoweredExpr::Tuple { exprs: vec![], location: ctx.location }),
    )?;
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

/// Lowers an [ValueMatch] node.
fn lower_value_match<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &ValueMatch,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let db = ctx.ctx.db;
    let unit_type = unit_ty(db);

    let arms = node
        .nodes
        .iter()
        .enumerate()
        .map(|(value, variant_node)| MatchArm {
            arm_selector: MatchArmSelector::Value(ValueSelectorArm { value }),
            block_id: ctx.assign_child_block_id(*variant_node, &builder),
            var_ids: vec![ctx.ctx.new_var(VarRequest { ty: unit_type, location: ctx.location })],
        })
        .collect();

    let match_info = MatchInfo::Value(MatchEnumValue {
        num_of_arms: node.nodes.len(),
        arms,
        input: ctx.vars[&node.matched_var],
        location: ctx.location,
    });

    ctx.finalize_with_match(id, builder, match_info);
    Ok(())
}

/// Lowers an [EqualsLiteral] node.
fn lower_equals_literal<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &EqualsLiteral<'db>,
    mut builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let db = ctx.ctx.db;
    let felt252_ty = db.core_info().felt252;

    let literal_stable_ptr = node.stable_ptr.untyped();
    let literal_location = ctx.ctx.get_location(literal_stable_ptr);
    let input_var = ctx.vars[&node.input];

    // Lower the expression `input_var - literal`.
    let is_equal: VarUsage<'db> = if node.literal == BigInt::from(0) {
        // If the literal is 0, simply use the input variable.
        input_var
    } else {
        // Lower the literal to a [VarUsage].
        let literal_var_usage: VarUsage<'db> = lower_expr_literal_to_var_usage(
            ctx.ctx,
            literal_stable_ptr,
            felt252_ty,
            &node.literal,
            &mut builder,
        );

        let call_result = generators::Call {
            function: corelib::felt252_sub(db).lowered(db),
            inputs: vec![input_var, literal_var_usage],
            coupon_input: None,
            extra_ret_tys: vec![],
            ret_tys: vec![felt252_ty],
            location: literal_location,
        }
        .add(ctx.ctx, &mut builder.statements);

        call_result.returns.into_iter().next().unwrap()
    };

    // Allocate block ids for the two branches.
    let true_branch_block_id = ctx.assign_child_block_id(node.true_branch, &builder);
    let false_branch_block_id = ctx.assign_child_block_id(node.false_branch, &builder);

    // Create dummy variable for the non-zero return value of `core_felt252_is_zero`.
    let non_zero_type = corelib::core_nonzero_ty(db, felt252_ty);
    let false_branch_nonzero_var_id =
        ctx.ctx.new_var(VarRequest { ty: non_zero_type, location: literal_location });

    // Finalize the block.
    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(db).lowered(db),
        inputs: vec![is_equal],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_zero_variant(
                    db, felt252_ty,
                )),
                block_id: true_branch_block_id,
                var_ids: vec![],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_nonzero_variant(
                    db, felt252_ty,
                )),
                block_id: false_branch_block_id,
                var_ids: vec![false_branch_nonzero_var_id],
            },
        ],
        location: literal_location,
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
///
/// Deconstructs the input [super::FlowControlVar] into its members (tuple or struct), and binds the
/// members to the output [super::FlowControlVar]s.
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
        ctx.register_var(*output, VarUsage { var_id, location: output.location(ctx.graph) });
    }

    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(())
}

/// Lowers an [Upcast] node.
fn lower_upcast<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &Upcast,
    mut builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let db = ctx.ctx.db;
    let input_ty = node.input.ty(ctx.graph);
    let output_ty = node.output.ty(ctx.graph);

    let generic_args = vec![GenericArgumentId::Type(input_ty), GenericArgumentId::Type(output_ty)];
    let function = db.core_info().upcast_fn.concretize(db, generic_args).lowered(db);

    let call_result = generators::Call {
        function,
        inputs: vec![ctx.vars[&node.input]],
        coupon_input: None,
        extra_ret_tys: vec![],
        ret_tys: vec![output_ty],
        location: node.input.location(ctx.graph),
    }
    .add(ctx.ctx, &mut builder.statements);

    ctx.register_var(node.output, call_result.returns.into_iter().next().unwrap());
    ctx.pass_builder_to_child(id, node.next, builder);
    Ok(())
}

/// Lowers a [Downcast] node.
fn lower_downcast<'db>(
    ctx: &mut LowerGraphContext<'db, '_, '_>,
    id: NodeId,
    node: &Downcast,
    builder: BlockBuilder<'db>,
) -> Maybe<()> {
    let db = ctx.ctx.db;
    let input_ty = node.input.ty(ctx.graph);
    let output_ty = node.output.ty(ctx.graph);

    let generic_args = vec![GenericArgumentId::Type(input_ty), GenericArgumentId::Type(output_ty)];

    let function_id = db.core_info().downcast_fn.concretize(db, generic_args).lowered(db);

    // Allocate block ids for the two branches.
    let in_range_block_id = ctx.assign_child_block_id(node.in_range, &builder);
    let out_of_range_block_id = ctx.assign_child_block_id(node.out_of_range, &builder);

    // Create the output variable.
    let output_location = node.output.location(ctx.graph);
    let output_var = ctx.ctx.new_var(VarRequest { ty: output_ty, location: output_location });
    let var_usage = VarUsage { var_id: output_var, location: output_location };
    ctx.register_var(node.output, var_usage);

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: function_id,
        inputs: vec![ctx.vars[&node.input]],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_some_variant(
                    db, output_ty,
                )),
                block_id: in_range_block_id,
                var_ids: vec![output_var],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_none_variant(
                    db, output_ty,
                )),
                block_id: out_of_range_block_id,
                var_ids: vec![],
            },
        ],
        location: ctx.location,
    });

    ctx.finalize_with_match(id, builder, match_info);
    Ok(())
}
