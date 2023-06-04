use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib;

use super::block_builder::BlockBuilder;
use super::context::{LoweredExpr, LoweringContext, LoweringResult, VarRequest};
use super::generators::{self, StructConstruct};
use super::{create_subscope_with_bound_refs, lower_expr};
use crate::{MatchArm, MatchEnumInfo, MatchInfo, VariableId};

/// Creates a bool variable with the given variant.
pub fn create_bool(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    variant: semantic::ConcreteVariant,
    location: StableLocationOption,
) -> VariableId {
    let semantic_db = ctx.db.upcast();

    let unit = StructConstruct { inputs: vec![], ty: corelib::unit_ty(semantic_db), location }
        .add(ctx, &mut builder.statements);

    generators::EnumConstruct { input: unit, variant, location }.add(ctx, &mut builder.statements)
}

// Lowers `lhs && rhs` to `if lhs { rhs } else { false }`.
pub fn lower_and_and(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    location: StableLocationOption,
    lhs: semantic::ExprId,
    rhs: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    let semantic_db = ctx.db.upcast();
    let unit_ty = corelib::unit_ty(semantic_db);
    let lhs_var = lower_expr(ctx, builder, lhs)?.var(ctx, builder)?;

    let mut subscope_lhs_true = create_subscope_with_bound_refs(ctx, builder);
    let lhs_true_block_id = subscope_lhs_true.block_id;

    let rhs_var = lower_expr(ctx, &mut subscope_lhs_true, rhs)?.var(ctx, &mut subscope_lhs_true)?;

    let sealed_block_lhs_true = subscope_lhs_true.goto_callsite(Some(rhs_var));
    let mut subscope_lhs_false = create_subscope_with_bound_refs(ctx, builder);
    let lhs_false_block_id = subscope_lhs_false.block_id;
    let false_var =
        create_bool(ctx, &mut subscope_lhs_false, corelib::false_variant(semantic_db), location);
    let sealed_block_lhs_false = subscope_lhs_false.goto_callsite(Some(false_var));

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(semantic_db),
        input: lhs_var,
        arms: vec![
            MatchArm {
                variant_id: corelib::false_variant(semantic_db),
                block_id: lhs_true_block_id,
                var_ids: vec![ctx.new_var(VarRequest { ty: unit_ty, location })],
            },
            MatchArm {
                variant_id: corelib::true_variant(semantic_db),
                block_id: lhs_false_block_id,
                var_ids: vec![ctx.new_var(VarRequest { ty: unit_ty, location })],
            },
        ],
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![sealed_block_lhs_true, sealed_block_lhs_false],
        location,
    )
}

// Lowers `lhs || rhs` to `if lhs { true } else { rhs }`.
pub fn lower_or_or(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    location: StableLocationOption,
    lhs: semantic::ExprId,
    rhs: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    let semantic_db = ctx.db.upcast();

    let unit_ty = corelib::unit_ty(semantic_db);
    let lhs_var = lower_expr(ctx, builder, lhs)?.var(ctx, builder)?;

    let mut subscope_lhs_true = create_subscope_with_bound_refs(ctx, builder);
    let lhs_true_block_id = subscope_lhs_true.block_id;

    let true_var =
        create_bool(ctx, &mut subscope_lhs_true, corelib::true_variant(semantic_db), location);

    let sealed_block_lhs_true = subscope_lhs_true.goto_callsite(Some(true_var));
    let mut subscope_lhs_false = create_subscope_with_bound_refs(ctx, builder);
    let lhs_false_block_id = subscope_lhs_false.block_id;
    let rhs_var =
        lower_expr(ctx, &mut subscope_lhs_false, rhs)?.var(ctx, &mut subscope_lhs_false)?;

    let sealed_block_lhs_false = subscope_lhs_false.goto_callsite(Some(rhs_var));

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(semantic_db),
        input: lhs_var,
        arms: vec![
            MatchArm {
                variant_id: corelib::false_variant(semantic_db),
                block_id: lhs_true_block_id,
                var_ids: vec![ctx.new_var(VarRequest { ty: unit_ty, location })],
            },
            MatchArm {
                variant_id: corelib::true_variant(semantic_db),
                block_id: lhs_false_block_id,
                var_ids: vec![ctx.new_var(VarRequest { ty: unit_ty, location })],
            },
        ],
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![sealed_block_lhs_true, sealed_block_lhs_false],
        location,
    )
}

/// Lowers an expression of type [semantic::ExprLogicalOperator].
pub fn lower_binary_op(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprLogicalOperator,
) -> LoweringResult<LoweredExpr> {
    let location = ctx.get_location(expr.stable_ptr.untyped());
    match expr.op {
        semantic::LogicalOperator::AndAnd => {
            lower_and_and(ctx, builder, location, expr.lhs, expr.rhs)
        }
        semantic::LogicalOperator::OrOr => lower_or_or(ctx, builder, location, expr.lhs, expr.rhs),
    }
}
