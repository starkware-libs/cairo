use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::TypedStablePtr;
use semantic::MatchArmSelector;

use super::block_builder::BlockBuilder;
use super::context::{LoweredExpr, LoweringContext, LoweringResult, VarRequest};
use super::generators::{self, StructConstruct};
use super::{create_subscope, lower_expr_to_var_usage};
use crate::ids::LocationId;
use crate::{MatchArm, MatchEnumInfo, MatchInfo, VarUsage};

/// Creates a bool variable with the given variant.
pub fn create_bool(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    variant: semantic::ConcreteVariant,
    location: LocationId,
) -> VarUsage {
    let semantic_db = ctx.db.upcast();

    let unit = StructConstruct { inputs: vec![], ty: corelib::unit_ty(semantic_db), location }
        .add(ctx, &mut builder.statements);

    generators::EnumConstruct { input: unit, variant, location }.add(ctx, &mut builder.statements)
}

/// Lowers an expression of type [semantic::ExprLogicalOperator].
pub fn lower_logical_op(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprLogicalOperator,
) -> LoweringResult<LoweredExpr> {
    let location = ctx.get_location(expr.stable_ptr.untyped());

    let semantic_db = ctx.db.upcast();

    let unit_ty = corelib::unit_ty(semantic_db);
    let lhs = lower_expr_to_var_usage(ctx, builder, expr.lhs)?;

    let mut subscope_lhs_true = create_subscope(ctx, builder);
    let lhs_true_block_id = subscope_lhs_true.block_id;

    let (sealed_block_lhs_true, lhs_false_block_id, sealed_block_lhs_false) = match expr.op {
        // Lowers `lhs && rhs` to `if lhs { rhs } else { false }`.
        semantic::LogicalOperator::AndAnd => {
            let rhs_var = lower_expr_to_var_usage(ctx, &mut subscope_lhs_true, expr.rhs)?;

            let sealed_block_lhs_true = subscope_lhs_true.goto_callsite(Some(rhs_var));
            let mut subscope_lhs_false = create_subscope(ctx, builder);
            let lhs_false_block_id = subscope_lhs_false.block_id;
            let false_var = create_bool(
                ctx,
                &mut subscope_lhs_false,
                corelib::false_variant(semantic_db),
                location,
            );
            let sealed_block_lhs_false = subscope_lhs_false.goto_callsite(Some(false_var));
            (sealed_block_lhs_true, lhs_false_block_id, sealed_block_lhs_false)
        }

        // Lowers `lhs || rhs` to `if lhs { true } else { rhs }`.
        semantic::LogicalOperator::OrOr => {
            let true_var = create_bool(
                ctx,
                &mut subscope_lhs_true,
                corelib::true_variant(semantic_db),
                location,
            );
            let sealed_block_lhs_true = subscope_lhs_true.goto_callsite(Some(true_var));
            let mut subscope_lhs_false = create_subscope(ctx, builder);
            let lhs_false_block_id = subscope_lhs_false.block_id;
            let rhs_var = lower_expr_to_var_usage(ctx, &mut subscope_lhs_false, expr.rhs)?;

            let sealed_block_lhs_false = subscope_lhs_false.goto_callsite(Some(rhs_var));
            (sealed_block_lhs_true, lhs_false_block_id, sealed_block_lhs_false)
        }
    };

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(semantic_db),
        input: lhs,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::false_variant(semantic_db)),
                block_id: lhs_false_block_id,
                var_ids: vec![ctx.new_var(VarRequest { ty: unit_ty, location })],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::true_variant(semantic_db)),
                block_id: lhs_true_block_id,
                var_ids: vec![ctx.new_var(VarRequest { ty: unit_ty, location })],
            },
        ],
        location,
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![sealed_block_lhs_false, sealed_block_lhs_true],
        location,
    )
}
