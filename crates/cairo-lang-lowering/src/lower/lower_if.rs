use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::extract_matches;
use semantic::{Condition, MatchArmSelector};

use super::block_builder::{BlockBuilder, SealedBlockBuilder};
use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult};
use super::lowered_expr_to_block_scope_end;
use crate::diagnostic::LoweringDiagnosticKind::{self};
use crate::diagnostic::{LoweringDiagnosticsBuilder, MatchDiagnostic, MatchError, MatchKind};
use crate::ids::LocationId;
use crate::lower::context::VarRequest;
use crate::lower::lower_match::{self, MatchArmWrapper};
use crate::lower::{create_subscope, lower_block, lower_expr, lower_expr_to_var_usage};
use crate::{MatchArm, MatchEnumInfo, MatchInfo};

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprIf,
) -> LoweringResult<LoweredExpr> {
    match &expr.condition {
        Condition::BoolExpr(condition) => lower_expr_if_bool(ctx, builder, expr, *condition),
        Condition::Let(matched_expr, patterns) => {
            lower_expr_if_let(ctx, builder, expr, *matched_expr, patterns)
        }
    }
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if_bool(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprIf,
    condition: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a boolean if expression: {:?}", expr.debug(&ctx.expr_formatter));

    // The condition cannot be unit.
    let condition = lower_expr_to_var_usage(ctx, builder, condition)?;
    let db = ctx.db;
    let unit_ty = corelib::unit_ty(db);
    let if_location = ctx.get_location(expr.stable_ptr.untyped());

    // Main block.
    let subscope_main = create_subscope(ctx, builder);
    let block_main_id = subscope_main.block_id;
    let main_block =
        extract_matches!(&ctx.function_body.arenas.exprs[expr.if_block], semantic::Expr::Block)
            .clone();
    let main_block_var_id = ctx.new_var(VarRequest {
        ty: unit_ty,
        location: ctx.get_location(main_block.stable_ptr.untyped()),
    });
    let block_main =
        lower_block(ctx, subscope_main, &main_block).map_err(LoweringFlowError::Failed)?;

    // Else block.
    let subscope_else = create_subscope(ctx, builder);
    let block_else_id = subscope_else.block_id;

    let else_block_input_var_id = ctx.new_var(VarRequest { ty: unit_ty, location: if_location });
    let block_else = lower_optional_else_block(ctx, subscope_else, expr.else_block, if_location)
        .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(db),
        input: condition,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::false_variant(db)),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::true_variant(db)),
                block_id: block_main_id,
                var_ids: vec![main_block_var_id],
            },
        ],
        location: if_location,
    });
    builder.merge_and_end_with_match(ctx, match_info, vec![block_main, block_else], if_location)
}

/// Lowers an expression of type if where the condition is of type [semantic::Condition::Let].
pub fn lower_expr_if_let(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprIf,
    matched_expr: semantic::ExprId,
    patterns: &[semantic::PatternId],
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering an if let expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, builder, matched_expr)?;
    let ty = ctx.function_body.arenas.exprs[matched_expr].ty();

    if corelib::numeric_upcastable_to_felt252(ctx.db, ty) {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            expr.stable_ptr.untyped(),
            LoweringDiagnosticKind::MatchError(MatchError {
                kind: MatchKind::IfLet,
                error: MatchDiagnostic::UnsupportedNumericInLetCondition,
            }),
        )));
    }

    let arms = vec![
        MatchArmWrapper::Arm(patterns, expr.if_block),
        expr.else_block.map(MatchArmWrapper::ElseClause).unwrap_or(MatchArmWrapper::DefaultClause),
    ];

    lower_match::lower_match_arms(
        ctx,
        builder,
        matched_expr,
        lowered_expr,
        arms,
        location,
        MatchKind::IfLet,
    )
}
/// Lowers an optional else block. If the else block is missing it is replaced with a block
/// returning a unit.
/// Returns the sealed block builder of the else block.
fn lower_optional_else_block(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    else_expr_opt: Option<semantic::ExprId>,
    if_location: LocationId,
) -> Maybe<SealedBlockBuilder> {
    log::trace!("Started lowering of an optional else block.");
    match else_expr_opt {
        Some(else_expr) => {
            let expr = ctx.function_body.arenas.exprs[else_expr].clone();
            match &expr {
                semantic::Expr::Block(block) => lower_block(ctx, builder, block),
                semantic::Expr::If(if_expr) => {
                    let lowered_if = lower_expr_if(ctx, &mut builder, if_expr);
                    lowered_expr_to_block_scope_end(ctx, builder, lowered_if)
                }
                _ => unreachable!(),
            }
        }
        None => lowered_expr_to_block_scope_end(
            ctx,
            builder,
            Ok(LoweredExpr::Tuple { exprs: vec![], location: if_location }),
        ),
    }
}
