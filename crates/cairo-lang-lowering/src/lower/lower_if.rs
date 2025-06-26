use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::TypedStablePtr;
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

/// Represents an expression of the form:
///
///   `if conditions[0] && conditions[1] && ... && conditions[n] { expr } else { else_block }`
///
/// where `n` is `conditions.len() - 1`.
///
/// In particular, note that if `conditions` is empty, there are no conditions and the
/// expression is simply [Self::expr].
pub struct ConditionedExpr<'a> {
    pub expr: semantic::ExprId,
    pub conditions: &'a [Condition],
    pub else_block: Option<semantic::ExprId>,
}

impl ConditionedExpr<'_> {
    /// Returns a copy of self, without the first condition.
    pub fn remove_first(&self) -> Self {
        Self { conditions: &self.conditions[1..], ..*self }
    }
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprIf,
) -> LoweringResult<LoweredExpr> {
    // Else block is not supported yet for multiple conditions.
    if expr.conditions.len() > 1 {
        if let Some(else_block) = expr.else_block {
            let stable_ptr = ctx.function_body.arenas.exprs[else_block].stable_ptr().untyped();
            return Err(LoweringFlowError::Failed(
                ctx.diagnostics.report(stable_ptr, LoweringDiagnosticKind::Unsupported),
            ));
        }
    }
    lower_conditioned_expr(
        ctx,
        builder,
        &ConditionedExpr {
            expr: expr.if_block,
            conditions: &expr.conditions,
            else_block: expr.else_block,
        },
    )
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_if_bool_condition(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    condition: semantic::ExprId,
    inner_expr: ConditionedExpr<'_>,
) -> LoweringResult<LoweredExpr> {
    // The condition cannot be unit.
    let condition_var = lower_expr_to_var_usage(ctx, builder, condition)?;
    let db = ctx.db;
    let unit_ty = corelib::unit_ty(db);

    let condition_expr = &ctx.function_body.arenas.exprs[condition];
    let stable_ptr = condition_expr.stable_ptr().untyped();
    let condition_location = ctx.get_location(stable_ptr);

    // Main block.
    let subscope_main = create_subscope(ctx, builder);
    let block_main_id = subscope_main.block_id;
    let main_block_var_id = ctx.new_var(VarRequest { ty: unit_ty, location: condition_location });

    let block_main = lower_conditioned_expr_and_seal(ctx, subscope_main, &inner_expr)
        .map_err(LoweringFlowError::Failed)?;

    // Else block.
    let subscope_else = create_subscope(ctx, builder);
    let block_else_id = subscope_else.block_id;

    let else_block_input_var_id =
        ctx.new_var(VarRequest { ty: unit_ty, location: condition_location });
    let block_else =
        lower_optional_else_block(ctx, subscope_else, inner_expr.else_block, condition_location)
            .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(db),
        input: condition_var,
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
        location: condition_location,
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![block_main, block_else],
        condition_location,
    )
}

/// Lowers an expression of type if where the condition is of type [semantic::Condition::Let].
pub fn lower_if_let_condition(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    matched_expr_id: semantic::ExprId,
    patterns: &[semantic::PatternId],
    inner_expr: ConditionedExpr<'_>,
) -> LoweringResult<LoweredExpr> {
    let matched_expr = &ctx.function_body.arenas.exprs[matched_expr_id];
    let stable_ptr = matched_expr.stable_ptr().untyped();
    let ty = matched_expr.ty();
    let location = ctx.get_location(stable_ptr);

    let lowered_expr = lower_expr(ctx, builder, matched_expr_id)?;

    if corelib::numeric_upcastable_to_felt252(ctx.db, ty) {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            stable_ptr,
            LoweringDiagnosticKind::MatchError(MatchError {
                kind: MatchKind::IfLet,
                error: MatchDiagnostic::UnsupportedNumericInLetCondition,
            }),
        )));
    }

    let else_arm = inner_expr
        .else_block
        .map(MatchArmWrapper::ElseClause)
        .unwrap_or(MatchArmWrapper::DefaultClause);
    let arms = vec![MatchArmWrapper::ConditionedArm(patterns, inner_expr), else_arm];

    lower_match::lower_match_arms(
        ctx,
        builder,
        matched_expr_id,
        lowered_expr,
        arms,
        location,
        MatchKind::IfLet,
    )
}

/// Lowers a [ConditionedExpr] recursively by iterating over the conditions and calling
/// [lower_if_let_condition] or [lower_if_bool_condition].
fn lower_conditioned_expr(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &ConditionedExpr<'_>,
) -> LoweringResult<LoweredExpr> {
    log::trace!(
        "Lowering a conditioned expression: {:?} (# of conditions: {})",
        expr.expr.debug(&ctx.expr_formatter),
        expr.conditions.len()
    );

    // If there are no more conditions, we can simply lower the expression.
    if expr.conditions.is_empty() {
        return lower_expr(ctx, builder, expr.expr);
    }

    match &expr.conditions[0] {
        Condition::Let(matched_expr_id, patterns) => {
            lower_if_let_condition(ctx, builder, *matched_expr_id, patterns, expr.remove_first())
        }
        Condition::BoolExpr(condition) => {
            lower_if_bool_condition(ctx, builder, *condition, expr.remove_first())
        }
    }
}

/// Lowers a [ConditionedExpr] and seals the block. See [lower_conditioned_expr].
pub fn lower_conditioned_expr_and_seal(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    expr: &ConditionedExpr<'_>,
) -> Maybe<SealedBlockBuilder> {
    let lowered_expr = lower_conditioned_expr(ctx, &mut builder, expr);
    lowered_expr_to_block_scope_end(ctx, builder, lowered_expr)
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
