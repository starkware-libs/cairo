use semantic::corelib;
use utils::extract_matches;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError};
use super::scope::{generators, BlockFlowMerger, BlockScope};
use super::{lower_block, lower_expr, lowered_expr_from_block_result};

#[allow(dead_code)]
enum IfCondition {
    BoolExpr(semantic::ExprId),
    EqZero(semantic::ExprId),
}

/// Analyzes the condition of an if statement into an [IfCondition] tree, to allow different
/// optimizations.
fn analyze_condition(ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> IfCondition {
    let expr = &ctx.function_def.exprs[expr_id];
    if let semantic::Expr::FunctionCall(function_call) = expr {
        if function_call.function == corelib::felt_eq(ctx.db)
            && function_call.args.len() == 2
            && is_zero(ctx, function_call.args[1])
        {
            return IfCondition::EqZero(function_call.args[0]);
        };
    };

    IfCondition::BoolExpr(expr_id)
}

fn is_zero(ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> bool {
    let expr = &ctx.function_def.exprs[expr_id];
    if let semantic::Expr::Literal(literal) = expr {
        if literal.value == 0 {
            return true;
        }
    }
    false
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
) -> Result<LoweredExpr, LoweringFlowError> {
    match analyze_condition(ctx, expr.condition) {
        IfCondition::BoolExpr(_) => lower_expr_if_bool(ctx, scope, expr),
        IfCondition::EqZero(tested_expr) => lower_expr_if_eq_zero(ctx, scope, expr, tested_expr),
    }
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if_bool(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
) -> Result<LoweredExpr, LoweringFlowError> {
    // The condition cannot be unit.
    let condition_var = lower_expr(ctx, scope, expr.condition)?.var(ctx, scope);

    // Lower both blocks.
    let unit_ty = corelib::unit_ty(ctx.db);
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
        let [main_block_scope, else_block_scope] =
            [expr.if_block, expr.else_block].map(|block_expr| {
                merger.run_in_subscope(ctx, vec![unit_ty], |ctx, subscope, _| {
                    lower_block(
                        ctx,
                        subscope,
                        extract_matches!(
                            &ctx.function_def.exprs[block_expr],
                            semantic::Expr::Block
                        ),
                    )
                })
            });
        Some((main_block_scope, else_block_scope))
    });

    let (main_block_sealed, else_block_sealed) = res.ok_or(LoweringFlowError::Failed)?;
    let main_finalized =
        finalized_merger.finalize_block(ctx, main_block_sealed.ok_or(LoweringFlowError::Failed)?);
    let else_finalized =
        finalized_merger.finalize_block(ctx, else_block_sealed.ok_or(LoweringFlowError::Failed)?);

    // Emit the statement.
    let match_generator = generators::MatchEnum {
        input: condition_var,
        concrete_enum_id: corelib::core_bool_enum(ctx.db),
        arms: vec![
            (corelib::true_variant(ctx.db), main_finalized.block),
            (corelib::false_variant(ctx.db), else_finalized.block),
        ],
        end_info: finalized_merger.end_info,
    };
    let block_result = match_generator.add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if_eq_zero(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
    tested_expr: semantic::ExprId,
) -> Result<LoweredExpr, LoweringFlowError> {
    // The condition cannot be unit.
    let condition_var = lower_expr(ctx, scope, tested_expr)?.var(ctx, scope);

    // Lower both blocks.
    let unit_ty = corelib::unit_ty(ctx.db);
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
        let [main_block_scope, else_block_scope] =
            [expr.if_block, expr.else_block].map(|block_expr| {
                merger.run_in_subscope(ctx, vec![unit_ty], |ctx, subscope, _| {
                    lower_block(
                        ctx,
                        subscope,
                        extract_matches!(
                            &ctx.function_def.exprs[block_expr],
                            semantic::Expr::Block
                        ),
                    )
                })
            });
        Some((main_block_scope, else_block_scope))
    });

    let (main_block_sealed, else_block_sealed) = res.ok_or(LoweringFlowError::Failed)?;
    let main_finalized =
        finalized_merger.finalize_block(ctx, main_block_sealed.ok_or(LoweringFlowError::Failed)?);
    let else_finalized =
        finalized_merger.finalize_block(ctx, else_block_sealed.ok_or(LoweringFlowError::Failed)?);

    // Emit the statement.
    let match_generator = generators::MatchExtern {
        function: corelib::core_jump_nz_func(ctx.db),
        inputs: vec![condition_var],
        arms: vec![main_finalized.block, else_finalized.block],
        end_info: finalized_merger.end_info,
    };
    let block_result = match_generator.add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}
