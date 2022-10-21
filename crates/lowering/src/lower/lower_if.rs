use semantic::corelib;
use utils::extract_matches;

use crate::BlockId;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError};
use super::scope::{generators, BlockFlowMerger, BlockMergerFinalized, BlockScope};
use super::{lower_block, lower_expr, lowered_expr_from_block_result};

#[allow(dead_code)]
enum IfCondition {
    BoolExpr(semantic::ExprId),
}

/// Analyzes the condition of an if statement into an [IfCondition] tree, to allow different
/// optimizations.
fn analyze_condition(_ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> IfCondition {
    IfCondition::BoolExpr(expr_id)
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
) -> Result<LoweredExpr, LoweringFlowError> {
    // Lower both blocks.
    let unit_ty = corelib::unit_ty(ctx.db);
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, |ctx, merger| {
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

    match analyze_condition(ctx, expr.condition) {
        IfCondition::BoolExpr(condition) => lower_expr_if_bool(
            ctx,
            scope,
            condition,
            main_finalized.block,
            else_finalized.block,
            finalized_merger,
        ),
    }
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if_bool(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    condition: semantic::ExprId,
    main_finalized: BlockId,
    else_finalized: BlockId,
    finalized_merger: BlockMergerFinalized,
) -> Result<LoweredExpr, LoweringFlowError> {
    // The condition cannot be unit.
    let condition_var =
        extract_matches!(lower_expr(ctx, scope, condition)?, LoweredExpr::AtVariable);

    // Emit the statement.
    let match_generator = generators::MatchEnum {
        input: condition_var,
        concrete_enum_id: corelib::core_bool_enum(ctx.db),
        arms: vec![
            (corelib::true_variant(ctx.db), main_finalized),
            (corelib::false_variant(ctx.db), else_finalized),
        ],
        end_info: finalized_merger.end_info,
    };
    let block_result = match_generator.add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}
