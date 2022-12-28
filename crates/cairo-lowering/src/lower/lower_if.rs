use debug::DebugWithDb;
use diagnostics::Maybe;
use num_traits::Zero;
use semantic::corelib;
use utils::extract_matches;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError};
use super::scope::{generators, BlockFlowMerger, BlockScope, BlockScopeEnd};
use super::{
    lower_block, lower_expr, lowered_expr_from_block_result, lowered_expr_to_block_scope_end,
};

#[allow(dead_code)]
enum IfCondition {
    BoolExpr(semantic::ExprId),
    Eq(semantic::ExprId, semantic::ExprId),
}

/// Analyzes the condition of an if statement into an [IfCondition] tree, to allow different
/// optimizations.
// TODO(lior): Make it an actual tree (handling && and ||).
fn analyze_condition(ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> IfCondition {
    let expr = &ctx.function_def.exprs[expr_id];
    if let semantic::Expr::FunctionCall(function_call) = expr {
        if function_call.function == corelib::felt_eq(ctx.db.upcast())
            && function_call.args.len() == 2
        {
            return IfCondition::Eq(function_call.args[0], function_call.args[1]);
        };
    };

    IfCondition::BoolExpr(expr_id)
}

fn is_zero(ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> bool {
    let expr = &ctx.function_def.exprs[expr_id];
    matches!(expr, semantic::Expr::Literal(literal) if literal.value.is_zero())
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
) -> Result<LoweredExpr, LoweringFlowError> {
    match analyze_condition(ctx, expr.condition) {
        IfCondition::BoolExpr(_) => lower_expr_if_bool(ctx, scope, expr),
        IfCondition::Eq(expr_a, expr_b) => lower_expr_if_eq(ctx, scope, expr, expr_a, expr_b),
    }
}

/// Lowers an expression of type [semantic::ExprIf], for the case of [IfCondition::BoolExpr].
pub fn lower_expr_if_bool(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a boolean if expression: {:?}", expr.debug(&ctx.expr_formatter));
    // The condition cannot be unit.
    let condition_var = lower_expr(ctx, scope, expr.condition)?.var(ctx, scope)?;

    let semantic_db = ctx.db.upcast();

    // Lower both blocks.
    let unit_ty = corelib::unit_ty(semantic_db);
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
        let main_block_scope = merger.run_in_subscope(ctx, vec![unit_ty], |ctx, subscope, _| {
            lower_block(
                ctx,
                subscope,
                extract_matches!(&ctx.function_def.exprs[expr.if_block], semantic::Expr::Block),
                false,
            )
        });
        let else_block_scope = merger.run_in_subscope(ctx, vec![unit_ty], |ctx, subscope, _| {
            lower_optional_else_block(ctx, subscope, expr.else_block)
        });
        Ok((main_block_scope, else_block_scope))
    });

    let (main_block_sealed, else_block_sealed) = res.map_err(LoweringFlowError::Failed)?;
    let main_finalized =
        finalized_merger.finalize_block(ctx, main_block_sealed.map_err(LoweringFlowError::Failed)?);
    let else_finalized =
        finalized_merger.finalize_block(ctx, else_block_sealed.map_err(LoweringFlowError::Failed)?);

    // Emit the statement.
    let block_result = (generators::MatchEnum {
        input: condition_var,
        concrete_enum_id: corelib::core_bool_enum(semantic_db),
        arms: vec![
            (corelib::false_variant(semantic_db), else_finalized.block),
            (corelib::true_variant(semantic_db), main_finalized.block),
        ],
        end_info: finalized_merger.end_info.clone(),
    })
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
}

/// Lowers an expression of type [semantic::ExprIf], for the case of [IfCondition::Eq].
pub fn lower_expr_if_eq(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprIf,
    expr_a: semantic::ExprId,
    expr_b: semantic::ExprId,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!(
        "Started lowering of an if-eq-zero expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let condition_var = if is_zero(ctx, expr_b) {
        lower_expr(ctx, scope, expr_a)?.var(ctx, scope)?
    } else if is_zero(ctx, expr_a) {
        lower_expr(ctx, scope, expr_b)?.var(ctx, scope)?
    } else {
        let lowered_a = lower_expr(ctx, scope, expr_a)?.var(ctx, scope)?;
        let lowered_b = lower_expr(ctx, scope, expr_b)?.var(ctx, scope)?;
        let ret_ty = corelib::core_felt_ty(ctx.db.upcast());
        let call_result = generators::Call {
            function: corelib::felt_sub(ctx.db.upcast()),
            inputs: vec![lowered_a, lowered_b],
            ref_tys: vec![],
            ret_tys: vec![ret_ty],
        }
        .add(ctx, scope);
        call_result.returns.into_iter().next().unwrap()
    };

    let semantic_db = ctx.db.upcast();

    // Lower both blocks.
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
        let main_block_scope = merger.run_in_subscope(ctx, vec![], |ctx, subscope, _| {
            lower_block(
                ctx,
                subscope,
                extract_matches!(&ctx.function_def.exprs[expr.if_block], semantic::Expr::Block),
                false,
            )
        });
        let non_zero_type =
            corelib::core_nonzero_ty(semantic_db, corelib::core_felt_ty(semantic_db));
        let else_block_scope =
            merger.run_in_subscope(ctx, vec![non_zero_type], |ctx, subscope, _| {
                lower_optional_else_block(ctx, subscope, expr.else_block)
            });
        Ok((main_block_scope, else_block_scope))
    });

    let (main_block_sealed, else_block_sealed) = res.map_err(LoweringFlowError::Failed)?;
    let main_finalized =
        finalized_merger.finalize_block(ctx, main_block_sealed.map_err(LoweringFlowError::Failed)?);
    let else_finalized =
        finalized_merger.finalize_block(ctx, else_block_sealed.map_err(LoweringFlowError::Failed)?);

    // Emit the statement.
    let block_result = (generators::MatchExtern {
        function: corelib::core_jump_nz_func(semantic_db),
        inputs: vec![condition_var],
        arms: vec![
            (corelib::jump_nz_zero_variant(ctx.db.upcast()), main_finalized.block),
            (corelib::jump_nz_nonzero_variant(ctx.db.upcast()), else_finalized.block),
        ],
        end_info: finalized_merger.end_info.clone(),
    })
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
}

/// Lowers an optional else block. If the else block is missing it is replaced with a block
/// returning a unit.
fn lower_optional_else_block(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    else_expr_opt: Option<semantic::ExprId>,
) -> Maybe<BlockScopeEnd> {
    log::trace!("Started lowering of an optional else block.");
    match else_expr_opt {
        Some(else_expr) => match &ctx.function_def.exprs[else_expr] {
            semantic::Expr::Block(block) => lower_block(ctx, scope, block, false),
            semantic::Expr::If(if_expr) => {
                let lowered_if = lower_expr_if(ctx, scope, if_expr);
                lowered_expr_to_block_scope_end(ctx, scope, lowered_if, false)
            }
            _ => unreachable!(),
        },
        None => lowered_expr_to_block_scope_end(ctx, scope, Ok(LoweredExpr::Tuple(vec![])), false),
    }
}
