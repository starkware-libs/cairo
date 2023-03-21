use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib;
use cairo_lang_utils::extract_matches;
use num_traits::Zero;
use semantic::ExprFunctionCallArg;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult};
use super::scope::{BlockBuilder, SealedBlockBuilder};
use super::{lower_expr, lowered_expr_to_block_scope_end};
use crate::lower::context::VarRequest;
use crate::lower::{create_subscope_with_bound_refs, generators, lower_block};
use crate::{MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo};

#[allow(dead_code)]
enum IfCondition {
    BoolExpr(semantic::ExprId),
    Eq(semantic::ExprId, semantic::ExprId),
}

/// Analyzes the condition of an if statement into an [IfCondition] tree, to allow different
/// optimizations.
// TODO(lior): Make it an actual tree (handling && and ||).
fn analyze_condition(ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> IfCondition {
    let expr = &ctx.function_body.exprs[expr_id];
    if let semantic::Expr::FunctionCall(function_call) = expr {
        if function_call.function == corelib::felt252_eq(ctx.db.upcast())
            && function_call.args.len() == 2
        {
            return IfCondition::Eq(
                extract_matches!(function_call.args[0], ExprFunctionCallArg::Value),
                extract_matches!(function_call.args[1], ExprFunctionCallArg::Value),
            );
        };
    };

    IfCondition::BoolExpr(expr_id)
}

fn is_zero(ctx: &LoweringContext<'_>, expr_id: semantic::ExprId) -> bool {
    let expr = &ctx.function_body.exprs[expr_id];
    matches!(expr, semantic::Expr::Literal(literal) if literal.value.is_zero())
}

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    expr: &semantic::ExprIf,
) -> LoweringResult<LoweredExpr> {
    match analyze_condition(ctx, expr.condition) {
        IfCondition::BoolExpr(_) => lower_expr_if_bool(ctx, scope, expr),
        IfCondition::Eq(expr_a, expr_b) => lower_expr_if_eq(ctx, scope, expr, expr_a, expr_b),
    }
}

/// Lowers an expression of type [semantic::ExprIf], for the case of [IfCondition::BoolExpr].
pub fn lower_expr_if_bool(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    expr: &semantic::ExprIf,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a boolean if expression: {:?}", expr.debug(&ctx.expr_formatter));
    // The condition cannot be unit.
    let condition_var = lower_expr(ctx, scope, expr.condition)?.var(ctx, scope)?;
    let semantic_db = ctx.db.upcast();
    let unit_ty = corelib::unit_ty(semantic_db);
    let if_location = ctx.get_location(expr.stable_ptr.untyped());

    // Main block.
    let subscope_main = create_subscope_with_bound_refs(ctx, scope);
    let block_main_id = subscope_main.block_id;
    let main_block =
        extract_matches!(&ctx.function_body.exprs[expr.if_block], semantic::Expr::Block);
    let main_block_var_id = ctx.new_var(VarRequest {
        ty: unit_ty,
        location: ctx.get_location(main_block.stable_ptr.untyped()),
    });
    let block_main =
        lower_block(ctx, subscope_main, main_block).map_err(LoweringFlowError::Failed)?;

    // Else block.
    let subscope_else = create_subscope_with_bound_refs(ctx, scope);
    let block_else_id = subscope_else.block_id;

    let else_block_input_var_id = ctx.new_var(VarRequest { ty: unit_ty, location: if_location });
    let block_else = lower_optional_else_block(ctx, subscope_else, expr.else_block, if_location)
        .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(semantic_db),
        input: condition_var,
        arms: vec![
            MatchArm {
                variant_id: corelib::false_variant(semantic_db),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
            MatchArm {
                variant_id: corelib::true_variant(semantic_db),
                block_id: block_main_id,
                var_ids: vec![main_block_var_id],
            },
        ],
    });
    scope.merge_and_end_with_match(ctx, match_info, vec![block_main, block_else], if_location)
}

/// Lowers an expression of type [semantic::ExprIf], for the case of [IfCondition::Eq].
pub fn lower_expr_if_eq(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    expr: &semantic::ExprIf,
    expr_a: semantic::ExprId,
    expr_b: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    log::trace!(
        "Started lowering of an if-eq-zero expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let if_location = ctx.get_location(expr.stable_ptr.untyped());
    let condition_var = if is_zero(ctx, expr_b) {
        lower_expr(ctx, scope, expr_a)?.var(ctx, scope)?
    } else if is_zero(ctx, expr_a) {
        lower_expr(ctx, scope, expr_b)?.var(ctx, scope)?
    } else {
        let lowered_a = lower_expr(ctx, scope, expr_a)?.var(ctx, scope)?;
        let lowered_b = lower_expr(ctx, scope, expr_b)?.var(ctx, scope)?;
        let ret_ty = corelib::core_felt252_ty(ctx.db.upcast());
        let call_result = generators::Call {
            function: corelib::felt252_sub(ctx.db.upcast()),
            inputs: vec![lowered_a, lowered_b],
            ref_tys: vec![],
            ret_tys: vec![ret_ty],
            location: ctx
                .get_location(ctx.function_body.exprs[expr.condition].stable_ptr().untyped()),
        }
        .add(ctx, &mut scope.statements);
        call_result.returns.into_iter().next().unwrap()
    };

    let semantic_db = ctx.db.upcast();

    // Main block.
    let subscope_main = create_subscope_with_bound_refs(ctx, scope);
    let block_main_id = subscope_main.block_id;
    let block_main = lower_block(
        ctx,
        subscope_main,
        extract_matches!(&ctx.function_body.exprs[expr.if_block], semantic::Expr::Block),
    )
    .map_err(LoweringFlowError::Failed)?;

    // Else block.
    let non_zero_type =
        corelib::core_nonzero_ty(semantic_db, corelib::core_felt252_ty(semantic_db));
    let subscope_else = create_subscope_with_bound_refs(ctx, scope);
    let block_else_id = subscope_else.block_id;

    let else_block_input_var_id =
        ctx.new_var(VarRequest { ty: non_zero_type, location: if_location });
    let block_else = lower_optional_else_block(ctx, subscope_else, expr.else_block, if_location)
        .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(semantic_db),
        inputs: vec![condition_var],
        arms: vec![
            MatchArm {
                variant_id: corelib::jump_nz_zero_variant(semantic_db),
                block_id: block_main_id,
                var_ids: vec![],
            },
            MatchArm {
                variant_id: corelib::jump_nz_nonzero_variant(semantic_db),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
        ],
        location: if_location,
    });
    scope.merge_and_end_with_match(ctx, match_info, vec![block_main, block_else], if_location)
}

/// Lowers an optional else block. If the else block is missing it is replaced with a block
/// returning a unit.
/// Returns the sealed block builder of the else block.
fn lower_optional_else_block(
    ctx: &mut LoweringContext<'_>,
    mut scope: BlockBuilder,
    else_expr_opt: Option<semantic::ExprId>,
    if_location: StableLocationOption,
) -> Maybe<SealedBlockBuilder> {
    log::trace!("Started lowering of an optional else block.");
    match else_expr_opt {
        Some(else_expr) => {
            let expr = &ctx.function_body.exprs[else_expr];
            match expr {
                semantic::Expr::Block(block) => lower_block(ctx, scope, block),
                semantic::Expr::If(if_expr) => {
                    let lowered_if = lower_expr_if(ctx, &mut scope, if_expr);
                    lowered_expr_to_block_scope_end(ctx, scope, lowered_if)
                }
                _ => unreachable!(),
            }
        }
        None => lowered_expr_to_block_scope_end(
            ctx,
            scope,
            Ok(LoweredExpr::Tuple { exprs: vec![], location: if_location }),
        ),
    }
}
