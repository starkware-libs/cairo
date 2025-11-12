use std::vec;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_semantic::corelib::{
    CorelibSemantic, ErrorPropagationType, bounded_int_ty, get_enum_concrete_variant,
    try_get_ty_by_name, unwrap_error_propagation_type, validate_literal,
};
use cairo_lang_semantic::items::constant::ConstValueId;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::items::functions::{
    FunctionsSemantic, GenericFunctionId, ImplGenericFunctionId,
};
use cairo_lang_semantic::items::imp::ImplLongId;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::items::trt::TraitSemantic;
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{
    ConcreteFunction, ConcreteTraitLongId, ExprVar, LocalVariable, VarId, corelib,
};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::{Entry, UnorderedHashMap};
use cairo_lang_utils::{Intern, extract_matches, try_extract_matches};
use context::handle_lowering_flow_error;
use defs::ids::TopLevelLanguageElementId;
use flow_control::create_graph::{
    create_graph_expr_if, create_graph_expr_match, create_graph_expr_while_let,
};
use flow_control::lower_graph::lower_graph;
use itertools::{Itertools, chain, izip, zip_eq};
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use refs::ClosureInfo;
use salsa::Database;
use semantic::corelib::{
    core_submodule, get_core_function_id, get_core_ty_by_name, get_function_id, never_ty, unit_ty,
};
use semantic::items::constant::ConstValue;
use semantic::types::{peel_snapshots, wrap_in_snapshots};
use semantic::{
    ExprFunctionCallArg, ExprId, ExprPropagateError, ExprVarMemberPath, GenericArgumentId,
    MatchArmSelector, SemanticDiagnostic, TypeLongId,
};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use self::block_builder::{BlockBuilder, SealedBlockBuilder, SealedGotoCallsite};
use self::context::{
    EncapsulatingLoweringContext, LoweredExpr, LoweredExprExternEnum, LoweringContext,
    LoweringFlowError,
};
use self::external::{extern_facade_expr, extern_facade_return_tys};
use self::logical_op::lower_logical_op;
use crate::blocks::Blocks;
use crate::diagnostic::LoweringDiagnosticKind::{self, *};
use crate::diagnostic::LoweringDiagnosticsBuilder;
use crate::ids::{
    EnrichedSemanticSignature, FunctionLongId, FunctionWithBodyId, FunctionWithBodyLongId,
    GeneratedFunction, GeneratedFunctionKey, LocationId, SemanticFunctionIdEx,
    parameter_as_member_path,
};
use crate::lower::context::{LoopContext, LoopEarlyReturnInfo, LoweringResult, VarRequest};
use crate::lower::generators::StructDestructure;
use crate::{
    BlockId, Lowered, MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo, VarUsage, VariableId,
};

mod block_builder;
pub mod context;
mod external;
mod flow_control;
pub mod generators;
mod logical_op;
mod lower_let_else;
pub mod refs;

#[cfg(test)]
mod test_utils;

#[cfg(test)]
mod block_builder_test;

#[cfg(test)]
mod generated_test;

#[cfg(test)]
mod specialized_test;

/// Lowering of a function together with extra generated functions.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct MultiLowering<'db> {
    pub main_lowering: Lowered<'db>,
    pub generated_lowerings: OrderedHashMap<GeneratedFunctionKey<'db>, Lowered<'db>>,
}

/// Lowers a semantic free function.
pub fn lower_semantic_function<'db>(
    db: &'db dyn Database,
    semantic_function_id: defs::ids::FunctionWithBodyId<'db>,
) -> Maybe<MultiLowering<'db>> {
    let declaration_diagnostics = db.function_declaration_diagnostics(semantic_function_id);
    check_error_free_or_warn(db, declaration_diagnostics, semantic_function_id, "declaration")?;
    let body_diagnostics = db.function_body_diagnostics(semantic_function_id);
    check_error_free_or_warn(db, body_diagnostics, semantic_function_id, "body")?;

    let mut encapsulating_ctx = EncapsulatingLoweringContext::new(db, semantic_function_id)?;
    let function_id = FunctionWithBodyLongId::Semantic(semantic_function_id).intern(db);
    let signature = db.function_with_body_signature(semantic_function_id)?;

    // TODO(spapini): Build semantic_defs in semantic model.
    for semantic_var in &signature.params {
        encapsulating_ctx.semantic_defs.insert(
            semantic::VarId::Param(semantic_var.id),
            semantic::Binding::Param(semantic_var.clone()),
        );
    }

    let block_expr_id = encapsulating_ctx.function_body.body_expr;
    let main_lowering = lower_function(
        &mut encapsulating_ctx,
        function_id,
        EnrichedSemanticSignature::from_semantic(db, signature),
        block_expr_id,
    )?;
    Ok(MultiLowering { main_lowering, generated_lowerings: encapsulating_ctx.lowerings })
}

/// Lowers a function into [Lowered].
pub fn lower_function<'db>(
    encapsulating_ctx: &mut EncapsulatingLoweringContext<'db>,
    function_id: FunctionWithBodyId<'db>,
    signature: EnrichedSemanticSignature<'db>,
    block_expr_id: semantic::ExprId,
) -> Maybe<Lowered<'db>> {
    log::trace!("Lowering a free function.");
    let return_type = signature.return_type;
    let mut ctx = LoweringContext::new(encapsulating_ctx, function_id, signature, return_type)?;

    // Fetch body block expr.
    let semantic_block =
        extract_matches!(&ctx.function_body.arenas.exprs[block_expr_id], semantic::Expr::Block)
            .clone();

    // Initialize builder.
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(root_block_id);

    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            // TODO(spapini): Introduce member paths, not just base variables.
            let param_var = extract_matches!(param, ExprVarMemberPath::Var);
            builder.put_semantic(param_var.var, var);
            var
        })
        .collect_vec();

    let root_ok = {
        let maybe_sealed_block = lower_block(&mut ctx, builder, &semantic_block);
        maybe_sealed_block.and_then(|block_sealed| {
            wrap_sealed_block_as_function(
                &mut ctx,
                block_sealed,
                semantic_block.stable_ptr.untyped(),
            )?;
            Ok(root_block_id)
        })
    };
    let blocks = root_ok
        .map(|_| ctx.blocks.build().expect("Root block must exist."))
        .unwrap_or_else(Blocks::new_errored);
    Ok(Lowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.into(),
        parameters,
    })
}

/// Lowers an expression of type [semantic::ExprFor].
pub fn lower_for_loop<'db, 'mt>(
    ctx: &mut LoweringContext<'db, 'mt>,
    builder: &mut BlockBuilder<'db>,
    loop_expr: semantic::ExprFor<'db>,
    loop_expr_id: semantic::ExprId,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let db = ctx.db;
    let for_location = ctx.get_location(loop_expr.stable_ptr.untyped());
    let next_semantic_signature =
        db.concrete_function_signature(loop_expr.next_function_id).unwrap();
    let into_iter = builder.get_ref(ctx, &loop_expr.into_iter_member_path).unwrap();
    let next_call = generators::Call {
        function: loop_expr.next_function_id.lowered(db),
        inputs: vec![into_iter],
        coupon_input: None,
        extra_ret_tys: vec![next_semantic_signature.params.first().unwrap().ty],
        ret_tys: vec![next_semantic_signature.return_type],
        location: for_location,
    }
    .add(ctx, &mut builder.statements);
    let next_iterator = next_call.extra_outputs.first().unwrap();
    let next_value = next_call.returns.first().unwrap();
    let ErrorPropagationType::Option { some_variant, none_variant } =
        unwrap_error_propagation_type(db, ctx.variables[next_value.var_id].ty)
            .expect("Expected Option type for next function return.")
    else {
        unreachable!("Return type for next function must be Option.")
    };
    let next_value_type = some_variant.ty;
    builder.update_ref(ctx, &loop_expr.into_iter_member_path, next_iterator.var_id);
    let unit_ty = corelib::unit_ty(db);
    let some_block: cairo_lang_semantic::ExprBlock<'_> =
        extract_matches!(&ctx.function_body.arenas.exprs[loop_expr.body], semantic::Expr::Block)
            .clone();
    let mut some_subscope = create_subscope(ctx, builder);
    let some_subscope_block_id = some_subscope.block_id;
    let some_var_id = ctx.new_var(VarRequest {
        ty: next_value_type,
        location: ctx.get_location(some_block.stable_ptr.untyped()),
    });
    let variant_expr = LoweredExpr::AtVariable(VarUsage {
        var_id: some_var_id,
        location: ctx.get_location(some_block.stable_ptr.untyped()),
    });
    let lowered_pattern =
        lower_single_pattern(ctx, &mut some_subscope, loop_expr.pattern, variant_expr);
    let sealed_some = match lowered_pattern {
        Ok(_) => {
            let block_expr = (|| {
                lower_expr_block(ctx, &mut some_subscope, &some_block)?;
                recursively_call_loop_func(
                    ctx,
                    &mut some_subscope,
                    loop_expr_id,
                    loop_expr.stable_ptr.untyped(),
                )
            })();
            lowered_expr_to_block_scope_end(ctx, some_subscope, block_expr)
        }
        Err(err) => handle_lowering_flow_error(ctx, some_subscope.clone(), err).map(|_| None),
    }
    .map_err(LoweringFlowError::Failed)?;

    let none_subscope = create_subscope(ctx, builder);
    let none_var_id = ctx.new_var(VarRequest {
        ty: unit_ty,
        location: ctx.get_location(some_block.stable_ptr.untyped()),
    });
    let none_subscope_block_id = none_subscope.block_id;
    let sealed_none = return_a_unit(ctx, none_subscope, for_location, false)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: some_variant.concrete_enum_id,
        input: *next_value,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(some_variant),
                block_id: some_subscope_block_id,
                var_ids: vec![some_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(none_variant),
                block_id: none_subscope_block_id,
                var_ids: vec![none_var_id],
            },
        ],
        location: for_location,
    });
    builder.merge_and_end_with_match(ctx, match_info, vec![sealed_some, sealed_none], for_location)
}

/// Lowers an expression of type [semantic::ExprWhile].
pub fn lower_while_loop<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    loop_expr: semantic::ExprWhile<'db>,
    loop_expr_id: semantic::ExprId,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let while_location = ctx.get_location(loop_expr.stable_ptr.untyped());
    let semantic_condition = match &loop_expr.condition {
        semantic::Condition::BoolExpr(semantic_condition) => *semantic_condition,
        semantic::Condition::Let(match_expr, patterns) => {
            return (|| {
                let ret_var = lower_expr_while_let(
                    ctx,
                    builder,
                    &loop_expr,
                    *match_expr,
                    patterns,
                    loop_expr_id,
                    loop_expr.stable_ptr.untyped(),
                )?
                .as_var_usage(ctx, builder)?;

                lower_return(ctx, builder, ret_var, while_location, false)
            })();
        }
    };
    let condition = lower_expr_to_var_usage(ctx, builder, semantic_condition)?;
    let db = ctx.db;
    let unit_ty = corelib::unit_ty(db);

    // Main block.
    let mut subscope_main = create_subscope(ctx, builder);
    let block_main_id = subscope_main.block_id;
    let main_block =
        extract_matches!(&ctx.function_body.arenas.exprs[loop_expr.body], semantic::Expr::Block)
            .clone();
    let main_block_var_id = ctx.new_var(VarRequest {
        ty: unit_ty,
        location: ctx.get_location(main_block.stable_ptr.untyped()),
    });

    let block_expr = (|| {
        lower_expr_block(ctx, &mut subscope_main, &main_block)?;
        recursively_call_loop_func(
            ctx,
            &mut subscope_main,
            loop_expr_id,
            loop_expr.stable_ptr.untyped(),
        )
    })();
    let block_main = lowered_expr_to_block_scope_end(ctx, subscope_main, block_expr)
        .map_err(LoweringFlowError::Failed)?;

    // Empty else block.
    let subscope_else = create_subscope(ctx, builder);
    let block_else_id = subscope_else.block_id;
    let else_block_input_var_id = ctx.new_var(VarRequest { ty: unit_ty, location: while_location });
    let block_else = return_a_unit(ctx, subscope_else, while_location, false)?;

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
        location: while_location,
    });
    builder.merge_and_end_with_match(ctx, match_info, vec![block_main, block_else], while_location)
}

/// Lowers an expression of type if where the condition is of type [semantic::Condition::Let].
pub fn lower_expr_while_let<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    loop_expr: &semantic::ExprWhile<'db>,
    matched_expr: semantic::ExprId,
    patterns: &[semantic::PatternId],
    loop_expr_id: semantic::ExprId,
    loop_stable_ptr: SyntaxStablePtrId<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a match expression: {:?}", loop_expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(loop_expr.stable_ptr.untyped());

    let graph = create_graph_expr_while_let(
        ctx,
        patterns,
        matched_expr,
        loop_expr.body,
        loop_expr_id,
        loop_stable_ptr,
    );
    lower_graph(ctx, builder, &graph, location)
}

/// Lowers a loop inner function into [Lowered].
/// Similar to `lower_function`, but adds a recursive call.
// TODO(spapini): Unite with `lower_function`.
pub fn lower_loop_function<'db>(
    encapsulating_ctx: &mut EncapsulatingLoweringContext<'db>,
    function_id: FunctionWithBodyId<'db>,
    loop_signature: EnrichedSemanticSignature<'db>,
    loop_ctx: LoopContext<'db>,
    return_type: semantic::TypeId<'db>,
) -> Maybe<Lowered<'db>> {
    let loop_expr_id = loop_ctx.loop_expr_id;
    let mut ctx =
        LoweringContext::new(encapsulating_ctx, function_id, loop_signature, return_type)?;
    let old_loop_ctx = ctx.current_loop_ctx.replace(loop_ctx);

    // Initialize builder.
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(root_block_id);

    let snapped_params = ctx.usages.usages[&loop_expr_id].snap_usage.clone();
    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            if snapped_params.contains_key::<MemberPath<'_>>(&(&param).into()) {
                ctx.snapped_semantics.insert((&param).into(), var);
            } else {
                builder.introduce((&param).into(), var);
            }
            var
        })
        .collect_vec();

    let root_ok = (|| {
        let (block_expr, stable_ptr) = match ctx.function_body.arenas.exprs[loop_expr_id].clone() {
            semantic::Expr::Loop(semantic::ExprLoop { body, stable_ptr, .. }) => {
                // Fetch body block expr.
                let semantic_block =
                    extract_matches!(&ctx.function_body.arenas.exprs[body], semantic::Expr::Block)
                        .clone();

                let block_expr = (|| {
                    lower_expr_block(&mut ctx, &mut builder, &semantic_block)?;
                    recursively_call_loop_func(
                        &mut ctx,
                        &mut builder,
                        loop_expr_id,
                        stable_ptr.untyped(),
                    )
                })();
                (block_expr, stable_ptr)
            }

            semantic::Expr::While(while_expr) => {
                let stable_ptr = while_expr.stable_ptr;
                let location = ctx.get_location(stable_ptr.untyped());
                let block_expr = (|| {
                    let ret_var =
                        lower_while_loop(&mut ctx, &mut builder, while_expr, loop_expr_id)?
                            .as_var_usage(&mut ctx, &mut builder)?;

                    lower_return(&mut ctx, &mut builder, ret_var, location, false)
                })();
                (block_expr, stable_ptr)
            }

            semantic::Expr::For(for_expr) => {
                let stable_ptr: cairo_lang_syntax::node::ast::ExprPtr<'_> = for_expr.stable_ptr;
                let block_expr: Result<LoweredExpr<'_>, LoweringFlowError<'_>> =
                    lower_for_loop(&mut ctx, &mut builder, for_expr, loop_expr_id);
                (block_expr, stable_ptr)
            }
            _ => unreachable!("Loop expression must be either loop, while or for."),
        };

        let ctx_ref = &mut ctx;
        let block_sealed = lowered_expr_to_block_scope_end(ctx_ref, builder, block_expr)?;
        wrap_sealed_block_as_function(ctx_ref, block_sealed, stable_ptr.untyped())?;

        Ok(root_block_id)
    })();
    ctx.current_loop_ctx = old_loop_ctx;

    let blocks = root_ok
        .map(|_| ctx.blocks.build().expect("Root block must exist."))
        .unwrap_or_else(Blocks::new_errored);
    Ok(Lowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.into(),
        parameters,
    })
}

/// Wraps `block_sealed` as the root block of a function.
fn wrap_sealed_block_as_function<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    block_sealed: SealedBlockBuilder<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> Maybe<()> {
    let Some(SealedGotoCallsite { mut builder, expr }) = block_sealed else {
        return Ok(());
    };
    let location = ctx.get_location(stable_ptr);
    match &expr {
        Some(expr) if ctx.variables[expr.var_id].ty == never_ty(ctx.db) => {
            // If the expression is of type never, then the block is unreachable, so add a match on
            // never to make it a viable block end.
            let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
                ctx.variables[expr.var_id].ty.long(ctx.db)
            else {
                unreachable!("Never type must be a concrete enum.");
            };
            builder.unreachable_match(
                ctx,
                MatchInfo::Enum(MatchEnumInfo {
                    concrete_enum_id: *concrete_enum_id,
                    input: *expr,
                    arms: vec![],
                    location,
                }),
            );
            Ok(())
        }
        _ => {
            // Convert to a return.
            let var_usage = expr.unwrap_or_else(|| {
                generators::StructConstruct { inputs: vec![], ty: unit_ty(ctx.db), location }
                    .add(ctx, &mut builder.statements)
            });
            builder.ret(ctx, var_usage, location)
        }
    }
}

/// Lowers a semantic block.
fn lower_block<'db, 'mt>(
    ctx: &mut LoweringContext<'db, 'mt>,
    mut builder: BlockBuilder<'db>,
    semantic_block: &semantic::ExprBlock<'db>,
) -> Maybe<SealedBlockBuilder<'db>> {
    let block_expr = lower_expr_block(ctx, &mut builder, semantic_block);
    lowered_expr_to_block_scope_end(ctx, builder, block_expr)
}

/// Lowers a semantic block.
fn lower_expr_block<'db, 'mt>(
    ctx: &mut LoweringContext<'db, 'mt>,
    builder: &mut BlockBuilder<'db>,
    expr_block: &semantic::ExprBlock<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a block.");
    for (i, stmt_id) in expr_block.statements.iter().enumerate() {
        let stmt = ctx.function_body.arenas.statements[*stmt_id].clone();
        let Err(err) = lower_statement(ctx, builder, &stmt) else {
            continue;
        };
        if err.is_unreachable() {
            let stmt_ptr = |id| ctx.function_body.arenas.statements[id].stable_ptr().untyped();
            let tail_ptr =
                expr_block.tail.map(|id| ctx.function_body.arenas.exprs[id].stable_ptr().untyped());
            // If flow is not reachable anymore, no need to continue emitting statements.
            if let Some(start_ptr) =
                expr_block.statements.get(i + 1).copied().map(stmt_ptr).or(tail_ptr)
            {
                let end_ptr = tail_ptr
                    .or_else(|| expr_block.statements.last().copied().map(stmt_ptr))
                    .unwrap();
                // Emit diagnostic for the rest of the block with unreachable.
                ctx.diagnostics.report(start_ptr, Unreachable { block_end_ptr: end_ptr });
            }
        }
        return Err(err);
    }
    // Determine correct block end.
    let location = ctx.get_location(expr_block.stable_ptr.untyped());
    expr_block
        .tail
        .map(|expr| lower_expr(ctx, builder, expr))
        .unwrap_or_else(|| Ok(LoweredExpr::Tuple { exprs: vec![], location }))
}

/// Lowers an expression that is either a complete block, or the end (tail expression) of a
/// block.
pub fn lower_tail_expr<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    mut builder: BlockBuilder<'db>,
    expr: semantic::ExprId,
) -> Maybe<SealedBlockBuilder<'db>> {
    log::trace!("Lowering a tail expression.");
    let lowered_expr = lower_expr(ctx, &mut builder, expr);
    lowered_expr_to_block_scope_end(ctx, builder, lowered_expr)
}

/// Converts [`LoweringResult<'db, LoweredExpr<'db>>`] into `BlockScopeEnd`.
pub fn lowered_expr_to_block_scope_end<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    mut builder: BlockBuilder<'db>,
    lowered_expr: LoweringResult<'db, LoweredExpr<'db>>,
) -> Maybe<SealedBlockBuilder<'db>> {
    Ok(match lowered_expr {
        Ok(LoweredExpr::Tuple { exprs, .. }) if exprs.is_empty() => builder.goto_callsite(None),
        Ok(lowered_expr) => match lowered_expr.as_var_usage(ctx, &mut builder) {
            Ok(var) => builder.goto_callsite(Some(var)),
            Err(err) => handle_lowering_flow_error(ctx, builder, err).map(|_| None)?,
        },
        Err(err) => handle_lowering_flow_error(ctx, builder, err).map(|_| None)?,
    })
}

/// Generates the lowering for a return `ret_expr`
/// in the case where we are inside a loop `is_early_return` indicates if this is a normal return
/// or an early return.
pub fn lower_return<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    mut ret_var: VarUsage<'db>,
    location: LocationId<'db>,
    is_early_return: bool,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    if let Some(LoopContext {
        early_return_info: Some(LoopEarlyReturnInfo { normal_return_variant, early_return_variant }),
        ..
    }) = &ctx.current_loop_ctx
    {
        let variant = if is_early_return { early_return_variant } else { normal_return_variant };

        ret_var = generators::EnumConstruct { input: ret_var, variant: *variant, location }
            .add(ctx, &mut builder.statements);
    }

    Err(LoweringFlowError::Return(ret_var, location))
}

/// Generates lowering to return a unit.
pub fn return_a_unit<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    mut builder: BlockBuilder<'db>,
    location: LocationId<'db>,
    is_early_return: bool,
) -> LoweringResult<'db, SealedBlockBuilder<'db>> {
    let ret_var = LoweredExpr::Tuple { exprs: vec![], location }.as_var_usage(ctx, &mut builder)?;

    let ret_expr = lower_return(ctx, &mut builder, ret_var, location, is_early_return);
    lowered_expr_to_block_scope_end(ctx, builder, ret_expr).map_err(LoweringFlowError::Failed)
}

/// Lowers a semantic statement.
pub fn lower_statement<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    stmt: &semantic::Statement<'db>,
) -> LoweringResult<'db, ()> {
    match stmt {
        semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
            log::trace!("Lowering an expression statement.");
            let lowered_expr = lower_expr(ctx, builder, *expr)?;
            // The LoweredExpr must be evaluated now to push/bring back variables in case it is
            // LoweredExpr::ExternEnum.
            if let LoweredExpr::ExternEnum(x) = lowered_expr {
                x.as_var_usage(ctx, builder)?;
            }
        }
        semantic::Statement::Let(semantic::StatementLet {
            pattern,
            expr,
            else_clause,
            stable_ptr,
        }) => {
            if let Some(else_clause) = else_clause {
                log::trace!("Lowering a let-else statement.");
                lower_let_else::lower_let_else(
                    ctx,
                    builder,
                    *pattern,
                    *expr,
                    *else_clause,
                    stable_ptr,
                )?;
            } else {
                log::trace!("Lowering a let statement.");
                let lowered_expr = lower_expr(ctx, builder, *expr)?;
                lower_single_pattern(ctx, builder, *pattern, lowered_expr)?;
            }
        }
        semantic::Statement::Continue(semantic::StatementContinue { stable_ptr }) => {
            log::trace!("Lowering a continue statement.");
            recursively_call_loop_func(
                ctx,
                builder,
                ctx.current_loop_ctx.as_ref().unwrap().loop_expr_id,
                stable_ptr.untyped(),
            )?;
            return Ok(());
        }
        semantic::Statement::Return(semantic::StatementReturn { expr_option, stable_ptr })
        | semantic::Statement::Break(semantic::StatementBreak { expr_option, stable_ptr }) => {
            log::trace!("Lowering a return | break statement.");
            let location = ctx.get_location(stable_ptr.untyped());
            let ret_var = match expr_option {
                None => {
                    LoweredExpr::Tuple { exprs: vec![], location }.as_var_usage(ctx, builder)?
                }
                Some(expr) => lower_expr_to_var_usage(ctx, builder, *expr)?,
            };

            lower_return(
                ctx,
                builder,
                ret_var,
                location,
                matches!(stmt, semantic::Statement::Return(_)),
            )?;
        }
        semantic::Statement::Item(_) => {}
    }
    Ok(())
}

// TODO(spapini): Separate match pattern from non-match (single) patterns in the semantic
// model.
/// Lowers a single-pattern (pattern that does not appear in a match. This includes structs,
/// tuples, variables, etc...
/// Adds the bound variables to the builder.
/// Note that single patterns are the only way to bind new local variables in the semantic
/// model.
fn lower_single_pattern<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    pattern_id: semantic::PatternId,
    lowered_expr: LoweredExpr<'db>,
) -> LoweringResult<'db, ()> {
    log::trace!("Lowering a single pattern.");
    let pattern = &ctx.function_body.arenas.patterns[pattern_id];
    match pattern {
        semantic::Pattern::Literal(_)
        | semantic::Pattern::StringLiteral(_)
        | semantic::Pattern::EnumVariant(_) => {
            return Err(LoweringFlowError::Failed(
                ctx.diagnostics.report(pattern.stable_ptr(), UnsupportedPattern),
            ));
        }
        semantic::Pattern::Variable(semantic::PatternVariable {
            name: _,
            var: sem_var,
            stable_ptr,
        }) => {
            let sem_var = semantic::Binding::LocalVar(sem_var.clone());
            let stable_ptr = *stable_ptr;
            // Deposit the owned variable in the semantic variables store.
            let var = lowered_expr.as_var_usage(ctx, builder)?.var_id;
            // Override variable location.
            ctx.variables.variables[var].location = ctx.get_location(stable_ptr.untyped());
            builder.put_semantic(sem_var.id(), var);
            // TODO(spapini): Build semantic_defs in semantic model.
            ctx.semantic_defs.insert(sem_var.id(), sem_var);
        }
        semantic::Pattern::Struct(structure) => {
            let members = ctx
                .db
                .concrete_struct_members(structure.concrete_struct_id)
                .map_err(LoweringFlowError::Failed)?;
            let mut required_members = UnorderedHashMap::<_, _>::from_iter(
                structure.field_patterns.iter().map(|(pattern, member)| (member.id, *pattern)),
            );
            let n_snapshots = structure.n_snapshots;
            let stable_ptr = structure.stable_ptr.untyped();
            let generator = generators::StructDestructure {
                input: lowered_expr.as_var_usage(ctx, builder)?,
                var_reqs: members
                    .iter()
                    .map(|(_, member)| VarRequest {
                        ty: wrap_in_snapshots(ctx.db, member.ty, n_snapshots),
                        location: ctx.get_location(
                            required_members
                                .get(&member.id)
                                .map(|pattern| {
                                    ctx.function_body.arenas.patterns[*pattern]
                                        .stable_ptr()
                                        .untyped()
                                })
                                .unwrap_or_else(|| stable_ptr),
                        ),
                    })
                    .collect(),
            };
            for (var_id, (_, member)) in
                izip!(generator.add(ctx, &mut builder.statements), members.iter())
            {
                if let Some(member_pattern) = required_members.remove(&member.id) {
                    let stable_ptr = ctx.function_body.arenas.patterns[member_pattern].stable_ptr();
                    lower_single_pattern(
                        ctx,
                        builder,
                        member_pattern,
                        LoweredExpr::AtVariable(VarUsage {
                            var_id,
                            location: ctx.get_location(stable_ptr.untyped()),
                        }),
                    )?;
                }
            }
        }
        semantic::Pattern::Tuple(semantic::PatternTuple {
            field_patterns: patterns, ty, ..
        })
        | semantic::Pattern::FixedSizeArray(semantic::PatternFixedSizeArray {
            elements_patterns: patterns,
            ty,
            ..
        }) => {
            let patterns = patterns.clone();
            lower_tuple_like_pattern_helper(ctx, builder, lowered_expr, &patterns, *ty)?;
        }
        semantic::Pattern::Otherwise(pattern) => {
            let stable_ptr = pattern.stable_ptr.untyped();
            let var = lowered_expr.as_var_usage(ctx, builder)?.var_id;
            ctx.variables.variables[var].location = ctx.get_location(stable_ptr);
        }
        semantic::Pattern::Missing(_) => unreachable!("Missing pattern in semantic model."),
    }
    Ok(())
}

/// A helper function to handle patterns of tuples or fixed size arrays.
fn lower_tuple_like_pattern_helper<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    lowered_expr: LoweredExpr<'db>,
    patterns: &[semantic::PatternId],
    ty: semantic::TypeId<'db>,
) -> LoweringResult<'db, ()> {
    let outputs = match lowered_expr {
        LoweredExpr::Tuple { exprs, .. } => exprs,
        LoweredExpr::FixedSizeArray { exprs, .. } => exprs,
        _ => {
            let (n_snapshots, long_type_id) = peel_snapshots(ctx.db, ty);
            let tys = match long_type_id {
                TypeLongId::Tuple(tys) => tys,
                TypeLongId::FixedSizeArray { type_id, size } => {
                    let size = size
                        .long(ctx.db)
                        .to_int()
                        .expect("Expected ConstValue::Int for size")
                        .to_usize()
                        .unwrap();
                    vec![type_id; size]
                }
                _ => unreachable!("Tuple-like pattern must be a tuple or fixed size array."),
            };
            let reqs = patterns
                .iter()
                .zip_eq(tys)
                .map(|(pattern, ty)| VarRequest {
                    ty: wrap_in_snapshots(ctx.db, ty, n_snapshots),
                    location: ctx.get_location(
                        ctx.function_body.arenas.patterns[*pattern].stable_ptr().untyped(),
                    ),
                })
                .collect();
            generators::StructDestructure {
                input: lowered_expr.as_var_usage(ctx, builder)?,
                var_reqs: reqs,
            }
            .add(ctx, &mut builder.statements)
            .into_iter()
            .map(|var_id| {
                LoweredExpr::AtVariable(VarUsage {
                    var_id,
                    location: ctx.variables[var_id].location,
                })
            })
            .collect()
        }
    };
    for (var, pattern) in zip_eq(outputs, patterns) {
        lower_single_pattern(ctx, builder, *pattern, var)?;
    }
    Ok(())
}

/// Lowers a semantic expression to a VarUsage.
///
/// For example, if we have the code:
/// foo(a + b)
///
/// then `a + b` will be assigned  variable and a VarUsage object whose origin
/// is the location of the `a + b` expression.
fn lower_expr_to_var_usage<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr_id: semantic::ExprId,
) -> LoweringResult<'db, VarUsage<'db>> {
    lower_expr(ctx, builder, expr_id)?.as_var_usage(ctx, builder)
}

/// Lowers a semantic expression.
fn lower_expr<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr_id: semantic::ExprId,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let expr = ctx.function_body.arenas.exprs[expr_id].clone();
    match expr {
        semantic::Expr::Constant(expr) => lower_expr_constant(ctx, &expr, builder),
        semantic::Expr::Tuple(expr) => lower_expr_tuple(ctx, &expr, builder),
        semantic::Expr::Snapshot(expr) => lower_expr_snapshot(ctx, &expr, builder),
        semantic::Expr::Desnap(expr) => lower_expr_desnap(ctx, &expr, builder),
        semantic::Expr::Assignment(expr) => lower_expr_assignment(ctx, &expr, builder),
        semantic::Expr::LogicalOperator(expr) => lower_logical_op(ctx, builder, &expr),
        semantic::Expr::Block(expr) => lower_expr_block(ctx, builder, &expr),
        semantic::Expr::FunctionCall(expr) => lower_expr_function_call(ctx, &expr, builder),
        semantic::Expr::Match(expr) => lower_expr_match(ctx, &expr, builder),
        semantic::Expr::If(expr) => lower_expr_if(ctx, builder, &expr),
        semantic::Expr::Loop(_) | semantic::Expr::While(_) | semantic::Expr::For(_) => {
            lower_expr_loop(ctx, builder, expr_id)
        }
        semantic::Expr::Var(expr) => {
            let member_path = ExprVarMemberPath::Var(expr.clone());
            log::trace!("Lowering a variable: {:?}", expr.debug(ctx.db));
            Ok(LoweredExpr::MemberPath(member_path, ctx.get_location(expr.stable_ptr.untyped())))
        }
        semantic::Expr::Literal(expr) => lower_expr_literal(ctx, &expr, builder),
        semantic::Expr::StringLiteral(expr) => lower_expr_string_literal(ctx, &expr, builder),
        semantic::Expr::MemberAccess(expr) => lower_expr_member_access(ctx, &expr, builder),
        semantic::Expr::StructCtor(expr) => lower_expr_struct_ctor(ctx, &expr, builder),
        semantic::Expr::EnumVariantCtor(expr) => lower_expr_enum_ctor(ctx, &expr, builder),
        semantic::Expr::FixedSizeArray(expr) => lower_expr_fixed_size_array(ctx, &expr, builder),
        semantic::Expr::ExprClosure(expr) => lower_expr_closure(ctx, &expr, expr_id, builder),
        semantic::Expr::PropagateError(expr) => lower_expr_error_propagate(ctx, &expr, builder),
        semantic::Expr::Missing(semantic::ExprMissing { diag_added, .. }) => {
            Err(LoweringFlowError::Failed(diag_added))
        }
    }
}

/// Lowers a semantic expression that is a literal, possibly including a negation.
fn lower_expr_literal<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprLiteral<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a literal: {:?}", expr.debug(&ctx.expr_formatter));
    Ok(LoweredExpr::AtVariable(lower_expr_literal_to_var_usage(
        ctx,
        expr.stable_ptr.untyped(),
        expr.ty,
        &expr.value,
        builder,
    )))
}

/// Same as [lower_expr_literal] but returns a [VarUsage] instead of a [LoweredExpr].
fn lower_expr_literal_to_var_usage<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    stable_ptr: SyntaxStablePtrId<'db>,
    ty: semantic::TypeId<'db>,
    value: &BigInt,
    builder: &mut BlockBuilder<'db>,
) -> VarUsage<'db> {
    let value = if let Err(err) = validate_literal(ctx.db, ty, value) {
        ConstValue::Missing(
            ctx.diagnostics.report(stable_ptr, LoweringDiagnosticKind::LiteralError(err)),
        )
        .intern(ctx.db)
    } else {
        ConstValueId::from_int(ctx.db, ty, value)
    };
    let location = ctx.get_location(stable_ptr);
    generators::Const { value, ty, location }.add(ctx, &mut builder.statements)
}

fn lower_expr_string_literal<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprStringLiteral<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a string literal: {:?}", expr.debug(&ctx.expr_formatter));
    let db = ctx.db;

    // Get all the relevant types from the corelib.
    let bytes31_ty = get_core_ty_by_name(db, SmolStrId::from(db, "bytes31"), vec![]);
    let data_array_ty = get_core_ty_by_name(
        db,
        SmolStrId::from(db, "Array"),
        vec![GenericArgumentId::Type(bytes31_ty)],
    );
    let byte_array_ty = get_core_ty_by_name(db, SmolStrId::from(db, "ByteArray"), vec![]);

    let array_submodule = core_submodule(db, SmolStrId::from(db, "array"));
    let data_array_new_function = FunctionLongId::Semantic(get_function_id(
        db,
        array_submodule,
        SmolStrId::from(db, "array_new"),
        vec![GenericArgumentId::Type(bytes31_ty)],
    ))
    .intern(db);
    let data_array_append_function = FunctionLongId::Semantic(get_function_id(
        db,
        array_submodule,
        SmolStrId::from(db, "array_append"),
        vec![GenericArgumentId::Type(bytes31_ty)],
    ))
    .intern(db);

    // Emit lowering statements to build the ByteArray struct components.
    let mut data_array_usage =
        build_empty_data_array(ctx, builder, expr, data_array_new_function, data_array_ty);
    let remainder = add_chunks_to_data_array(
        ctx,
        builder,
        expr,
        bytes31_ty,
        &mut data_array_usage,
        data_array_append_function,
        data_array_ty,
    );
    let (pending_word_usage, pending_word_len_usage) =
        add_pending_word(ctx, builder, expr, remainder);

    // Emit the lowering statement for creating the ByteArray struct.
    let byte_array_usage = generators::StructConstruct {
        inputs: vec![data_array_usage, pending_word_usage, pending_word_len_usage],
        ty: byte_array_ty,
        location: ctx.get_location(expr.stable_ptr.untyped()),
    }
    .add(ctx, &mut builder.statements);

    Ok(LoweredExpr::AtVariable(byte_array_usage))
}

/// Emits lowering statements to build an empty data array.
fn build_empty_data_array<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr: &semantic::ExprStringLiteral<'db>,
    data_array_new_function: crate::ids::FunctionId<'db>,
    data_array_ty: semantic::TypeId<'db>,
) -> VarUsage<'db> {
    generators::Call {
        function: data_array_new_function,
        inputs: vec![],
        coupon_input: None,
        extra_ret_tys: vec![],
        ret_tys: vec![data_array_ty],
        location: ctx.get_location(expr.stable_ptr.untyped()),
    }
    .add(ctx, &mut builder.statements)
    .returns[0]
}

/// Emits lowering statements to add 31-byte words to the given data array.
fn add_chunks_to_data_array<'db, 'r>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr: &'r semantic::ExprStringLiteral<'db>,
    bytes31_ty: semantic::TypeId<'db>,
    data_array_usage: &mut VarUsage<'db>,
    data_array_append_function: crate::ids::FunctionId<'db>,
    data_array_ty: semantic::TypeId<'db>,
) -> &'r [u8] {
    let expr_stable_ptr = expr.stable_ptr.untyped();

    let chunks = expr.value.as_bytes().chunks_exact(31);
    let remainder = chunks.remainder();
    for chunk in chunks {
        let chunk_usage = generators::Const {
            value: ConstValue::Int(BigInt::from_bytes_be(Sign::Plus, chunk), bytes31_ty)
                .intern(ctx.db),
            ty: bytes31_ty,
            location: ctx.get_location(expr_stable_ptr),
        }
        .add(ctx, &mut builder.statements);

        *data_array_usage = generators::Call {
            function: data_array_append_function,
            inputs: vec![*data_array_usage, chunk_usage],
            coupon_input: None,
            extra_ret_tys: vec![data_array_ty],
            ret_tys: vec![],
            location: ctx.get_location(expr_stable_ptr),
        }
        .add(ctx, &mut builder.statements)
        .extra_outputs[0];
    }
    remainder
}

/// Emits lowering statements to set variables for the pending word of the
/// ByteArray.
fn add_pending_word<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr: &semantic::ExprStringLiteral<'db>,
    pending_word_bytes: &[u8],
) -> (VarUsage<'db>, VarUsage<'db>) {
    let expr_stable_ptr = expr.stable_ptr.untyped();

    let pending_word_len_ty = bounded_int_ty(ctx.db, BigInt::ZERO, 30.into());
    let felt252_ty = ctx.db.core_info().felt252;

    let pending_word_usage = generators::Const {
        value: ConstValue::Int(BigInt::from_bytes_be(Sign::Plus, pending_word_bytes), felt252_ty)
            .intern(ctx.db),
        ty: felt252_ty,
        location: ctx.get_location(expr_stable_ptr),
    }
    .add(ctx, &mut builder.statements);

    let pending_word_len = expr.value.len() % 31;
    let pending_word_len_usage = generators::Const {
        value: ConstValue::Int(pending_word_len.into(), pending_word_len_ty).intern(ctx.db),
        ty: pending_word_len_ty,
        location: ctx.get_location(expr_stable_ptr),
    }
    .add(ctx, &mut builder.statements);
    (pending_word_usage, pending_word_len_usage)
}

fn lower_expr_constant<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprConstant<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a constant: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::Const { value: expr.const_value_id, ty: expr.ty, location }
            .add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprTuple].
fn lower_expr_tuple<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprTuple<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a tuple: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let inputs = expr
        .items
        .iter()
        .map(|arg_expr_id| lower_expr(ctx, builder, *arg_expr_id))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(LoweredExpr::Tuple { exprs: inputs, location })
}

/// Lowers an expression of type [semantic::ExprFixedSizeArray]
fn lower_expr_fixed_size_array<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprFixedSizeArray<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a fixed size array: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let exprs = match &expr.items {
        semantic::FixedSizeArrayItems::Items(items) => items
            .iter()
            .map(|arg_expr_id| lower_expr(ctx, builder, *arg_expr_id))
            .collect::<Result<Vec<_>, _>>()?,
        semantic::FixedSizeArrayItems::ValueAndSize(value, size) => {
            let lowered_value = lower_expr(ctx, builder, *value)?;
            let var_usage = lowered_value.as_var_usage(ctx, builder)?;
            let size = size
                .long(ctx.db)
                .to_int()
                .expect("Expected ConstValue::Int for size")
                .to_usize()
                .unwrap();
            if size == 0 {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics
                        .report(expr.stable_ptr.untyped(), EmptyRepeatedElementFixedSizeArray),
                ));
            }
            // If there are multiple elements, the type must be copyable as we copy the var `size`
            // times.
            if size > 1 && ctx.variables[var_usage.var_id].info.copyable.is_err() {
                {
                    return Err(LoweringFlowError::Failed(
                        ctx.diagnostics.report(expr.stable_ptr.0, FixedSizeArrayNonCopyableType),
                    ));
                }
            }
            let expr = LoweredExpr::AtVariable(var_usage);
            vec![expr; size]
        }
    };
    Ok(LoweredExpr::FixedSizeArray { exprs, location, ty: expr.ty })
}

/// Lowers an expression of type [semantic::ExprSnapshot].
fn lower_expr_snapshot<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprSnapshot<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a snapshot: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let expr = Box::new(lower_expr(ctx, builder, expr.inner)?);
    Ok(LoweredExpr::Snapshot { expr, location })
}

/// Lowers an expression of type [semantic::ExprDesnap].
fn lower_expr_desnap<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprDesnap<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a desnap: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let expr = lower_expr(ctx, builder, expr.inner)?;
    if let LoweredExpr::Snapshot { expr, .. } = &expr {
        return Ok(expr.as_ref().clone());
    }
    let input = expr.as_var_usage(ctx, builder)?;

    Ok(LoweredExpr::AtVariable(
        generators::Desnap { input, location }.add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprIf].
fn lower_expr_if<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr: &semantic::ExprIf<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let graph = create_graph_expr_if(ctx, expr);
    lower_graph(ctx, builder, &graph, ctx.get_location(expr.stable_ptr.untyped()))
}

/// Lowers an expression of type [semantic::ExprMatch].
fn lower_expr_match<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprMatch<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a match expression: {:?}", expr.debug(&ctx.expr_formatter));
    let graph = create_graph_expr_match(ctx, expr);
    lower_graph(ctx, builder, &graph, ctx.get_location(expr.stable_ptr.untyped()))
}

/// Lowers an expression of type [semantic::ExprFunctionCall].
fn lower_expr_function_call<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprFunctionCall<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a function call expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());

    // TODO(spapini): Use the correct stable pointer.
    let arg_inputs = lower_exprs_to_var_usages(ctx, &expr.args, builder)?;
    let ref_args_iter = expr
        .args
        .iter()
        .filter_map(|arg| try_extract_matches!(arg, ExprFunctionCallArg::Reference));
    let ref_tys = ref_args_iter.clone().map(|ref_arg| ref_arg.ty()).collect();

    let coupon_input = if let Some(coupon_arg) = expr.coupon_arg {
        Some(lower_expr_to_var_usage(ctx, builder, coupon_arg)?)
    } else {
        None
    };

    // If the function is panic(), do something special.
    if expr.function == get_core_function_id(ctx.db, SmolStrId::from(ctx.db, "panic"), vec![]) {
        let [input] = <[_; 1]>::try_from(arg_inputs).ok().unwrap();
        return Err(LoweringFlowError::Panic(input, location));
    }

    // The following is relevant only to extern functions.
    if expr.function.try_get_extern_function_id(ctx.db).is_some()
        && let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
            expr.ty.long(ctx.db)
    {
        let lowered_expr = LoweredExprExternEnum {
            function: expr.function,
            concrete_enum_id: *concrete_enum_id,
            inputs: arg_inputs,
            member_paths: ref_args_iter.cloned().collect(),
            location,
        };

        // It is still unknown whether we directly match on this enum result, or store it to a
        // variable. Thus we can't perform the call. Performing it and pushing/bringing-back
        // variables are done on the 2 places where this result is used:
        // 1. [lower_optimized_extern_match]
        // 2. [context::LoweredExprExternEnum::var]
        return Ok(LoweredExpr::ExternEnum(lowered_expr));
    }

    let (ref_outputs, res) = perform_function_call(
        ctx,
        builder,
        FunctionCallInfo {
            function: expr.function,
            inputs: arg_inputs,
            coupon_input,
            extra_ret_tys: ref_tys,
            ret_ty: expr.ty,
        },
        location,
    )?;

    // Rebind the ref variables.
    for (ref_arg, output_var) in zip_eq(ref_args_iter, ref_outputs) {
        builder.update_ref(ctx, ref_arg, output_var.var_id);
    }

    Ok(res)
}

/// Information required for [perform_function_call].
struct FunctionCallInfo<'db> {
    function: semantic::FunctionId<'db>,
    inputs: Vec<VarUsage<'db>>,
    coupon_input: Option<VarUsage<'db>>,
    extra_ret_tys: Vec<semantic::TypeId<'db>>,
    ret_ty: semantic::TypeId<'db>,
}

/// Creates a LoweredExpr for a function call, taking into consideration external function facades:
/// For external functions, sometimes the high level signature doesn't exactly correspond to the
/// external function returned variables / branches.
fn perform_function_call<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    function_call_info: FunctionCallInfo<'db>,
    location: LocationId<'db>,
) -> LoweringResult<'db, (Vec<VarUsage<'db>>, LoweredExpr<'db>)> {
    let FunctionCallInfo { function, inputs, coupon_input, extra_ret_tys, ret_ty } =
        function_call_info;

    // If the function is not extern, simply call it.
    if function.try_get_extern_function_id(ctx.db).is_none() {
        let call_result = generators::Call {
            function: function.lowered(ctx.db),
            inputs,
            coupon_input,
            extra_ret_tys,
            ret_tys: vec![ret_ty],
            location,
        }
        .add(ctx, &mut builder.statements);

        if ret_ty == never_ty(ctx.db) {
            // If the function returns never, the control flow is not allowed to continue.
            // This special case is required because without it the following code:
            // ```
            //    let res: felt252 = match a {
            //        true => 1,
            //        false => never_returns()
            //    };
            // ```
            // would try to assign never to res, which is not allowed.

            return Err(LoweringFlowError::Match(MatchInfo::Enum(MatchEnumInfo {
                concrete_enum_id: *extract_matches!(
                    extract_matches!(ret_ty.long(ctx.db), semantic::TypeLongId::Concrete),
                    semantic::ConcreteTypeId::Enum
                ),
                input: VarUsage { var_id: call_result.returns[0].var_id, location },
                arms: vec![],
                location,
            })));
        }

        let res = LoweredExpr::AtVariable(call_result.returns.into_iter().next().unwrap());
        return Ok((call_result.extra_outputs, res));
    };

    // Extern function.
    assert!(coupon_input.is_none(), "Extern functions cannot have a __coupon__ argument.");
    let ret_tys = extern_facade_return_tys(ctx, ret_ty);
    let call_result = generators::Call {
        function: function.lowered(ctx.db),
        inputs,
        coupon_input: None,
        extra_ret_tys,
        ret_tys,
        location,
    }
    .add(ctx, &mut builder.statements);

    Ok((
        call_result.extra_outputs,
        extern_facade_expr(
            ctx,
            ret_ty,
            call_result.returns.into_iter().map(|var_usage| var_usage.var_id).collect_vec(),
            location,
        ),
    ))
}

/// Lowers an expression of type [semantic::ExprLoop].
fn lower_expr_loop<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    loop_expr_id: ExprId,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let db = ctx.db;
    let (stable_ptr, return_type) = match ctx.function_body.arenas.exprs[loop_expr_id].clone() {
        semantic::Expr::Loop(semantic::ExprLoop { stable_ptr, ty, .. }) => (stable_ptr, ty),
        semantic::Expr::While(semantic::ExprWhile { stable_ptr, ty, .. }) => (stable_ptr, ty),
        semantic::Expr::For(semantic::ExprFor {
            stable_ptr,
            ty,
            into_iter,
            expr_id,
            into_iter_member_path,
            ..
        }) => {
            let var_id = lower_expr(ctx, builder, expr_id)?.as_var_usage(ctx, builder)?;
            let into_iter_call = generators::Call {
                function: into_iter.lowered(db),
                inputs: vec![var_id],
                coupon_input: None,
                extra_ret_tys: vec![],
                ret_tys: vec![db.concrete_function_signature(into_iter).unwrap().return_type],
                location: ctx.get_location(stable_ptr.untyped()),
            }
            .add(ctx, &mut builder.statements);
            let into_iter_var = into_iter_call.returns.into_iter().next().unwrap();
            let sem_var = LocalVariable {
                ty: db.concrete_function_signature(into_iter).unwrap().return_type,
                is_mut: true,
                id: extract_matches!(into_iter_member_path.base_var(), VarId::Local),
                allow_unused: true, // Synthetic variables should never generate unused warnings.
            };
            builder.put_semantic(into_iter_member_path.base_var(), into_iter_var.var_id);

            ctx.semantic_defs
                .insert(into_iter_member_path.base_var(), semantic::Binding::LocalVar(sem_var));

            (stable_ptr, ty)
        }
        _ => unreachable!("Loop expression must be either loop, while or for."),
    };

    let usage = &ctx.usages.usages[&loop_expr_id];
    let has_normal_return = return_type != never_ty(db);

    let (loop_return_ty, early_return_info) = if !has_normal_return {
        // If the loop does not have a normal return, `LoopResult` is not used
        // but we need to override the return type of the loop to match the function.
        (ctx.return_type, None)
    } else if !usage.has_early_return {
        (return_type, None)
    } else {
        let generic_args =
            vec![GenericArgumentId::Type(return_type), GenericArgumentId::Type(ctx.return_type)];

        let internal_module = core_submodule(db, SmolStrId::from(db, "internal"));
        let ret_ty = try_get_ty_by_name(
            db,
            internal_module,
            SmolStrId::from(db, "LoopResult"),
            generic_args.clone(),
        )
        .unwrap();
        (
            ret_ty,
            Some(LoopEarlyReturnInfo {
                normal_return_variant: get_enum_concrete_variant(
                    db,
                    internal_module,
                    SmolStrId::from(db, "LoopResult"),
                    generic_args.clone(),
                    SmolStrId::from(db, "Normal"),
                ),
                early_return_variant: get_enum_concrete_variant(
                    db,
                    internal_module,
                    SmolStrId::from(db, "LoopResult"),
                    generic_args,
                    SmolStrId::from(db, "EarlyReturn"),
                ),
            }),
        )
    };

    // Determine signature.
    let params = usage
        .usage
        .iter()
        .map(|(_, expr)| expr.clone())
        .chain(usage.snap_usage.iter().map(|(_, expr)| match expr {
            ExprVarMemberPath::Var(var) => {
                ExprVarMemberPath::Var(ExprVar { ty: wrap_in_snapshots(db, var.ty, 1), ..*var })
            }
            ExprVarMemberPath::Member { parent, member_id, stable_ptr, concrete_struct_id, ty } => {
                ExprVarMemberPath::Member {
                    parent: parent.clone(),
                    member_id: *member_id,
                    stable_ptr: *stable_ptr,
                    concrete_struct_id: *concrete_struct_id,
                    ty: wrap_in_snapshots(db, *ty, 1),
                }
            }
        }))
        .collect_vec();
    let extra_rets = usage.changes.iter().map(|(_, expr)| expr.clone()).collect_vec();

    let loop_location = ctx.get_location(stable_ptr.untyped());
    let loop_signature = EnrichedSemanticSignature {
        params,
        extra_rets,
        return_type: loop_return_ty,
        implicits: vec![],
        panicable: ctx.signature.panicable,
        location: loop_location,
    };

    // Get the function id.
    let function = FunctionWithBodyLongId::Generated {
        parent: ctx.semantic_function_id,
        key: GeneratedFunctionKey::Loop(stable_ptr),
    }
    .intern(db);

    // Generate the function.
    let encapsulating_ctx = ctx.encapsulating_ctx.take().unwrap();
    let loop_ctx = LoopContext { loop_expr_id, early_return_info: early_return_info.clone() };
    let lowered = lower_loop_function(
        encapsulating_ctx,
        function,
        loop_signature.clone(),
        loop_ctx,
        ctx.return_type,
    )
    .map_err(LoweringFlowError::Failed)?;
    // TODO(spapini): Recursive call.
    encapsulating_ctx.lowerings.insert(GeneratedFunctionKey::Loop(stable_ptr), lowered);
    ctx.encapsulating_ctx = Some(encapsulating_ctx);
    let call_loop_expr = call_loop_func_ex(
        ctx,
        loop_signature,
        builder,
        loop_expr_id,
        stable_ptr.untyped(),
        |ctx, builder, param| {
            if let Some(var) = builder.get_snap_ref(ctx, param) {
                return Some(var);
            };
            let input = builder.get_ref(ctx, param)?;
            let location = ctx.get_location(param.stable_ptr().untyped());
            let (original, snapped) =
                generators::Snapshot { input, location }.add(ctx, &mut builder.statements);
            builder.update_ref(ctx, param, original);
            Some(VarUsage { var_id: snapped, location })
        },
    )?;

    let Some(LoopEarlyReturnInfo { normal_return_variant, early_return_variant }) =
        early_return_info
    else {
        if !has_normal_return {
            let ret_var_usage = call_loop_expr.as_var_usage(ctx, builder)?;
            return Err(LoweringFlowError::Return(ret_var_usage, loop_location));
        }

        return Ok(call_loop_expr);
    };

    let loop_res = call_loop_expr.as_var_usage(ctx, builder)?;

    let normal_return_subscope = create_subscope(ctx, builder);
    let normal_return_subscope_block_id = normal_return_subscope.block_id;
    let normal_return_var_id = ctx.new_var(VarRequest { ty: return_type, location: loop_location });
    let sealed_normal_return = lowered_expr_to_block_scope_end(
        ctx,
        normal_return_subscope,
        Ok(LoweredExpr::AtVariable(VarUsage {
            var_id: normal_return_var_id,
            location: loop_location,
        })),
    )
    .map_err(LoweringFlowError::Failed)?;

    let mut early_return_subscope = create_subscope(ctx, builder);
    let early_return_var_id =
        ctx.new_var(VarRequest { ty: ctx.signature.return_type, location: loop_location });
    let early_return_subscope_block_id = early_return_subscope.block_id;
    let ret_expr = lower_return(
        ctx,
        &mut early_return_subscope,
        VarUsage { var_id: early_return_var_id, location: loop_location },
        loop_location,
        true,
    );
    let sealed_early_return = lowered_expr_to_block_scope_end(ctx, early_return_subscope, ret_expr)
        .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: normal_return_variant.concrete_enum_id,
        input: loop_res,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(normal_return_variant),
                block_id: normal_return_subscope_block_id,
                var_ids: vec![normal_return_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(early_return_variant),
                block_id: early_return_subscope_block_id,
                var_ids: vec![early_return_var_id],
            },
        ],
        location: loop_location,
    });

    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![sealed_normal_return, sealed_early_return],
        loop_location,
    )
}

/// Adds a call to an inner loop-generated function from the loop function itself.
fn recursively_call_loop_func<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    loop_expr_id: ExprId,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let loop_res = call_loop_func_ex(
        ctx,
        ctx.signature.clone(),
        builder,
        loop_expr_id,
        stable_ptr,
        |ctx, builder, param| builder.get_snap_ref(ctx, param),
    )?
    .as_var_usage(ctx, builder)?;
    Err(LoweringFlowError::Return(loop_res, ctx.get_location(stable_ptr)))
}

/// Adds a call to an inner loop-generated function.
fn call_loop_func_ex<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    loop_signature: EnrichedSemanticSignature<'db>,
    builder: &mut BlockBuilder<'db>,
    loop_expr_id: ExprId,
    stable_ptr: SyntaxStablePtrId<'db>,
    handle_snap: impl Fn(
        &mut LoweringContext<'db, '_>,
        &mut BlockBuilder<'db>,
        &ExprVarMemberPath<'db>,
    ) -> Option<VarUsage<'db>>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let location = ctx.get_location(stable_ptr);
    let loop_stable_ptr = ctx.function_body.arenas.exprs[loop_expr_id].stable_ptr();
    // Call it.
    let function = FunctionLongId::Generated(GeneratedFunction {
        parent: ctx.concrete_function_id.base_semantic_function(ctx.db),
        key: GeneratedFunctionKey::Loop(loop_stable_ptr),
    })
    .intern(ctx.db);
    let inputs = loop_signature
        .params
        .into_iter()
        .map(|param| {
            if let Some(var) = builder.get_ref_of_type(ctx, &param, param.ty()) {
                return Ok(var);
            }

            if let Some(var) = handle_snap(ctx, builder, &param)
                && ctx.variables[var.var_id].ty == param.ty()
            {
                return Ok(var);
            }

            // TODO(TomerStarkware): make sure this is unreachable and remove
            // `MemberPathLoop` diagnostic.
            Err(LoweringFlowError::Failed(
                ctx.diagnostics.report(param.stable_ptr(), MemberPathLoop),
            ))
        })
        .collect::<LoweringResult<'db, Vec<_>>>()?;
    let extra_ret_tys = loop_signature.extra_rets.iter().map(|path| path.ty()).collect_vec();
    let call_result = generators::Call {
        function,
        inputs,
        coupon_input: None,
        extra_ret_tys,
        ret_tys: vec![loop_signature.return_type],
        location,
    }
    .add(ctx, &mut builder.statements);

    // Rebind the ref variables.
    for (ref_arg, output_var) in zip_eq(&loop_signature.extra_rets, call_result.extra_outputs) {
        builder.update_ref(ctx, ref_arg, output_var.var_id);
    }

    Ok(LoweredExpr::AtVariable(call_result.returns.into_iter().next().unwrap()))
}

/// Lowers a sequence of expressions and return them all. If the flow ended in the middle,
/// propagates that flow error without returning any variable.
fn lower_exprs_to_var_usages<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    args: &[semantic::ExprFunctionCallArg<'db>],
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, Vec<VarUsage<'db>>> {
    // Since value expressions may depends on the same variables as the references, which must be
    // variables, all expressions must be evaluated before using the references for binding into the
    // call.
    // TODO(orizi): Consider changing this to disallow taking a reference and then using the
    // variable, while still allowing `arr.append(arr.len())`.
    let mut value_iter = args
        .iter()
        .filter_map(|arg| try_extract_matches!(arg, ExprFunctionCallArg::Value))
        .map(|arg_expr_id| lower_expr_to_var_usage(ctx, builder, *arg_expr_id))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter();
    Ok(args
        .iter()
        .map(|arg| match arg {
            semantic::ExprFunctionCallArg::Reference(ref_arg) => {
                builder.get_ref(ctx, ref_arg).unwrap()
            }
            semantic::ExprFunctionCallArg::Value(_) => value_iter.next().unwrap(),
        })
        .collect())
}

/// Lowers an expression of type [semantic::ExprEnumVariantCtor].
fn lower_expr_enum_ctor<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprEnumVariantCtor<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!(
        "Started lowering of an enum c'tor expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::EnumConstruct {
            input: lower_expr_to_var_usage(ctx, builder, expr.value_expr)?,
            variant: expr.variant,
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprMemberAccess].
fn lower_expr_member_access<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprMemberAccess<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a member-access expression: {:?}", expr.debug(&ctx.expr_formatter));
    if let Some(member_path) = &expr.member_path {
        return Ok(LoweredExpr::MemberPath(
            member_path.clone(),
            ctx.get_location(expr.stable_ptr.untyped()),
        ));
    }
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let members = ctx
        .db
        .concrete_struct_members(expr.concrete_struct_id)
        .map_err(LoweringFlowError::Failed)?;
    let member_idx =
        members.iter().position(|(_, member)| member.id == expr.member).ok_or_else(|| {
            LoweringFlowError::Failed(
                ctx.diagnostics.report(expr.stable_ptr.untyped(), UnexpectedError),
            )
        })?;
    Ok(LoweredExpr::AtVariable(
        generators::StructMemberAccess {
            input: lower_expr_to_var_usage(ctx, builder, expr.expr)?,
            member_tys: members
                .iter()
                .map(|(_, member)| wrap_in_snapshots(ctx.db, member.ty, expr.n_snapshots))
                .collect(),
            member_idx,
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprStructCtor].
fn lower_expr_struct_ctor<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprStructCtor<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a struct c'tor expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let members = ctx
        .db
        .concrete_struct_members(expr.concrete_struct_id)
        .map_err(LoweringFlowError::Failed)?;
    let mut member_expr_usages =
        UnorderedHashMap::<_, _>::from_iter(expr.members.iter().map(|(expr, id)| {
            let usage = lower_expr_to_var_usage(ctx, builder, *expr);
            (*id, usage)
        }));
    if members.len() != member_expr_usages.len() {
        // Semantic model should have made sure base struct exist if some members are missing.
        let base_struct = lower_expr(ctx, builder, expr.base_struct.unwrap())?;
        if let LoweredExpr::MemberPath(path, location) = base_struct {
            for (_, member) in members.iter() {
                let Entry::Vacant(entry) = member_expr_usages.entry(member.id) else {
                    continue;
                };
                let member_path = ExprVarMemberPath::Member {
                    parent: Box::new(path.clone()),
                    member_id: member.id,
                    stable_ptr: path.stable_ptr(),
                    concrete_struct_id: expr.concrete_struct_id,
                    ty: member.ty,
                };
                entry
                    .insert(Ok(LoweredExpr::MemberPath(member_path, location)
                        .as_var_usage(ctx, builder)?));
            }
        } else {
            for (base_member, (_, member)) in izip!(
                StructDestructure {
                    input: base_struct.as_var_usage(ctx, builder)?,
                    var_reqs: members
                        .iter()
                        .map(|(_, member)| VarRequest { ty: member.ty, location })
                        .collect(),
                }
                .add(ctx, &mut builder.statements),
                members.iter()
            ) {
                match member_expr_usages.entry(member.id) {
                    Entry::Occupied(_) => {}
                    Entry::Vacant(entry) => {
                        entry.insert(Ok(VarUsage { var_id: base_member, location }));
                    }
                }
            }
        }
    }
    Ok(LoweredExpr::AtVariable(
        generators::StructConstruct {
            inputs: members
                .iter()
                .map(|(_, member)| member_expr_usages.remove(&member.id).unwrap())
                .collect::<Result<Vec<_>, _>>()?,
            ty: expr.ty,
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
}

/// Adds the lowering for the destruct or panic_destruct of a capture variable.
fn add_capture_destruct_impl<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    capture_var_usage: VarUsage<'db>,
    closure_info: &ClosureInfo<'db>,
    location: StableLocation<'db>,
) -> Maybe<()> {
    let capture_ty_info = &ctx.variables.variables[capture_var_usage.var_id].info;
    // Skipping generation for the case of `Drop`.
    let Some(Ok(impl_id)) = [&capture_ty_info.destruct_impl, &capture_ty_info.panic_destruct_impl]
        .iter()
        .find(|infer_result| infer_result.is_ok())
    else {
        return Ok(());
    };

    let db = ctx.db;
    let concrete_trait = impl_id.concrete_trait(db)?;

    let trait_functions = db.trait_functions(concrete_trait.trait_id(db))?;

    assert_eq!(trait_functions.len(), 1);
    let trait_function = *trait_functions.values().next().unwrap();

    let generic_function = GenericFunctionId::Impl(ImplGenericFunctionId {
        impl_id: *impl_id,
        function: trait_function,
    });

    let function = semantic::FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args: vec![] },
    }
    .intern(db);

    let signature =
        EnrichedSemanticSignature::from_semantic(db, db.concrete_function_signature(function)?);

    let func_key = GeneratedFunctionKey::TraitFunc(trait_function, location);
    let function_id =
        FunctionWithBodyLongId::Generated { parent: ctx.semantic_function_id, key: func_key }
            .intern(db);

    let location_id = LocationId::from_stable_location(db, location);

    let encapsulating_ctx = ctx.encapsulating_ctx.take().unwrap();
    let return_type = signature.return_type;
    let lowered_impl_res = get_destruct_lowering(
        LoweringContext::new(encapsulating_ctx, function_id, signature, return_type)?,
        location_id,
        closure_info,
    );
    // Restore the encapsulating context before unwrapping the result.
    ctx.encapsulating_ctx = Some(encapsulating_ctx);
    ctx.lowerings.insert(func_key, lowered_impl_res?);
    Ok(())
}

/// Returns the lowering of the destruct function of a capture variable.
fn get_destruct_lowering<'db>(
    mut ctx: LoweringContext<'db, '_>,
    location_id: LocationId<'db>,
    closure_info: &ClosureInfo<'db>,
) -> Maybe<Lowered<'db>> {
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(root_block_id);

    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let var = ctx.new_var(VarRequest { ty: param.ty(), location: location_id });
            builder.introduce((&param).into(), var);
            var
        })
        .collect_vec();

    builder.destructure_closure(&mut ctx, location_id, parameters[0], closure_info);
    let var_usage =
        generators::StructConstruct { inputs: vec![], ty: unit_ty(ctx.db), location: location_id }
            .add(&mut ctx, &mut builder.statements);
    builder.ret(&mut ctx, var_usage, location_id)?;
    let lowered_impl = Lowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks: ctx.blocks.build().unwrap(),
        signature: ctx.signature.into(),
        parameters,
    };
    Ok(lowered_impl)
}

/// Adds the lowering for the closure function.
fn add_closure_call_function<'db>(
    encapsulated_ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprClosure<'db>,
    closure_info: &ClosureInfo<'db>,
    trait_id: cairo_lang_defs::ids::TraitId<'db>,
) -> Maybe<()> {
    let db = encapsulated_ctx.db;
    let closure_ty = extract_matches!(expr.ty.long(db), TypeLongId::Closure);
    let expr_location = encapsulated_ctx.get_location(expr.stable_ptr.untyped());
    let parameters_ty = TypeLongId::Tuple(closure_ty.param_tys.clone()).intern(db);
    let concrete_trait = ConcreteTraitLongId {
        trait_id,
        generic_args: vec![
            GenericArgumentId::Type(expr.ty),
            GenericArgumentId::Type(parameters_ty),
        ],
    }
    .intern(db);
    let Ok(impl_id) = semantic::types::get_impl_at_context(
        db,
        encapsulated_ctx.variables.lookup_context,
        concrete_trait,
        None,
    ) else {
        // If the impl doesn't exist, there won't be a call to the call-function, so we don't need
        // to generate it.
        return Ok(());
    };
    if !matches!(impl_id.long(db), ImplLongId::GeneratedImpl(_)) {
        // If the impl is not generated, we don't need to generate a lowering for it.
        return Ok(());
    }

    let trait_function: cairo_lang_defs::ids::TraitFunctionId<'_> = db
        .trait_function_by_name(trait_id, SmolStrId::from(db, "call"))
        .unwrap()
        .expect("Call function must exist for an Fn trait.");

    let generic_function =
        GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function: trait_function });
    let function = semantic::FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args: vec![] },
    }
    .intern(db);
    let function_with_body_id = FunctionWithBodyLongId::Generated {
        parent: encapsulated_ctx.semantic_function_id,
        key: GeneratedFunctionKey::TraitFunc(trait_function, closure_ty.wrapper_location),
    }
    .intern(db);
    let signature =
        EnrichedSemanticSignature::from_semantic(db, db.concrete_function_signature(function)?);

    let return_type = signature.return_type;
    let mut ctx =
        LoweringContext::new(encapsulated_ctx, function_with_body_id, signature, return_type)?;

    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(root_block_id);

    let info = db.core_info();
    let (closure_param_var_id, closure_var) = if trait_id == info.fn_once_trt {
        // If the closure is FnOnce, the closure is passed by value.
        let closure_param_var = ctx.new_var(VarRequest { ty: expr.ty, location: expr_location });
        let closure_var = VarUsage { var_id: closure_param_var, location: expr_location };
        (closure_param_var, closure_var)
    } else {
        // If the closure is Fn the closure argument will be a snapshot, so we need to desnap it.
        let closure_param_var = ctx
            .new_var(VarRequest { ty: wrap_in_snapshots(db, expr.ty, 1), location: expr_location });

        let closure_var = generators::Desnap {
            input: VarUsage { var_id: closure_param_var, location: expr_location },
            location: expr_location,
        }
        .add(&mut ctx, &mut builder.statements);
        (closure_param_var, closure_var)
    };
    let parameters: Vec<VariableId> = [
        closure_param_var_id,
        ctx.new_var(VarRequest { ty: parameters_ty, location: expr_location }),
    ]
    .into();
    let captured_vars = generators::StructDestructure {
        input: closure_var,
        var_reqs: chain!(closure_info.members.iter(), closure_info.snapshots.iter())
            .map(|(_, ty)| VarRequest { ty: *ty, location: expr_location })
            .collect_vec(),
    }
    .add(&mut ctx, &mut builder.statements);
    for (i, (param, _)) in closure_info.members.iter().enumerate() {
        builder.introduce(param.clone(), captured_vars[i]);
    }
    for (i, (param, _)) in closure_info.snapshots.iter().enumerate() {
        ctx.snapped_semantics.insert(param.clone(), captured_vars[i + closure_info.members.len()]);
    }
    let param_vars = generators::StructDestructure {
        input: VarUsage { var_id: parameters[1], location: expr_location },
        var_reqs: closure_ty
            .param_tys
            .iter()
            .map(|ty| VarRequest { ty: *ty, location: expr_location })
            .collect_vec(),
    }
    .add(&mut ctx, &mut builder.statements);
    for (param_var, param) in param_vars.into_iter().zip(expr.params.iter()) {
        builder.introduce((&parameter_as_member_path(param.clone())).into(), param_var);
        ctx.semantic_defs
            .insert(semantic::VarId::Param(param.id), semantic::Binding::Param(param.clone()));
    }
    let lowered_expr = lower_expr(&mut ctx, &mut builder, expr.body);
    let maybe_sealed_block = lowered_expr_to_block_scope_end(&mut ctx, builder, lowered_expr);
    let root_ok = maybe_sealed_block.and_then(|block_sealed| {
        wrap_sealed_block_as_function(&mut ctx, block_sealed, expr.stable_ptr.untyped())?;
        Ok(root_block_id)
    });
    let blocks = root_ok
        .map(|_| ctx.blocks.build().expect("Root block must exist."))
        .unwrap_or_else(Blocks::new_errored);

    let lowered = Lowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.into(),
        parameters,
    };
    encapsulated_ctx.lowerings.insert(
        GeneratedFunctionKey::TraitFunc(trait_function, closure_ty.wrapper_location),
        lowered,
    );
    Ok(())
}

/// Lowers an expression of type [semantic::ExprClosure].
fn lower_expr_closure<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprClosure<'db>,
    expr_id: semantic::ExprId,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a closure expression: {:?}", expr.debug(&ctx.expr_formatter));

    let usage = ctx.usages.usages[&expr_id].clone();
    let (capture_var_usage, closure_info) = builder.capture(ctx, usage, expr);
    let closure_variable = LoweredExpr::AtVariable(capture_var_usage);
    let closure_ty = extract_matches!(expr.ty.long(ctx.db), TypeLongId::Closure);
    let _ = add_capture_destruct_impl(
        ctx,
        capture_var_usage,
        &closure_info,
        closure_ty.wrapper_location,
    );
    add_closure_call_function(
        ctx,
        expr,
        &closure_info,
        if ctx.variables[capture_var_usage.var_id].info.copyable.is_ok() {
            ctx.db.core_info().fn_trt
        } else {
            ctx.db.core_info().fn_once_trt
        },
    )
    .map_err(LoweringFlowError::Failed)?;

    Ok(closure_variable)
}

/// Lowers an expression of type [semantic::ExprPropagateError].
fn lower_expr_error_propagate<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprPropagateError<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!(
        "Started lowering of an error-propagate expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, builder, expr.inner)?;
    let ExprPropagateError { ok_variant, err_variant, func_err_variant, .. } = expr;
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_error_propagate(
            ctx,
            builder,
            extern_enum,
            ok_variant,
            err_variant,
            func_err_variant,
            location,
        );
    }

    let match_input = lowered_expr.as_var_usage(ctx, builder)?;
    // Ok arm.
    let subscope_ok = create_subscope(ctx, builder);
    let block_ok_id = subscope_ok.block_id;
    let expr_var = ctx.new_var(VarRequest { ty: ok_variant.ty, location });
    let sealed_block_ok = subscope_ok.goto_callsite(Some(VarUsage { var_id: expr_var, location }));

    // Err arm.
    let mut subscope_err = create_subscope(ctx, builder);
    let block_err_id = subscope_err.block_id;
    let err_value = ctx.new_var(VarRequest { ty: err_variant.ty, location });
    let err_res = generators::EnumConstruct {
        input: VarUsage { var_id: err_value, location },
        variant: *func_err_variant,
        location,
    }
    .add(ctx, &mut subscope_err.statements);
    let ret_expr = lower_return(ctx, &mut subscope_err, err_res, location, true);
    let sealed_block_err = lowered_expr_to_block_scope_end(ctx, subscope_err, ret_expr)
        .map_err(LoweringFlowError::Failed)?;

    // Merge blocks.
    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: ok_variant.concrete_enum_id,
        input: match_input,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(*ok_variant),
                block_id: block_ok_id,
                var_ids: vec![expr_var],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(*err_variant),
                block_id: block_err_id,
                var_ids: vec![err_value],
            },
        ],
        location,
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![sealed_block_ok, sealed_block_err],
        location,
    )
}

/// Lowers an error propagation expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_error_propagate<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    extern_enum: LoweredExprExternEnum<'db>,
    ok_variant: &semantic::ConcreteVariant<'db>,
    err_variant: &semantic::ConcreteVariant<'db>,
    func_err_variant: &semantic::ConcreteVariant<'db>,
    location: LocationId<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Started lowering of an optimized error-propagate expression.");

    // Ok arm.
    let mut subscope_ok = create_subscope(ctx, builder);
    let block_ok_id = subscope_ok.block_id;
    let input_tys = match_extern_variant_arm_input_types(ctx, ok_variant.ty, &extern_enum);
    let mut input_vars: Vec<VariableId> =
        input_tys.into_iter().map(|ty| ctx.new_var(VarRequest { ty, location })).collect();
    let block_ok_input_vars = input_vars.clone();
    match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope_ok);
    let expr = extern_facade_expr(ctx, ok_variant.ty, input_vars, location)
        .as_var_usage(ctx, &mut subscope_ok)?;
    let sealed_block_ok = subscope_ok.goto_callsite(Some(expr));

    // Err arm.
    let mut subscope_err = create_subscope(ctx, builder);
    let block_err_id = subscope_err.block_id;
    let input_tys = match_extern_variant_arm_input_types(ctx, err_variant.ty, &extern_enum);
    let mut input_vars: Vec<VariableId> =
        input_tys.into_iter().map(|ty| ctx.new_var(VarRequest { ty, location })).collect();
    let block_err_input_vars = input_vars.clone();

    match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope_err);
    let expr = extern_facade_expr(ctx, err_variant.ty, input_vars, location);
    let input = expr.as_var_usage(ctx, &mut subscope_err)?;
    let err_res = generators::EnumConstruct { input, variant: *func_err_variant, location }
        .add(ctx, &mut subscope_err.statements);

    let ret_expr = lower_return(ctx, &mut subscope_err, err_res, location, true);
    let sealed_block_err = lowered_expr_to_block_scope_end(ctx, subscope_err, ret_expr)
        .map_err(LoweringFlowError::Failed)?;

    // Merge.
    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: extern_enum.inputs,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(*ok_variant),
                block_id: block_ok_id,
                var_ids: block_ok_input_vars,
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(*err_variant),
                block_id: block_err_id,
                var_ids: block_err_input_vars,
            },
        ],
        location,
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![sealed_block_ok, sealed_block_err],
        location,
    )
}

/// Returns the input types for an extern match variant arm.
fn match_extern_variant_arm_input_types<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    ty: semantic::TypeId<'db>,
    extern_enum: &LoweredExprExternEnum<'db>,
) -> Vec<semantic::TypeId<'db>> {
    let variant_input_tys = extern_facade_return_tys(ctx, ty);
    let ref_tys = extern_enum.member_paths.iter().map(|ref_arg| ref_arg.ty());
    chain!(ref_tys, variant_input_tys.into_iter()).collect()
}

/// Binds input references when matching on extern functions.
fn match_extern_arm_ref_args_bind<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    arm_inputs: &mut Vec<VariableId>,
    extern_enum: &LoweredExprExternEnum<'db>,
    subscope: &mut BlockBuilder<'db>,
) {
    let ref_outputs: Vec<_> = arm_inputs.drain(0..extern_enum.member_paths.len()).collect();
    // Bind the ref parameters.
    for (ref_arg, output_var) in zip_eq(&extern_enum.member_paths, ref_outputs) {
        subscope.update_ref(ctx, ref_arg, output_var);
    }
}

/// Lowers an expression of type [semantic::ExprAssignment].
fn lower_expr_assignment<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprAssignment<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!(
        "Started lowering of an assignment expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let var = lower_expr(ctx, builder, expr.rhs)?.as_var_usage(ctx, builder)?.var_id;
    builder.update_ref(ctx, &expr.ref_arg, var);
    Ok(LoweredExpr::Tuple { exprs: vec![], location })
}

/// Allocates and empty block in `ctx`.
fn alloc_empty_block<'db>(ctx: &mut LoweringContext<'db, '_>) -> BlockId {
    ctx.blocks.alloc_empty()
}

/// Creates a new subscope of the given builder, with an empty block.
fn create_subscope<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &BlockBuilder<'db>,
) -> BlockBuilder<'db> {
    builder.child_block_builder(alloc_empty_block(ctx))
}

/// Calls `.check_error_free()` and warns (in log) if there are errors.
fn check_error_free_or_warn<'db>(
    db: &dyn Database,
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    semantic_function_id: defs::ids::FunctionWithBodyId<'db>,
    diagnostics_description: &str,
) -> Maybe<()> {
    let declaration_error_free = diagnostics.check_error_free();
    declaration_error_free.inspect_err(|_| {
        log::warn!(
            "Function `{function_path}` has semantic diagnostics in its \
             {diagnostics_description}:\n{diagnostics_format}",
            function_path = semantic_function_id.full_path(db),
            diagnostics_format = diagnostics.format(db)
        );
    })
}
