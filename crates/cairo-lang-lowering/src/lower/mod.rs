use std::vec;

use block_builder::BlockBuilder;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic::corelib::{ErrorPropagationType, unwrap_error_propagation_type};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_semantic::items::imp::ImplLongId;
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{
    ConcreteFunction, ConcreteTraitLongId, ExprVar, LocalVariable, VarId, corelib,
};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::{Entry, UnorderedHashMap};
use cairo_lang_utils::{Intern, LookupIntern, extract_matches, try_extract_matches};
use defs::ids::TopLevelLanguageElementId;
use itertools::{Itertools, chain, izip, zip_eq};
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use refs::ClosureInfo;
use semantic::corelib::{
    core_submodule, get_core_function_id, get_core_ty_by_name, get_function_id, never_ty, unit_ty,
};
use semantic::items::constant::{ConstValue, value_as_const_value};
use semantic::types::{peel_snapshots, wrap_in_snapshots};
use semantic::{
    ExprFunctionCallArg, ExprId, ExprPropagateError, ExprVarMemberPath, GenericArgumentId,
    MatchArmSelector, SemanticDiagnostic, TypeLongId,
};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use self::block_builder::SealedBlockBuilder;
use self::context::{
    EncapsulatingLoweringContext, LoweredExpr, LoweredExprExternEnum, LoweringContext,
    LoweringFlowError, lowering_flow_error_to_sealed_block,
};
use self::external::{extern_facade_expr, extern_facade_return_tys};
use self::logical_op::lower_logical_op;
use self::lower_if::lower_expr_if;
use self::lower_match::lower_expr_match;
use crate::blocks::FlatBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::{self, *};
use crate::diagnostic::{LoweringDiagnosticsBuilder, MatchDiagnostic, MatchError, MatchKind};
use crate::ids::{
    FunctionLongId, FunctionWithBodyId, FunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey, LocationId, SemanticFunctionIdEx, Signature, parameter_as_member_path,
};
use crate::lower::context::{LoweringResult, VarRequest};
use crate::lower::generators::StructDestructure;
use crate::lower::lower_match::{
    MatchArmWrapper, TupleInfo, lower_concrete_enum_match, lower_expr_match_tuple,
    lower_optimized_extern_match,
};
use crate::{
    BlockId, FlatLowered, MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo, VarUsage, VariableId,
};

mod block_builder;
pub mod context;
mod external;
pub mod generators;
mod logical_op;
mod lower_if;
mod lower_match;
pub mod refs;

#[cfg(test)]
mod generated_test;

/// Lowering of a function together with extra generated functions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MultiLowering {
    pub main_lowering: FlatLowered,
    pub generated_lowerings: OrderedHashMap<GeneratedFunctionKey, FlatLowered>,
}

/// Lowers a semantic free function.
pub fn lower_semantic_function(
    db: &dyn LoweringGroup,
    semantic_function_id: defs::ids::FunctionWithBodyId,
) -> Maybe<MultiLowering> {
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
        Signature::from_semantic(db, signature),
        block_expr_id,
    )?;
    Ok(MultiLowering { main_lowering, generated_lowerings: encapsulating_ctx.lowerings })
}

/// Lowers a function into [FlatLowered].
pub fn lower_function(
    encapsulating_ctx: &mut EncapsulatingLoweringContext<'_>,
    function_id: FunctionWithBodyId,
    signature: Signature,
    block_expr_id: semantic::ExprId,
) -> Maybe<FlatLowered> {
    log::trace!("Lowering a free function.");
    let mut ctx = LoweringContext::new(encapsulating_ctx, function_id, signature)?;

    // Fetch body block expr.
    let semantic_block =
        extract_matches!(&ctx.function_body.arenas.exprs[block_expr_id], semantic::Expr::Block)
            .clone();

    // Initialize builder.
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(&mut ctx, root_block_id);

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
        .unwrap_or_else(FlatBlocks::new_errored);
    Ok(FlatLowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.clone(),
        parameters,
    })
}

/// Lowers an expression of type [semantic::ExprFor].
pub fn lower_for_loop(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    loop_expr: semantic::ExprFor,
    loop_expr_id: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    let semantic_db: &dyn SemanticGroup = ctx.db.upcast();
    let for_location = ctx.get_location(loop_expr.stable_ptr.untyped());
    let next_semantic_signature =
        semantic_db.concrete_function_signature(loop_expr.next_function_id).unwrap();
    let into_iter = builder.get_ref(ctx, &loop_expr.into_iter_member_path).unwrap();
    let next_call = generators::Call {
        function: loop_expr.next_function_id.lowered(ctx.db),
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
        unwrap_error_propagation_type(semantic_db, ctx.variables[next_value.var_id].ty)
            .expect("Expected Option type for next function return.")
    else {
        unreachable!("Return type for next function must be Option.")
    };
    let next_value_type = some_variant.ty;
    builder.update_ref(ctx, &loop_expr.into_iter_member_path, next_iterator.var_id);
    let pattern = ctx.function_body.arenas.patterns[loop_expr.pattern].clone();
    let unit_ty = corelib::unit_ty(semantic_db);
    let some_block: cairo_lang_semantic::ExprBlock =
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
    let lowered_pattern = lower_single_pattern(ctx, &mut some_subscope, pattern, variant_expr);
    let sealed_some = match lowered_pattern {
        Ok(_) => {
            let block_expr = (|| {
                lower_expr_block(ctx, &mut some_subscope, &some_block)?;
                // Add recursive call.
                let signature = ctx.signature.clone();
                call_loop_func(
                    ctx,
                    signature,
                    &mut some_subscope,
                    loop_expr_id,
                    loop_expr.stable_ptr.untyped(),
                )
            })();
            lowered_expr_to_block_scope_end(ctx, some_subscope, block_expr)
        }
        Err(err) => lowering_flow_error_to_sealed_block(ctx, some_subscope.clone(), err),
    }
    .map_err(LoweringFlowError::Failed)?;

    let none_subscope = create_subscope(ctx, builder);
    let none_var_id = ctx.new_var(VarRequest {
        ty: unit_ty,
        location: ctx.get_location(some_block.stable_ptr.untyped()),
    });
    let none_subscope_block_id = none_subscope.block_id;
    let sealed_none = lower_early_return(
        ctx,
        none_subscope,
        LoweredExpr::Tuple { exprs: vec![], location: for_location },
        for_location,
    )
    .map_err(LoweringFlowError::Failed)?;

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
pub fn lower_while_loop(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    loop_expr: semantic::ExprWhile,
    loop_expr_id: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    let semantic_condition = match &loop_expr.condition {
        semantic::Condition::BoolExpr(semantic_condition) => *semantic_condition,
        semantic::Condition::Let(match_expr, patterns) => {
            return lower_expr_while_let(
                ctx,
                builder,
                &loop_expr,
                *match_expr,
                patterns,
                MatchKind::WhileLet(loop_expr_id, loop_expr.stable_ptr.untyped()),
            );
        }
    };
    let condition = lower_expr_to_var_usage(ctx, builder, semantic_condition)?;
    let semantic_db = ctx.db.upcast();
    let unit_ty = corelib::unit_ty(semantic_db);
    let while_location = ctx.get_location(loop_expr.stable_ptr.untyped());

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
        // Add recursive call.
        let signature = ctx.signature.clone();
        call_loop_func(
            ctx,
            signature,
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
    let block_else = lower_early_return(
        ctx,
        subscope_else,
        LoweredExpr::Tuple { exprs: vec![], location: while_location },
        while_location,
    )
    .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: corelib::core_bool_enum(semantic_db),
        input: condition,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::false_variant(semantic_db)),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::true_variant(semantic_db)),
                block_id: block_main_id,
                var_ids: vec![main_block_var_id],
            },
        ],
        location: while_location,
    });
    builder.merge_and_end_with_match(ctx, match_info, vec![block_main, block_else], while_location)
}

/// Lowers an expression of type if where the condition is of type [semantic::Condition::Let].
pub fn lower_expr_while_let(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    loop_expr: &semantic::ExprWhile,
    matched_expr: semantic::ExprId,
    patterns: &[semantic::PatternId],
    match_type: MatchKind,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match expression: {:?}", loop_expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(loop_expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, builder, matched_expr)?;

    let matched_expr = ctx.function_body.arenas.exprs[matched_expr].clone();
    let ty = matched_expr.ty();

    if ty == ctx.db.core_info().felt252
        || corelib::get_convert_to_felt252_libfunc_name_by_type(ctx.db.upcast(), ty).is_some()
    {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            loop_expr.stable_ptr.untyped(),
            LoweringDiagnosticKind::MatchError(MatchError {
                kind: match_type,
                error: MatchDiagnostic::UnsupportedNumericInLetCondition,
            }),
        )));
    }

    let (n_snapshots, long_type_id) = peel_snapshots(ctx.db.upcast(), ty);

    let arms = vec![
        MatchArmWrapper { patterns: patterns.into(), expr: Some(loop_expr.body) },
        MatchArmWrapper { patterns: vec![], expr: None },
    ];

    if let Some(types) = try_extract_matches!(long_type_id, TypeLongId::Tuple) {
        return lower_expr_match_tuple(
            ctx,
            builder,
            lowered_expr,
            &matched_expr,
            &TupleInfo { types, n_snapshots },
            &arms,
            match_type,
        );
    }

    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, builder, extern_enum, &arms, match_type);
    }

    lower_concrete_enum_match(
        ctx,
        builder,
        &matched_expr,
        lowered_expr,
        &arms,
        location,
        match_type,
    )
}

/// Lowers a loop inner function into [FlatLowered].
/// Similar to `lower_function`, but adds a recursive call.
// TODO(spapini): Unite with `lower_function`.
pub fn lower_loop_function(
    encapsulating_ctx: &mut EncapsulatingLoweringContext<'_>,
    function_id: FunctionWithBodyId,
    loop_signature: Signature,
    loop_expr_id: semantic::ExprId,
) -> Maybe<FlatLowered> {
    let mut ctx = LoweringContext::new(encapsulating_ctx, function_id, loop_signature.clone())?;
    let old_loop_expr_id = std::mem::replace(&mut ctx.current_loop_expr_id, Some(loop_expr_id));

    // Initialize builder.
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(&mut ctx, root_block_id);

    let snapped_params = ctx.usages.usages[&loop_expr_id].snap_usage.clone();
    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let location = ctx.get_location(param.stable_ptr().untyped());
            let var = ctx.new_var(VarRequest { ty: param.ty(), location });
            if snapped_params.contains_key::<MemberPath>(&(&param).into()) {
                builder.update_snap_ref(&param, var)
            } else {
                builder.semantics.introduce((&param).into(), var);
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
                    // Add recursive call.
                    let signature = ctx.signature.clone();
                    call_loop_func(
                        &mut ctx,
                        signature,
                        &mut builder,
                        loop_expr_id,
                        stable_ptr.untyped(),
                    )
                })();
                (block_expr, stable_ptr)
            }

            semantic::Expr::While(while_expr) => {
                let stable_ptr = while_expr.stable_ptr;
                let block_expr = lower_while_loop(&mut ctx, &mut builder, while_expr, loop_expr_id);
                (block_expr, stable_ptr)
            }

            semantic::Expr::For(for_expr) => {
                let stable_ptr: cairo_lang_syntax::node::ast::ExprPtr = for_expr.stable_ptr;
                let block_expr: Result<LoweredExpr, LoweringFlowError> =
                    lower_for_loop(&mut ctx, &mut builder, for_expr, loop_expr_id);
                (block_expr, stable_ptr)
            }
            _ => unreachable!("Loop expression must be either loop, while or for."),
        };

        let block_sealed = lowered_expr_to_block_scope_end(&mut ctx, builder, block_expr)?;
        wrap_sealed_block_as_function(&mut ctx, block_sealed, stable_ptr.untyped())?;

        Ok(root_block_id)
    })();
    ctx.current_loop_expr_id = old_loop_expr_id;

    let blocks = root_ok
        .map(|_| ctx.blocks.build().expect("Root block must exist."))
        .unwrap_or_else(FlatBlocks::new_errored);
    Ok(FlatLowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.clone(),
        parameters,
    })
}

/// Wraps `block_sealed` as the root block of a function.
fn wrap_sealed_block_as_function(
    ctx: &mut LoweringContext<'_, '_>,
    block_sealed: SealedBlockBuilder,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<()> {
    let SealedBlockBuilder::GotoCallsite { mut builder, expr } = block_sealed else {
        return Ok(());
    };
    let location = ctx.get_location(stable_ptr);
    match &expr {
        Some(expr) if ctx.variables[expr.var_id].ty == never_ty(ctx.db.upcast()) => {
            // If the expression is of type never, then the block is unreachable, so add a match on
            // never to make it a viable block end.
            let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
                ctx.variables[expr.var_id].ty.lookup_intern(ctx.db)
            else {
                unreachable!("Never type must be a concrete enum.");
            };
            builder.unreachable_match(
                ctx,
                MatchInfo::Enum(MatchEnumInfo {
                    concrete_enum_id,
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
                generators::StructConstruct {
                    inputs: vec![],
                    ty: unit_ty(ctx.db.upcast()),
                    location,
                }
                .add(ctx, &mut builder.statements)
            });
            builder.ret(ctx, var_usage, location)
        }
    }
}

/// Lowers a semantic block.
fn lower_block(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    semantic_block: &semantic::ExprBlock,
) -> Maybe<SealedBlockBuilder> {
    let block_expr = lower_expr_block(ctx, &mut builder, semantic_block);
    lowered_expr_to_block_scope_end(ctx, builder, block_expr)
}

/// Lowers a semantic block.
fn lower_expr_block(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr_block: &semantic::ExprBlock,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a block.");
    for (i, stmt_id) in expr_block.statements.iter().enumerate() {
        let stmt = ctx.function_body.arenas.statements[*stmt_id].clone();
        let Err(err) = lower_statement(ctx, builder, &stmt) else {
            continue;
        };
        if err.is_unreachable() {
            // If flow is not reachable anymore, no need to continue emitting statements.
            // TODO(spapini): We might want to report unreachable for expr that abruptly
            // ends, e.g. `5 + {return; 6}`.
            if i + 1 < expr_block.statements.len() {
                let start_stmt = &ctx.function_body.arenas.statements[expr_block.statements[i + 1]];
                let end_stmt =
                    &ctx.function_body.arenas.statements[*expr_block.statements.last().unwrap()];
                // Emit diagnostic for the rest of the statements with unreachable.
                ctx.diagnostics.report(
                    start_stmt.stable_ptr().untyped(),
                    Unreachable { last_statement_ptr: end_stmt.into() },
                );
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
pub fn lower_tail_expr(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    expr: semantic::ExprId,
) -> Maybe<SealedBlockBuilder> {
    log::trace!("Lowering a tail expression.");
    let lowered_expr = lower_expr(ctx, &mut builder, expr);
    lowered_expr_to_block_scope_end(ctx, builder, lowered_expr)
}

/// Converts [`LoweringResult<LoweredExpr>`] into `BlockScopeEnd`.
pub fn lowered_expr_to_block_scope_end(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    lowered_expr: LoweringResult<LoweredExpr>,
) -> Maybe<SealedBlockBuilder> {
    Ok(match lowered_expr {
        Ok(LoweredExpr::Tuple { exprs, .. }) if exprs.is_empty() => builder.goto_callsite(None),
        Ok(lowered_expr) => match lowered_expr.as_var_usage(ctx, &mut builder) {
            Ok(var) => builder.goto_callsite(Some(var)),
            Err(err) => lowering_flow_error_to_sealed_block(ctx, builder, err)?,
        },
        Err(err) => lowering_flow_error_to_sealed_block(ctx, builder, err)?,
    })
}

/// Converts [`LoweringResult<LoweredExpr>`] into `BlockScopeEnd`.
pub fn lower_early_return(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    ret_expr: LoweredExpr,
    location: LocationId,
) -> Maybe<SealedBlockBuilder> {
    let lowered_expr = (|| {
        let ret_var_usage = ret_expr.as_var_usage(ctx, &mut builder)?;
        Err(LoweringFlowError::Return(ret_var_usage, location))
    })();
    lowered_expr_to_block_scope_end(ctx, builder, lowered_expr)
}

/// Lowers a semantic statement.
pub fn lower_statement(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    stmt: &semantic::Statement,
) -> Result<(), LoweringFlowError> {
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
        semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
            log::trace!("Lowering a let statement.");
            let lowered_expr = lower_expr(ctx, builder, *expr)?;
            let pattern = ctx.function_body.arenas.patterns[*pattern].clone();
            lower_single_pattern(ctx, builder, pattern, lowered_expr)?
        }
        semantic::Statement::Continue(semantic::StatementContinue { stable_ptr }) => {
            log::trace!("Lowering a continue statement.");
            let lowered_expr = call_loop_func(
                ctx,
                ctx.signature.clone(),
                builder,
                ctx.current_loop_expr_id.unwrap(),
                stable_ptr.untyped(),
            )?;
            let ret_var = lowered_expr.as_var_usage(ctx, builder)?;
            return Err(LoweringFlowError::Return(ret_var, ctx.get_location(stable_ptr.untyped())));
        }
        semantic::Statement::Return(semantic::StatementReturn { expr_option, stable_ptr })
        | semantic::Statement::Break(semantic::StatementBreak { expr_option, stable_ptr }) => {
            log::trace!("Lowering a return | break statement.");
            let ret_var = match expr_option {
                None => {
                    let location = ctx.get_location(stable_ptr.untyped());
                    LoweredExpr::Tuple { exprs: vec![], location }.as_var_usage(ctx, builder)?
                }
                Some(expr) => lower_expr_to_var_usage(ctx, builder, *expr)?,
            };
            return Err(LoweringFlowError::Return(ret_var, ctx.get_location(stable_ptr.untyped())));
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
fn lower_single_pattern(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    pattern: semantic::Pattern,
    lowered_expr: LoweredExpr,
) -> Result<(), LoweringFlowError> {
    log::trace!("Lowering a single pattern.");
    match pattern {
        semantic::Pattern::Literal(_)
        | semantic::Pattern::StringLiteral(_)
        | semantic::Pattern::EnumVariant(_) => {
            return Err(LoweringFlowError::Failed(
                ctx.diagnostics.report(&pattern, UnsupportedPattern),
            ));
        }
        semantic::Pattern::Variable(semantic::PatternVariable {
            name: _,
            var: sem_var,
            stable_ptr,
        }) => {
            let sem_var = semantic::Binding::LocalVar(sem_var);
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
                structure.field_patterns.iter().map(|(member, pattern)| (member.id, pattern)),
            );
            let generator = generators::StructDestructure {
                input: lowered_expr.as_var_usage(ctx, builder)?,
                var_reqs: members
                    .iter()
                    .map(|(_, member)| VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), member.ty, structure.n_snapshots),
                        location: ctx.get_location(
                            required_members
                                .get(&member.id)
                                .map(|pattern| {
                                    ctx.function_body.arenas.patterns[**pattern]
                                        .stable_ptr()
                                        .untyped()
                                })
                                .unwrap_or_else(|| structure.stable_ptr.untyped()),
                        ),
                    })
                    .collect(),
            };
            for (var_id, (_, member)) in
                izip!(generator.add(ctx, &mut builder.statements), members.iter())
            {
                if let Some(member_pattern) = required_members.remove(&member.id) {
                    let member_pattern = ctx.function_body.arenas.patterns[*member_pattern].clone();
                    let stable_ptr = member_pattern.stable_ptr();
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
            lower_tuple_like_pattern_helper(ctx, builder, lowered_expr, &patterns, ty)?;
        }
        semantic::Pattern::Otherwise(pattern) => {
            let var = lowered_expr.as_var_usage(ctx, builder)?.var_id;
            ctx.variables.variables[var].location = ctx.get_location(pattern.stable_ptr.untyped());
        }
        semantic::Pattern::Missing(_) => unreachable!("Missing pattern in semantic model."),
    }
    Ok(())
}

/// An helper function to handle patterns of tuples or fixed size arrays.
fn lower_tuple_like_pattern_helper(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    lowered_expr: LoweredExpr,
    patterns: &[semantic::PatternId],
    ty: semantic::TypeId,
) -> LoweringResult<()> {
    let outputs = match lowered_expr {
        LoweredExpr::Tuple { exprs, .. } => exprs,
        LoweredExpr::FixedSizeArray { exprs, .. } => exprs,
        _ => {
            let (n_snapshots, long_type_id) = peel_snapshots(ctx.db.upcast(), ty);
            let tys = match long_type_id {
                TypeLongId::Tuple(tys) => tys,
                TypeLongId::FixedSizeArray { type_id, size } => {
                    let size = size
                        .lookup_intern(ctx.db)
                        .into_int()
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
                    ty: wrap_in_snapshots(ctx.db.upcast(), ty, n_snapshots),
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
        lower_single_pattern(
            ctx,
            builder,
            ctx.function_body.arenas.patterns[*pattern].clone(),
            var,
        )?;
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
fn lower_expr_to_var_usage(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr_id: semantic::ExprId,
) -> LoweringResult<VarUsage> {
    lower_expr(ctx, builder, expr_id)?.as_var_usage(ctx, builder)
}

/// Lowers a semantic expression.
fn lower_expr(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr_id: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    let expr = ctx.function_body.arenas.exprs[expr_id].clone();
    match &expr {
        semantic::Expr::Constant(expr) => lower_expr_constant(ctx, expr, builder),
        semantic::Expr::Tuple(expr) => lower_expr_tuple(ctx, expr, builder),
        semantic::Expr::Snapshot(expr) => lower_expr_snapshot(ctx, expr, builder),
        semantic::Expr::Desnap(expr) => lower_expr_desnap(ctx, expr, builder),
        semantic::Expr::Assignment(expr) => lower_expr_assignment(ctx, expr, builder),
        semantic::Expr::LogicalOperator(expr) => lower_logical_op(ctx, builder, expr),
        semantic::Expr::Block(expr) => lower_expr_block(ctx, builder, expr),
        semantic::Expr::FunctionCall(expr) => lower_expr_function_call(ctx, expr, builder),
        semantic::Expr::Match(expr) => lower_expr_match(ctx, expr, builder),
        semantic::Expr::If(expr) => lower_expr_if(ctx, builder, expr),
        semantic::Expr::Loop(_) | semantic::Expr::While(_) | semantic::Expr::For(_) => {
            lower_expr_loop(ctx, builder, expr_id)
        }
        semantic::Expr::Var(expr) => {
            let member_path = ExprVarMemberPath::Var(expr.clone());
            log::trace!("Lowering a variable: {:?}", expr.debug(&ctx.expr_formatter));
            Ok(LoweredExpr::Member(member_path, ctx.get_location(expr.stable_ptr.untyped())))
        }
        semantic::Expr::Literal(expr) => lower_expr_literal(ctx, expr, builder),
        semantic::Expr::StringLiteral(expr) => lower_expr_string_literal(ctx, expr, builder),
        semantic::Expr::MemberAccess(expr) => lower_expr_member_access(ctx, expr, builder),
        semantic::Expr::StructCtor(expr) => lower_expr_struct_ctor(ctx, expr, builder),
        semantic::Expr::EnumVariantCtor(expr) => lower_expr_enum_ctor(ctx, expr, builder),
        semantic::Expr::FixedSizeArray(expr) => lower_expr_fixed_size_array(ctx, expr, builder),
        semantic::Expr::ExprClosure(expr) => lower_expr_closure(ctx, expr, expr_id, builder),
        semantic::Expr::PropagateError(expr) => lower_expr_error_propagate(ctx, expr, builder),
        semantic::Expr::Missing(semantic::ExprMissing { diag_added, .. }) => {
            Err(LoweringFlowError::Failed(*diag_added))
        }
    }
}

fn lower_expr_literal(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprLiteral,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a literal: {:?}", expr.debug(&ctx.expr_formatter));
    lower_expr_literal_helper(ctx, expr.stable_ptr.untyped(), expr.ty, &expr.value, builder)
}

/// Lowers a semantic expression that is a literal, possibly including a negation.
fn lower_expr_literal_helper(
    ctx: &mut LoweringContext<'_, '_>,
    stable_ptr: SyntaxStablePtrId,
    ty: semantic::TypeId,
    value: &BigInt,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    let value = value_as_const_value(ctx.db.upcast(), ty, value)
        .map_err(|err| {
            ctx.diagnostics.report(stable_ptr, LoweringDiagnosticKind::LiteralError(err))
        })
        .unwrap_or_else(ConstValue::Missing);
    let location = ctx.get_location(stable_ptr);
    Ok(LoweredExpr::AtVariable(
        generators::Const { value, ty, location }.add(ctx, &mut builder.statements),
    ))
}

fn lower_expr_string_literal(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprStringLiteral,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a string literal: {:?}", expr.debug(&ctx.expr_formatter));
    let semantic_db = ctx.db.upcast();

    // Get all the relevant types from the corelib.
    let bytes31_ty = get_core_ty_by_name(semantic_db, "bytes31".into(), vec![]);
    let data_array_ty =
        get_core_ty_by_name(semantic_db, "Array".into(), vec![GenericArgumentId::Type(bytes31_ty)]);
    let byte_array_ty = get_core_ty_by_name(semantic_db, "ByteArray".into(), vec![]);

    let array_submodule = core_submodule(semantic_db, "array");
    let data_array_new_function = FunctionLongId::Semantic(get_function_id(
        semantic_db,
        array_submodule,
        "array_new".into(),
        vec![GenericArgumentId::Type(bytes31_ty)],
    ))
    .intern(ctx.db);
    let data_array_append_function = FunctionLongId::Semantic(get_function_id(
        semantic_db,
        array_submodule,
        "array_append".into(),
        vec![GenericArgumentId::Type(bytes31_ty)],
    ))
    .intern(ctx.db);

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
fn build_empty_data_array(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprStringLiteral,
    data_array_new_function: crate::ids::FunctionId,
    data_array_ty: semantic::TypeId,
) -> VarUsage {
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
fn add_chunks_to_data_array<'a>(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &'a semantic::ExprStringLiteral,
    bytes31_ty: semantic::TypeId,
    data_array_usage: &mut VarUsage,
    data_array_append_function: crate::ids::FunctionId,
    data_array_ty: semantic::TypeId,
) -> &'a [u8] {
    let expr_stable_ptr = expr.stable_ptr.untyped();

    let chunks = expr.value.as_bytes().chunks_exact(31);
    let remainder = chunks.remainder();
    for chunk in chunks {
        let chunk_usage = generators::Const {
            value: ConstValue::Int(BigInt::from_bytes_be(Sign::Plus, chunk), bytes31_ty),
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
fn add_pending_word(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprStringLiteral,
    pending_word_bytes: &[u8],
) -> (VarUsage, VarUsage) {
    let expr_stable_ptr = expr.stable_ptr.untyped();

    let u32_ty = ctx.db.core_info().u32;
    let felt252_ty = ctx.db.core_info().felt252;

    let pending_word_usage = generators::Const {
        value: ConstValue::Int(BigInt::from_bytes_be(Sign::Plus, pending_word_bytes), felt252_ty),
        ty: felt252_ty,
        location: ctx.get_location(expr_stable_ptr),
    }
    .add(ctx, &mut builder.statements);

    let pending_word_len = expr.value.len() % 31;
    let pending_word_len_usage = generators::Const {
        value: ConstValue::Int(pending_word_len.into(), u32_ty),
        ty: u32_ty,
        location: ctx.get_location(expr_stable_ptr),
    }
    .add(ctx, &mut builder.statements);
    (pending_word_usage, pending_word_len_usage)
}

fn lower_expr_constant(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprConstant,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a constant: {:?}", expr.debug(&ctx.expr_formatter));
    let value = expr.const_value_id.lookup_intern(ctx.db);
    let ty = expr.ty;

    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::Const { value, ty, location }.add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprTuple].
fn lower_expr_tuple(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprTuple,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
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
fn lower_expr_fixed_size_array(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprFixedSizeArray,
    builder: &mut BlockBuilder,
) -> Result<LoweredExpr, LoweringFlowError> {
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
                .lookup_intern(ctx.db)
                .into_int()
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
            if size > 1 && ctx.variables[var_usage.var_id].copyable.is_err() {
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
fn lower_expr_snapshot(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprSnapshot,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a snapshot: {:?}", expr.debug(&ctx.expr_formatter));
    // If the inner expression is a variable, or a member access, and we already have a snapshot var
    // we can use it without creating a new one.
    // Note that in a closure we might only have a snapshot of the variable and not the original.
    match &ctx.function_body.arenas.exprs[expr.inner] {
        semantic::Expr::Var(expr_var) => {
            let member_path = ExprVarMemberPath::Var(expr_var.clone());
            if let Some(var) = builder.get_snap_ref(ctx, &member_path) {
                return Ok(LoweredExpr::AtVariable(var));
            }
        }
        semantic::Expr::MemberAccess(expr) => {
            if let Some(var) = expr
                .member_path
                .clone()
                .and_then(|member_path| builder.get_snap_ref(ctx, &member_path))
            {
                return Ok(LoweredExpr::AtVariable(var));
            }
        }
        _ => {}
    }
    let lowered = lower_expr(ctx, builder, expr.inner)?;

    let location = ctx.get_location(expr.stable_ptr.untyped());
    let expr = Box::new(lowered);
    Ok(LoweredExpr::Snapshot { expr, location })
}

/// Lowers an expression of type [semantic::ExprDesnap].
fn lower_expr_desnap(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprDesnap,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
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

/// Lowers an expression of type [semantic::ExprFunctionCall].
fn lower_expr_function_call(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprFunctionCall,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
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
    if expr.function == get_core_function_id(ctx.db.upcast(), "panic".into(), vec![]) {
        let [input] = <[_; 1]>::try_from(arg_inputs).ok().unwrap();
        return Err(LoweringFlowError::Panic(input, location));
    }

    // The following is relevant only to extern functions.
    if expr.function.try_get_extern_function_id(ctx.db.upcast()).is_some() {
        if let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
            expr.ty.lookup_intern(ctx.db)
        {
            let lowered_expr = LoweredExprExternEnum {
                function: expr.function,
                concrete_enum_id,
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
struct FunctionCallInfo {
    function: semantic::FunctionId,
    inputs: Vec<VarUsage>,
    coupon_input: Option<VarUsage>,
    extra_ret_tys: Vec<semantic::TypeId>,
    ret_ty: semantic::TypeId,
}

/// Creates a LoweredExpr for a function call, taking into consideration external function facades:
/// For external functions, sometimes the high level signature doesn't exactly correspond to the
/// external function returned variables / branches.
fn perform_function_call(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    function_call_info: FunctionCallInfo,
    location: LocationId,
) -> Result<(Vec<VarUsage>, LoweredExpr), LoweringFlowError> {
    let FunctionCallInfo { function, inputs, coupon_input, extra_ret_tys, ret_ty } =
        function_call_info;

    // If the function is not extern, simply call it.
    if function.try_get_extern_function_id(ctx.db.upcast()).is_none() {
        let call_result = generators::Call {
            function: function.lowered(ctx.db),
            inputs,
            coupon_input,
            extra_ret_tys,
            ret_tys: vec![ret_ty],
            location,
        }
        .add(ctx, &mut builder.statements);

        if ret_ty == never_ty(ctx.db.upcast()) {
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
                concrete_enum_id: extract_matches!(
                    extract_matches!(ret_ty.lookup_intern(ctx.db), semantic::TypeLongId::Concrete),
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
fn lower_expr_loop(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    loop_expr_id: ExprId,
) -> LoweringResult<LoweredExpr> {
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
            let semantic_db: &dyn SemanticGroup = ctx.db.upcast();
            let var_id = lower_expr(ctx, builder, expr_id)?.as_var_usage(ctx, builder)?;
            let into_iter_call = generators::Call {
                function: into_iter.lowered(ctx.db),
                inputs: vec![var_id],
                coupon_input: None,
                extra_ret_tys: vec![],
                ret_tys: vec![
                    semantic_db.concrete_function_signature(into_iter).unwrap().return_type,
                ],
                location: ctx.get_location(stable_ptr.untyped()),
            }
            .add(ctx, &mut builder.statements);
            let into_iter_var = into_iter_call.returns.into_iter().next().unwrap();
            let sem_var = LocalVariable {
                ty: semantic_db.concrete_function_signature(into_iter).unwrap().return_type,
                is_mut: true,
                id: extract_matches!(into_iter_member_path.base_var(), VarId::Local),
            };
            builder.put_semantic(into_iter_member_path.base_var(), into_iter_var.var_id);

            ctx.semantic_defs
                .insert(into_iter_member_path.base_var(), semantic::Binding::LocalVar(sem_var));

            (stable_ptr, ty)
        }
        _ => unreachable!("Loop expression must be either loop, while or for."),
    };

    let usage = &ctx.usages.usages[&loop_expr_id];

    // Determine signature.
    let params = usage
        .usage
        .iter()
        .map(|(_, expr)| expr.clone())
        .chain(usage.snap_usage.iter().map(|(_, expr)| match expr {
            ExprVarMemberPath::Var(var) => ExprVarMemberPath::Var(ExprVar {
                ty: wrap_in_snapshots(ctx.db.upcast(), var.ty, 1),
                ..*var
            }),
            ExprVarMemberPath::Member { parent, member_id, stable_ptr, concrete_struct_id, ty } => {
                ExprVarMemberPath::Member {
                    parent: parent.clone(),
                    member_id: *member_id,
                    stable_ptr: *stable_ptr,
                    concrete_struct_id: *concrete_struct_id,
                    ty: wrap_in_snapshots(ctx.db.upcast(), *ty, 1),
                }
            }
        }))
        .collect_vec();
    let extra_rets = usage.changes.iter().map(|(_, expr)| expr.clone()).collect_vec();

    let loop_signature = Signature {
        params,
        extra_rets,
        return_type,
        implicits: vec![],
        panicable: ctx.signature.panicable,
        location: ctx.get_location(stable_ptr.untyped()),
    };

    // Get the function id.
    let function = FunctionWithBodyLongId::Generated {
        parent: ctx.semantic_function_id,
        key: GeneratedFunctionKey::Loop(stable_ptr),
    }
    .intern(ctx.db);

    // Generate the function.
    let encapsulating_ctx = std::mem::take(&mut ctx.encapsulating_ctx).unwrap();
    let lowered =
        lower_loop_function(encapsulating_ctx, function, loop_signature.clone(), loop_expr_id)
            .map_err(LoweringFlowError::Failed)?;
    // TODO(spapini): Recursive call.
    encapsulating_ctx.lowerings.insert(GeneratedFunctionKey::Loop(stable_ptr), lowered);
    ctx.encapsulating_ctx = Some(encapsulating_ctx);
    let old_loop_expr_id = std::mem::replace(&mut ctx.current_loop_expr_id, Some(loop_expr_id));
    let call = call_loop_func_ex(
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
    );

    ctx.current_loop_expr_id = old_loop_expr_id;
    call
}

/// Adds a call to an inner loop-generated function from the loop function itself.
fn call_loop_func(
    ctx: &mut LoweringContext<'_, '_>,
    loop_signature: Signature,
    builder: &mut BlockBuilder,
    loop_expr_id: ExprId,
    stable_ptr: SyntaxStablePtrId,
) -> LoweringResult<LoweredExpr> {
    call_loop_func_ex(
        ctx,
        loop_signature,
        builder,
        loop_expr_id,
        stable_ptr,
        |ctx, builder, param| builder.get_snap_ref(ctx, param),
    )
}

/// Adds a call to an inner loop-generated function.
fn call_loop_func_ex(
    ctx: &mut LoweringContext<'_, '_>,
    loop_signature: Signature,
    builder: &mut BlockBuilder,
    loop_expr_id: ExprId,
    stable_ptr: SyntaxStablePtrId,
    handle_snap: impl Fn(
        &mut LoweringContext<'_, '_>,
        &mut BlockBuilder,
        &ExprVarMemberPath,
    ) -> Option<VarUsage>,
) -> LoweringResult<LoweredExpr> {
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
            builder
                .get_ref(ctx, &param)
                .and_then(|var| (ctx.variables[var.var_id].ty == param.ty()).then_some(var))
                .or_else(|| {
                    let var = handle_snap(ctx, builder, &param)?;
                    (ctx.variables[var.var_id].ty == param.ty()).then_some(var)
                })
                .ok_or_else(|| {
                    // TODO(TomerStaskware): make sure this is unreachable and remove
                    // `MemberPathLoop` diagnostic.
                    LoweringFlowError::Failed(
                        ctx.diagnostics.report(param.stable_ptr(), MemberPathLoop),
                    )
                })
        })
        .collect::<LoweringResult<Vec<_>>>()?;
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
fn lower_exprs_to_var_usages(
    ctx: &mut LoweringContext<'_, '_>,
    args: &[semantic::ExprFunctionCallArg],
    builder: &mut BlockBuilder,
) -> Result<Vec<VarUsage>, LoweringFlowError> {
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
fn lower_expr_enum_ctor(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprEnumVariantCtor,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!(
        "Started lowering of an enum c'tor expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::EnumConstruct {
            input: lower_expr_to_var_usage(ctx, builder, expr.value_expr)?,
            variant: expr.variant.clone(),
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprMemberAccess].
fn lower_expr_member_access(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMemberAccess,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a member-access expression: {:?}", expr.debug(&ctx.expr_formatter));
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
    if let Some(member_path) = &expr.member_path {
        return Ok(LoweredExpr::Member(
            member_path.clone(),
            ctx.get_location(expr.stable_ptr.untyped()),
        ));
    }
    Ok(LoweredExpr::AtVariable(
        generators::StructMemberAccess {
            input: lower_expr_to_var_usage(ctx, builder, expr.expr)?,
            member_tys: members
                .iter()
                .map(|(_, member)| wrap_in_snapshots(ctx.db.upcast(), member.ty, expr.n_snapshots))
                .collect(),
            member_idx,
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
}

/// Lowers an expression of type [semantic::ExprStructCtor].
fn lower_expr_struct_ctor(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprStructCtor,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a struct c'tor expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let members = ctx
        .db
        .concrete_struct_members(expr.concrete_struct_id)
        .map_err(LoweringFlowError::Failed)?;
    let mut member_expr_usages =
        UnorderedHashMap::<_, _>::from_iter(expr.members.iter().map(|(id, expr)| {
            let usage = lower_expr_to_var_usage(ctx, builder, *expr);
            (*id, usage)
        }));
    if members.len() != member_expr_usages.len() {
        // Semantic model should have made sure base struct exist if some members are missing.
        let base_struct = lower_expr(ctx, builder, expr.base_struct.unwrap())?;
        if let LoweredExpr::Member(path, location) = base_struct {
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
                entry.insert(Ok(
                    LoweredExpr::Member(member_path, location).as_var_usage(ctx, builder)?
                ));
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
fn add_capture_destruct_impl(
    ctx: &mut LoweringContext<'_, '_>,
    capture_var_usage: VarUsage,
    closure_info: &ClosureInfo,
    location: StableLocation,
) -> Maybe<()> {
    let capture_var = &ctx.variables.variables[capture_var_usage.var_id];
    // Skipping generation for the case of `Drop`.
    let Some(Ok(impl_id)) = [&capture_var.destruct_impl, &capture_var.panic_destruct_impl]
        .iter()
        .find(|infer_result| infer_result.is_ok())
    else {
        return Ok(());
    };

    let semantic_db = ctx.db.upcast();
    let concrete_trait = impl_id.concrete_trait(semantic_db)?;

    let trait_functions = semantic_db.trait_functions(concrete_trait.trait_id(semantic_db))?;

    assert_eq!(trait_functions.len(), 1);
    let trait_function = *trait_functions.values().next().unwrap();

    let generic_function = GenericFunctionId::Impl(ImplGenericFunctionId {
        impl_id: *impl_id,
        function: trait_function,
    });

    let function = semantic::FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args: vec![] },
    }
    .intern(ctx.db);

    let signature =
        Signature::from_semantic(ctx.db, semantic_db.concrete_function_signature(function)?);

    let func_key = GeneratedFunctionKey::TraitFunc(trait_function, location);
    let function_id =
        FunctionWithBodyLongId::Generated { parent: ctx.semantic_function_id, key: func_key }
            .intern(ctx.db);

    let location_id = LocationId::from_stable_location(ctx.db, location);

    let encapsulating_ctx = std::mem::take(&mut ctx.encapsulating_ctx).unwrap();
    let lowered_impl_res = get_destruct_lowering(
        LoweringContext::new(encapsulating_ctx, function_id, signature)?,
        location_id,
        closure_info,
    );
    // Restore the encapsulating context before unwrapping the result.
    ctx.encapsulating_ctx = Some(encapsulating_ctx);
    ctx.lowerings.insert(func_key, lowered_impl_res?);
    Ok(())
}

/// Returns the lowering of the destruct function of a capture variable.
fn get_destruct_lowering(
    mut ctx: LoweringContext<'_, '_>,
    location_id: LocationId,
    closure_info: &ClosureInfo,
) -> Maybe<FlatLowered> {
    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(&mut ctx, root_block_id);

    let parameters = ctx
        .signature
        .params
        .clone()
        .into_iter()
        .map(|param| {
            let var = ctx.new_var(VarRequest { ty: param.ty(), location: location_id });
            builder.semantics.introduce((&param).into(), var);
            var
        })
        .collect_vec();

    builder.destructure_closure(&mut ctx, location_id, parameters[0], closure_info);
    let var_usage = generators::StructConstruct {
        inputs: vec![],
        ty: unit_ty(ctx.db.upcast()),
        location: location_id,
    }
    .add(&mut ctx, &mut builder.statements);
    builder.ret(&mut ctx, var_usage, location_id)?;
    let lowered_impl = FlatLowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks: ctx.blocks.build().unwrap(),
        signature: ctx.signature,
        parameters,
    };
    Ok(lowered_impl)
}

/// Adds the lowering for the closure function.
fn add_closure_call_function(
    encapsulated_ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprClosure,
    closure_info: &ClosureInfo,
    trait_id: cairo_lang_defs::ids::TraitId,
) -> Maybe<()> {
    let semantic_db: &dyn SemanticGroup = encapsulated_ctx.db.upcast();
    let closure_ty = extract_matches!(expr.ty.lookup_intern(semantic_db), TypeLongId::Closure);
    let expr_location = encapsulated_ctx.get_location(expr.stable_ptr.untyped());
    let parameters_ty = TypeLongId::Tuple(closure_ty.param_tys.clone()).intern(semantic_db);
    let concrete_trait = ConcreteTraitLongId {
        trait_id,
        generic_args: vec![
            GenericArgumentId::Type(expr.ty),
            GenericArgumentId::Type(parameters_ty),
        ],
    }
    .intern(semantic_db);
    let Ok(impl_id) = semantic::types::get_impl_at_context(
        semantic_db,
        encapsulated_ctx.variables.lookup_context.clone(),
        concrete_trait,
        None,
    ) else {
        // If the impl doesn't exist, there won't be a call to the call-function, so we don't need
        // to generate it.
        return Ok(());
    };
    if !matches!(impl_id.lookup_intern(semantic_db), ImplLongId::GeneratedImpl(_)) {
        // If the impl is not generated, we don't need to generate a lowering for it.
        return Ok(());
    }

    let trait_function: cairo_lang_defs::ids::TraitFunctionId = semantic_db
        .trait_function_by_name(trait_id, "call".into())
        .unwrap()
        .expect("Call function must exist for an Fn trait.");

    let generic_function =
        GenericFunctionId::Impl(ImplGenericFunctionId { impl_id, function: trait_function });
    let function = semantic::FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args: vec![] },
    }
    .intern(semantic_db);
    let function_with_body_id = FunctionWithBodyLongId::Generated {
        parent: encapsulated_ctx.semantic_function_id,
        key: GeneratedFunctionKey::TraitFunc(trait_function, closure_ty.wrapper_location),
    }
    .intern(encapsulated_ctx.db);
    let signature = Signature::from_semantic(
        encapsulated_ctx.db,
        semantic_db.concrete_function_signature(function)?,
    );

    let mut ctx = LoweringContext::new(encapsulated_ctx, function_with_body_id, signature)?;

    let root_block_id = alloc_empty_block(&mut ctx);
    let mut builder = BlockBuilder::root(&mut ctx, root_block_id);

    let info = ctx.db.core_info();
    let (closure_param_var_id, closure_var) = if trait_id == info.fn_once_trt {
        // If the closure is FnOnce, the closure is passed by value.
        let closure_param_var = ctx.new_var(VarRequest { ty: expr.ty, location: expr_location });
        let closure_var = VarUsage { var_id: closure_param_var, location: expr_location };
        (closure_param_var, closure_var)
    } else {
        // If the closure is Fn the closure argument will be a snapshot, so we need to desnap it.
        let closure_param_var = ctx.new_var(VarRequest {
            ty: wrap_in_snapshots(semantic_db, expr.ty, 1),
            location: expr_location,
        });

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
        builder.semantics.introduce(param.clone(), captured_vars[i]);
    }
    for (i, (param, _)) in closure_info.snapshots.iter().enumerate() {
        builder
            .snapped_semantics
            .insert(param.clone(), captured_vars[i + closure_info.members.len()]);
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
        builder.semantics.introduce((&parameter_as_member_path(param.clone())).into(), param_var);
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
        .unwrap_or_else(FlatBlocks::new_errored);

    let lowered = FlatLowered {
        diagnostics: ctx.diagnostics.build(),
        variables: ctx.variables.variables,
        blocks,
        signature: ctx.signature.clone(),
        parameters,
    };
    encapsulated_ctx.lowerings.insert(
        GeneratedFunctionKey::TraitFunc(trait_function, closure_ty.wrapper_location),
        lowered,
    );
    Ok(())
}

/// Lowers an expression of type [semantic::ExprClosure].
fn lower_expr_closure(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprClosure,
    expr_id: semantic::ExprId,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a closure expression: {:?}", expr.debug(&ctx.expr_formatter));

    let usage = ctx.usages.usages[&expr_id].clone();
    let capture_var_usage = builder.capture(ctx, usage.clone(), expr);
    let closure_variable = LoweredExpr::AtVariable(capture_var_usage);
    let closure_ty = extract_matches!(expr.ty.lookup_intern(ctx.db), TypeLongId::Closure);
    let _ = add_capture_destruct_impl(
        ctx,
        capture_var_usage,
        builder.semantics.closures.get(&capture_var_usage.var_id).unwrap(),
        closure_ty.wrapper_location,
    );
    add_closure_call_function(
        ctx,
        expr,
        builder.semantics.closures.get(&capture_var_usage.var_id).unwrap(),
        if ctx.variables[capture_var_usage.var_id].copyable.is_ok() {
            ctx.db.core_info().fn_trt
        } else {
            ctx.db.core_info().fn_once_trt
        },
    )
    .map_err(LoweringFlowError::Failed)?;

    Ok(closure_variable)
}

/// Lowers an expression of type [semantic::ExprPropagateError].
fn lower_expr_error_propagate(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprPropagateError,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
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
        variant: func_err_variant.clone(),
        location,
    }
    .add(ctx, &mut subscope_err.statements);
    subscope_err.ret(ctx, err_res, location).map_err(LoweringFlowError::Failed)?;
    let sealed_block_err = SealedBlockBuilder::Ends(block_err_id);

    // Merge blocks.
    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: ok_variant.concrete_enum_id,
        input: match_input,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(ok_variant.clone()),
                block_id: block_ok_id,
                var_ids: vec![expr_var],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(err_variant.clone()),
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
fn lower_optimized_extern_error_propagate(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    extern_enum: LoweredExprExternEnum,
    ok_variant: &semantic::ConcreteVariant,
    err_variant: &semantic::ConcreteVariant,
    func_err_variant: &semantic::ConcreteVariant,
    location: LocationId,
) -> LoweringResult<LoweredExpr> {
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
    let err_res = generators::EnumConstruct { input, variant: func_err_variant.clone(), location }
        .add(ctx, &mut subscope_err.statements);
    subscope_err.ret(ctx, err_res, location).map_err(LoweringFlowError::Failed)?;
    let sealed_block_err = SealedBlockBuilder::Ends(block_err_id);

    // Merge.
    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: extern_enum.inputs,
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(ok_variant.clone()),
                block_id: block_ok_id,
                var_ids: block_ok_input_vars,
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(err_variant.clone()),
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
fn match_extern_variant_arm_input_types(
    ctx: &mut LoweringContext<'_, '_>,
    ty: semantic::TypeId,
    extern_enum: &LoweredExprExternEnum,
) -> Vec<semantic::TypeId> {
    let variant_input_tys = extern_facade_return_tys(ctx, ty);
    let ref_tys = extern_enum.member_paths.iter().map(|ref_arg| ref_arg.ty());
    chain!(ref_tys, variant_input_tys.into_iter()).collect()
}

/// Binds input references and implicits when matching on extern functions.
fn match_extern_arm_ref_args_bind(
    ctx: &mut LoweringContext<'_, '_>,
    arm_inputs: &mut Vec<VariableId>,
    extern_enum: &LoweredExprExternEnum,
    subscope: &mut BlockBuilder,
) {
    let ref_outputs: Vec<_> = arm_inputs.drain(0..extern_enum.member_paths.len()).collect();
    // Bind the ref parameters.
    for (ref_arg, output_var) in zip_eq(&extern_enum.member_paths, ref_outputs) {
        subscope.update_ref(ctx, ref_arg, output_var);
    }
}

/// Lowers an expression of type [semantic::ExprAssignment].
fn lower_expr_assignment(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprAssignment,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
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
fn alloc_empty_block(ctx: &mut LoweringContext<'_, '_>) -> BlockId {
    ctx.blocks.alloc_empty()
}

/// Creates a new subscope of the given builder, with an empty block.
fn create_subscope(ctx: &mut LoweringContext<'_, '_>, builder: &BlockBuilder) -> BlockBuilder {
    builder.child_block_builder(alloc_empty_block(ctx))
}

/// Calls `.check_error_free()` and warns (in log) if there are errors.
fn check_error_free_or_warn(
    db: &dyn LoweringGroup,
    diagnostics: Diagnostics<SemanticDiagnostic>,
    semantic_function_id: defs::ids::FunctionWithBodyId,
    diagnostics_description: &str,
) -> Maybe<()> {
    let declaration_error_free = diagnostics.check_error_free();
    declaration_error_free.inspect_err(|_| {
        log::warn!(
            "Function `{function_path}` has semantic diagnostics in its \
             {diagnostics_description}:\n{diagnostics_format}",
            function_path = semantic_function_id.full_path(db.upcast()),
            diagnostics_format = diagnostics.format(db.upcast())
        );
    })
}
