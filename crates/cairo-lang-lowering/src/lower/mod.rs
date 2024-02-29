use std::sync::Arc;
use std::vec;

use block_builder::BlockBuilder;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::{Entry, UnorderedHashMap};
use cairo_lang_utils::{extract_matches, try_extract_matches};
use defs::ids::{ConstantId, LanguageElementId, NamedLanguageElementId};
use id_arena::Arena;
use itertools::{chain, izip, zip_eq, Itertools};
use num_bigint::{BigInt, Sign};
use num_traits::{Num, Zero};
use semantic::corelib::{
    core_felt252_ty, core_submodule, get_core_function_id, get_core_ty_by_name, get_function_id,
    never_ty, try_extract_nz_wrapped_type, unit_ty, validate_literal,
};
use semantic::items::functions::GenericFunctionId;
use semantic::items::structure::SemanticStructEx;
use semantic::literals::try_extract_minus_literal;
use semantic::types::{peel_snapshots, wrap_in_snapshots};
use semantic::{
    ExprFunctionCallArg, ExprId, ExprPropagateError, ExprVarMemberPath, GenericArgumentId,
    MatchArmSelector, TypeLongId,
};
use {cairo_lang_defs as defs, cairo_lang_semantic as semantic};

use self::block_builder::SealedBlockBuilder;
use self::context::{
    lowering_flow_error_to_sealed_block, EncapsulatingLoweringContext, LoweredExpr,
    LoweredExprExternEnum, LoweringContext, LoweringFlowError,
};
use self::external::{extern_facade_expr, extern_facade_return_tys};
use self::logical_op::lower_logical_op;
use self::lower_if::lower_expr_if;
use self::lower_match::lower_expr_match;
use crate::blocks::FlatBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::{self, *};
use crate::diagnostic::{LoweringDiagnostics, MatchDiagnostic, MatchError, MatchKind};
use crate::ids::{
    FunctionLongId, FunctionWithBodyId, FunctionWithBodyLongId, GeneratedFunction, LocationId,
    SemanticFunctionIdEx, Signature,
};
use crate::lower::context::{LoweringResult, VarRequest};
use crate::lower::generators::StructDestructure;
use crate::lower::lower_match::{
    lower_concrete_enum_match, lower_expr_match_tuple, lower_optimized_extern_match,
    MatchArmWrapper, TupleInfo,
};
use crate::{
    BlockId, ConstValue, FlatLowered, LoweredConstant, MatchArm, MatchEnumInfo, MatchExternInfo,
    MatchInfo, VarUsage, VariableId,
};

mod block_builder;
pub mod context;
mod external;
pub mod generators;
mod logical_op;
mod lower_if;
mod lower_match;
pub mod refs;
pub mod usage;

#[cfg(test)]
mod generated_test;

/// Lowering of a function together with extra generated functions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MultiLowering {
    pub main_lowering: FlatLowered,
    pub generated_lowerings: OrderedHashMap<semantic::ExprId, FlatLowered>,
}

/// Lowers a semantic free function.
pub fn lower_semantic_function(
    db: &dyn LoweringGroup,
    semantic_function_id: defs::ids::FunctionWithBodyId,
) -> Maybe<MultiLowering> {
    db.function_declaration_diagnostics(semantic_function_id)
        .check_error_free()
        .and_then(|()| db.function_body_diagnostics(semantic_function_id).check_error_free())?;
    let mut encapsulating_ctx = EncapsulatingLoweringContext::new(db, semantic_function_id)?;
    let function_id = db
        .intern_lowering_function_with_body(FunctionWithBodyLongId::Semantic(semantic_function_id));
    let signature = db.function_with_body_signature(semantic_function_id)?;

    // TODO(spapini): Build semantic_defs in semantic model.
    for semantic_var in &signature.params {
        encapsulating_ctx.semantic_defs.insert(
            semantic::VarId::Param(semantic_var.id),
            semantic::Variable::Param(semantic_var.clone()),
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
        extract_matches!(&ctx.function_body.exprs[block_expr_id], semantic::Expr::Block).clone();

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
    let mut subscope_main = create_subscope_with_bound_refs(ctx, builder);
    let block_main_id = subscope_main.block_id;
    let main_block =
        extract_matches!(&ctx.function_body.exprs[loop_expr.body], semantic::Expr::Block).clone();
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
    let subscope_else = create_subscope_with_bound_refs(ctx, builder);
    let block_else_id = subscope_else.block_id;
    let else_block_input_var_id = ctx.new_var(VarRequest { ty: unit_ty, location: while_location });
    let block_else = lowered_expr_to_block_scope_end(
        ctx,
        subscope_else,
        Ok(LoweredExpr::Tuple { exprs: vec![], location: while_location }),
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

    let matched_expr = ctx.function_body.exprs[matched_expr].clone();
    let ty = matched_expr.ty();

    if ty == ctx.db.core_felt252_ty()
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
    ctx.current_loop_expr_id = Some(loop_expr_id);

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
            builder.semantics.introduce((&param).into(), var);
            var
        })
        .collect_vec();

    let root_ok = (|| {
        let (block_expr, stable_ptr) = match ctx.function_body.exprs[loop_expr_id].clone() {
            semantic::Expr::Loop(semantic::ExprLoop { body, stable_ptr, .. }) => {
                // Fetch body block expr.
                let semantic_block =
                    extract_matches!(&ctx.function_body.exprs[body], semantic::Expr::Block).clone();

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
            _ => unreachable!("Loop expression must be either loop or while."),
        };

        let block_sealed = lowered_expr_to_block_scope_end(&mut ctx, builder, block_expr)?;
        wrap_sealed_block_as_function(&mut ctx, block_sealed, stable_ptr.untyped())?;

        Ok(root_block_id)
    })();

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
                ctx.db.lookup_intern_type(ctx.variables[expr.var_id].ty)
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
        let stmt = ctx.function_body.statements[*stmt_id].clone();
        let Err(err) = lower_statement(ctx, builder, &stmt) else {
            continue;
        };
        if err.is_unreachable() {
            // If flow is not reachable anymore, no need to continue emitting statements.
            // TODO(spapini): We might want to report unreachable for expr that abruptly
            // ends, e.g. `5 + {return; 6}`.
            if i + 1 < expr_block.statements.len() {
                let start_stmt = &ctx.function_body.statements[expr_block.statements[i + 1]];
                let end_stmt =
                    &ctx.function_body.statements[*expr_block.statements.last().unwrap()];
                // Emit diagnostic for the rest of the statements with unreachable.
                ctx.diagnostics.report(
                    start_stmt.stable_ptr().untyped(),
                    Unreachable { last_statement_ptr: end_stmt.stable_ptr().untyped() },
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
            let pattern = ctx.function_body.patterns[*pattern].clone();
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
                ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnsupportedPattern),
            ));
        }
        semantic::Pattern::Variable(semantic::PatternVariable {
            name: _,
            var: sem_var,
            stable_ptr,
        }) => {
            let sem_var = semantic::Variable::Local(sem_var);
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
                input: lowered_expr.as_var_usage(ctx, builder)?.var_id,
                var_reqs: members
                    .iter()
                    .map(|(_, member)| VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), member.ty, structure.n_snapshots),
                        location: ctx.get_location(
                            required_members
                                .get(&member.id)
                                .map(|pattern| {
                                    ctx.function_body.patterns[**pattern].stable_ptr().untyped()
                                })
                                .unwrap_or_else(|| structure.stable_ptr.untyped()),
                        ),
                    })
                    .collect(),
            };
            for (var_id, (_, member)) in izip!(generator.add(ctx, &mut builder.statements), members)
            {
                if let Some(member_pattern) = required_members.remove(&member.id) {
                    let member_pattern = ctx.function_body.patterns[*member_pattern].clone();
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
        semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty, .. }) => {
            let outputs = if let LoweredExpr::Tuple { exprs, .. } = lowered_expr {
                exprs
            } else {
                let (n_snapshots, long_type_id) = peel_snapshots(ctx.db.upcast(), ty);
                let reqs =
                    zip_eq(&field_patterns, extract_matches!(long_type_id, TypeLongId::Tuple))
                        .map(|(pattern, ty)| VarRequest {
                            ty: wrap_in_snapshots(ctx.db.upcast(), ty, n_snapshots),
                            location: ctx.get_location(
                                ctx.function_body.patterns[*pattern].stable_ptr().untyped(),
                            ),
                        })
                        .collect();
                generators::StructDestructure {
                    input: lowered_expr.as_var_usage(ctx, builder)?.var_id,
                    var_reqs: reqs,
                }
                .add(ctx, &mut builder.statements)
                .into_iter()
                .map(|var_id| {
                    LoweredExpr::AtVariable(VarUsage {
                        var_id,
                        // The variable is used immediately after the destructure, so the usage
                        // location is the same as the definition location.
                        location: ctx.variables[var_id].location,
                    })
                })
                .collect()
            };
            for (var, pattern) in zip_eq(outputs, field_patterns) {
                lower_single_pattern(
                    ctx,
                    builder,
                    ctx.function_body.patterns[pattern].clone(),
                    var,
                )?;
            }
        }
        semantic::Pattern::Otherwise(_) => {}
        semantic::Pattern::Missing(_) => unreachable!("Missing pattern in semantic model."),
        semantic::Pattern::FixedSizeArray(_) => todo!(),
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
    let expr = ctx.function_body.exprs[expr_id].clone();
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
        semantic::Expr::Loop(_) | semantic::Expr::While(_) => {
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
    let value = value_as_const_value(ctx.db, stable_ptr, ty, value, &mut ctx.diagnostics);
    let location = ctx.get_location(stable_ptr);
    Ok(LoweredExpr::AtVariable(
        generators::Const { value, ty, location }.add(ctx, &mut builder.statements),
    ))
}

fn value_as_const_value(
    db: &dyn LoweringGroup,
    stable_ptr: SyntaxStablePtrId,
    ty: semantic::TypeId,
    value: &BigInt,
    diagnostics: &mut LoweringDiagnostics,
) -> ConstValue {
    if let Err(err) = validate_literal(db.upcast(), ty, value.clone()) {
        diagnostics.report(stable_ptr, LoweringDiagnosticKind::LiteralError(err));
    }
    let get_basic_const_value = |ty| {
        let u256_ty = get_core_ty_by_name(db.upcast(), "u256".into(), vec![]);

        if ty != u256_ty {
            ConstValue::Int(value.clone())
        } else {
            let u128_ty = get_core_ty_by_name(db.upcast(), "u128".into(), vec![]);
            let mask128 = BigInt::from(u128::MAX);
            let low = value & mask128;
            let high = value >> 128;
            ConstValue::Struct(vec![
                (u128_ty, ConstValue::Int(low)),
                (u128_ty, ConstValue::Int(high)),
            ])
        }
    };

    if let Some(inner) = try_extract_nz_wrapped_type(db.upcast(), ty) {
        ConstValue::NonZero(inner, Box::new(get_basic_const_value(inner)))
    } else {
        get_basic_const_value(ty)
    }
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
    let data_array_new_function =
        ctx.db.intern_lowering_function(FunctionLongId::Semantic(get_function_id(
            semantic_db,
            array_submodule,
            "array_new".into(),
            vec![GenericArgumentId::Type(bytes31_ty)],
        )));
    let data_array_append_function =
        ctx.db.intern_lowering_function(FunctionLongId::Semantic(get_function_id(
            semantic_db,
            array_submodule,
            "array_append".into(),
            vec![GenericArgumentId::Type(bytes31_ty)],
        )));

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
            value: ConstValue::Int(BigInt::from_bytes_be(Sign::Plus, chunk)),
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

    let u32_ty = get_core_ty_by_name(ctx.db.upcast(), "u32".into(), vec![]);
    let felt252_ty = core_felt252_ty(ctx.db.upcast());

    let pending_word_usage = generators::Const {
        value: ConstValue::Int(BigInt::from_bytes_be(Sign::Plus, pending_word_bytes)),
        ty: felt252_ty,
        location: ctx.get_location(expr_stable_ptr),
    }
    .add(ctx, &mut builder.statements);

    let pending_word_len = expr.value.len() % 31;
    let pending_word_len_usage = generators::Const {
        value: ConstValue::Int(pending_word_len.into()),
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
    let lowered_constant =
        ctx.db.lowered_constant(expr.constant_id).map_err(LoweringFlowError::Failed)?;
    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::Const {
            value: lowered_constant.value.clone(),
            ty: lowered_constant.ty,
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
}

/// Lowers a constant expression.
pub fn lowered_constant(
    db: &dyn LoweringGroup,
    constant_id: ConstantId,
) -> Maybe<Arc<LoweredConstant>> {
    db.constant_semantic_diagnostics(constant_id).check_error_free()?;
    let constant = db.constant_semantic_data(constant_id).unwrap();
    let module_file_id = constant_id.module_file_id(db.upcast());
    let mut diagnostics = LoweringDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let (ty, value) =
        lowered_constant_helper(db, &constant.exprs, constant.value, &mut diagnostics);
    Ok(Arc::new(LoweredConstant { value, ty, diagnostics: diagnostics.build() }))
}

/// Helper function to recursively create a lowered constant.
fn lowered_constant_helper(
    db: &dyn LoweringGroup,
    exprs: &Arena<semantic::Expr>,
    expr_id: semantic::ExprId,
    diagnostics: &mut LoweringDiagnostics,
) -> (semantic::TypeId, ConstValue) {
    let expr = &exprs[expr_id];
    (
        expr.ty(),
        match expr {
            semantic::Expr::Constant(expr) => db
                .lowered_constant(expr.constant_id)
                .map(|v| v.value.clone())
                .unwrap_or(ConstValue::Missing),
            semantic::Expr::Block(semantic::ExprBlock {
                statements, tail: Some(inner), ..
            }) if statements.is_empty() => {
                lowered_constant_helper(db, exprs, *inner, diagnostics).1
            }
            semantic::Expr::FunctionCall(expr) => {
                if let Some(value) = evaluate_const_function_call(db, exprs, expr, diagnostics) {
                    value_as_const_value(
                        db,
                        expr.stable_ptr.untyped(),
                        expr.ty,
                        &value,
                        diagnostics,
                    )
                } else {
                    ConstValue::Missing
                }
            }
            semantic::Expr::Literal(expr) => value_as_const_value(
                db,
                expr.stable_ptr.untyped(),
                expr.ty,
                &expr.value,
                diagnostics,
            ),
            semantic::Expr::Tuple(expr) => ConstValue::Struct(
                expr.items
                    .iter()
                    .map(|expr_id| lowered_constant_helper(db, exprs, *expr_id, diagnostics))
                    .collect(),
            ),
            semantic::Expr::StructCtor(expr) => ConstValue::Struct(
                expr.members
                    .iter()
                    .map(|(_, expr_id)| lowered_constant_helper(db, exprs, *expr_id, diagnostics))
                    .collect(),
            ),
            semantic::Expr::EnumVariantCtor(expr) => ConstValue::Enum(
                expr.variant.clone(),
                Box::new(lowered_constant_helper(db, exprs, expr.value_expr, diagnostics).1),
            ),
            semantic::Expr::MemberAccess(expr) => {
                extract_const_member_access(db, exprs, expr, diagnostics)
                    .unwrap_or(ConstValue::Missing)
            }
            _ => panic!("Unexpected constant in lowering: {:?}.", expr),
        },
    )
}

/// Attempts to evaluate constants from a function call.
fn evaluate_const_function_call(
    db: &dyn LoweringGroup,
    exprs: &Arena<semantic::Expr>,
    expr: &semantic::ExprFunctionCall,
    diagnostics: &mut LoweringDiagnostics,
) -> Option<BigInt> {
    if let Some(value) = try_extract_minus_literal(db.upcast(), exprs, expr) {
        return Some(value);
    }
    let args = expr
        .args
        .iter()
        .map(|arg| {
            let arg = extract_matches!(arg, ExprFunctionCallArg::Value);
            match lowered_constant_helper(db, exprs, *arg, diagnostics).1 {
                ConstValue::Int(v) => Some(v),
                // Handling u256 constants to enable const evaluation of them.
                ConstValue::Struct(v) => {
                    if let [(_, ConstValue::Int(low)), (_, ConstValue::Int(high))] = &v[..] {
                        Some(low + (high << 128))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        })
        .collect::<Option<Vec<_>>>()?;
    // No need for validation of the function args and generic id - as we only allowed the specific
    // trait functions and validated them at the semantic level.
    let imp = extract_matches!(
        expr.function.get_concrete(db.upcast()).generic_function,
        GenericFunctionId::Impl
    );
    let is_felt252_ty = expr.ty == core_felt252_ty(db.upcast());
    let mut value = match imp.function.name(db.upcast()).as_str() {
        "neg" => -&args[0],
        "add" => &args[0] + &args[1],
        "sub" => &args[0] - &args[1],
        "mul" => &args[0] * &args[1],
        "div" | "rem" if args[1].is_zero() => {
            diagnostics.report(expr.stable_ptr.untyped(), LoweringDiagnosticKind::DivisionByZero);
            return None;
        }
        "div" if !is_felt252_ty => &args[0] / &args[1],
        "rem" if !is_felt252_ty => &args[0] % &args[1],
        "bitand" if !is_felt252_ty => &args[0] & &args[1],
        "bitor" if !is_felt252_ty => &args[0] | &args[1],
        "bitxor" if !is_felt252_ty => &args[0] ^ &args[1],
        _ => unreachable!("Unexpected function call in constant lowering: {:?}", expr),
    };
    if is_felt252_ty {
        // Specifically handling felt252s since their evaluation is more complex.
        value %= BigInt::from_str_radix(
            "800000000000011000000000000000000000000000000000000000000000001",
            16,
        )
        .unwrap();
    }
    Some(value)
}

/// Extract const member access from a const value.
fn extract_const_member_access(
    db: &dyn LoweringGroup,
    exprs: &Arena<semantic::Expr>,
    expr: &semantic::ExprMemberAccess,
    diagnostics: &mut LoweringDiagnostics,
) -> Option<ConstValue> {
    let full_struct = lowered_constant_helper(db, exprs, expr.expr, diagnostics).1;
    let mut values = try_extract_matches!(full_struct, ConstValue::Struct)?;
    let members = db.concrete_struct_members(expr.concrete_struct_id).ok()?;
    let member_idx = members.iter().position(|(_, member)| member.id == expr.member)?;
    Some(values.swap_remove(member_idx).1)
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
    let exprs = expr
        .items
        .iter()
        .map(|arg_expr_id| lower_expr(ctx, builder, *arg_expr_id))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(LoweredExpr::FixedSizeArray { exprs, location })
}

/// Lowers an expression of type [semantic::ExprSnapshot].
fn lower_expr_snapshot(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprSnapshot,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a snapshot: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let expr = Box::new(lower_expr(ctx, builder, expr.inner)?);
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
    if let Some(value) = try_extract_minus_literal(ctx.db.upcast(), &ctx.function_body.exprs, expr)
    {
        return lower_expr_literal_helper(ctx, expr.stable_ptr.untyped(), expr.ty, &value, builder);
    }
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
            ctx.db.lookup_intern_type(expr.ty)
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
                    extract_matches!(
                        ctx.db.lookup_intern_type(ret_ty),
                        semantic::TypeLongId::Concrete
                    ),
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
    let (stable_ptr, return_type) = match ctx.function_body.exprs[loop_expr_id] {
        semantic::Expr::Loop(semantic::ExprLoop { stable_ptr, ty, .. }) => (stable_ptr, ty),
        semantic::Expr::While(semantic::ExprWhile { stable_ptr, ty, .. }) => (stable_ptr, ty),
        _ => unreachable!("Loop expression must be either loop or while."),
    };

    let usage = &ctx.block_usages.block_usages[&loop_expr_id];

    // Determine signature.
    let params = usage.usage.iter().map(|(_, expr)| expr.clone()).collect_vec();
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
    let function = ctx.db.intern_lowering_function_with_body(FunctionWithBodyLongId::Generated {
        parent: ctx.semantic_function_id,
        element: loop_expr_id,
    });

    // Generate the function.
    let encapsulating_ctx = std::mem::take(&mut ctx.encapsulating_ctx).unwrap();
    let lowered =
        lower_loop_function(encapsulating_ctx, function, loop_signature.clone(), loop_expr_id)
            .map_err(LoweringFlowError::Failed)?;
    // TODO(spapini): Recursive call.
    encapsulating_ctx.lowerings.insert(loop_expr_id, lowered);

    ctx.encapsulating_ctx = Some(encapsulating_ctx);
    ctx.current_loop_expr_id = Some(loop_expr_id);

    call_loop_func(ctx, loop_signature, builder, loop_expr_id, stable_ptr.untyped())
}

/// Adds a call to an inner loop-generated function.
fn call_loop_func(
    ctx: &mut LoweringContext<'_, '_>,
    loop_signature: Signature,
    builder: &mut BlockBuilder,
    loop_expr_id: ExprId,
    stable_ptr: SyntaxStablePtrId,
) -> LoweringResult<LoweredExpr> {
    let location = ctx.get_location(stable_ptr);

    // Call it.
    let function = ctx.db.intern_lowering_function(FunctionLongId::Generated(GeneratedFunction {
        parent: ctx.concrete_function_id.base_semantic_function(ctx.db),
        element: loop_expr_id,
    }));
    let inputs = loop_signature
        .params
        .into_iter()
        .map(|param| {
            builder.get_ref(ctx, &param).ok_or_else(|| {
                LoweringFlowError::Failed(ctx.diagnostics.report(stable_ptr, MemberPathLoop))
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
                .into_iter()
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
        let base_struct_usage = lower_expr_to_var_usage(ctx, builder, expr.base_struct.unwrap())?;

        for (base_member, (_, member)) in izip!(
            StructDestructure {
                input: base_struct_usage.var_id,
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
    Ok(LoweredExpr::AtVariable(
        generators::StructConstruct {
            inputs: members
                .into_iter()
                .map(|(_, member)| member_expr_usages.remove(&member.id).unwrap())
                .collect::<Result<Vec<_>, _>>()?,
            ty: expr.ty,
            location,
        }
        .add(ctx, &mut builder.statements),
    ))
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
    let subscope_ok = create_subscope_with_bound_refs(ctx, builder);
    let block_ok_id = subscope_ok.block_id;
    let expr_var = ctx.new_var(VarRequest { ty: ok_variant.ty, location });
    let sealed_block_ok = subscope_ok.goto_callsite(Some(VarUsage { var_id: expr_var, location }));

    // Err arm.
    let mut subscope_err = create_subscope_with_bound_refs(ctx, builder);
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
fn create_subscope_with_bound_refs(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &BlockBuilder,
) -> BlockBuilder {
    builder.child_block_builder(alloc_empty_block(ctx))
}

/// Creates a new subscope of the given builder, with unchanged refs and with an empty block.
fn create_subscope(ctx: &mut LoweringContext<'_, '_>, builder: &BlockBuilder) -> BlockBuilder {
    builder.child_block_builder(alloc_empty_block(ctx))
}
