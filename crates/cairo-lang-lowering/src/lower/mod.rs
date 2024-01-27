use std::vec;

use block_builder::BlockBuilder;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{extract_matches, try_extract_matches};
use itertools::{chain, izip, zip_eq, Itertools};
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use semantic::corelib::{
    core_felt252_ty, core_submodule, get_core_function_id, get_core_ty_by_name, get_function_id,
    never_ty, unit_ty, validate_literal,
};
use semantic::items::enm::SemanticEnumEx;
use semantic::items::structure::SemanticStructEx;
use semantic::literals::try_extract_minus_literal;
use semantic::types::{peel_snapshots, wrap_in_snapshots};
use semantic::{
    ConcreteTypeId, ExprFunctionCallArg, ExprId, ExprPropagateError, ExprVarMemberPath,
    GenericArgumentId, MatchArmSelector, Pattern, PatternEnumVariant, PatternId, TypeLongId,
    ValueSelectorArm,
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
use crate::blocks::FlatBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::{self, *};
use crate::ids::{
    FunctionLongId, FunctionWithBodyId, FunctionWithBodyLongId, GeneratedFunction, LocationId,
    SemanticFunctionIdEx, Signature,
};
use crate::lower::context::{LoweringResult, VarRequest};
use crate::lower::generators::StructDestructure;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchEnumValue, MatchExternInfo,
    MatchInfo, VarUsage, VariableId,
};

mod block_builder;
pub mod context;
mod external;
pub mod generators;
mod logical_op;
mod lower_if;
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
            match block_sealed {
                SealedBlockBuilder::GotoCallsite { mut builder, expr } => {
                    // Convert to a return.
                    let location = ctx.get_location(semantic_block.stable_ptr.untyped());
                    let var_usage = expr.unwrap_or_else(|| {
                        generators::StructConstruct {
                            inputs: vec![],
                            ty: unit_ty(ctx.db.upcast()),
                            location,
                        }
                        .add(&mut ctx, &mut builder.statements)
                    });
                    builder.ret(&mut ctx, var_usage, location)?;
                }
                SealedBlockBuilder::Ends(_) => {}
            }
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
    let condition = lower_expr_to_var_usage(ctx, builder, loop_expr.condition)?;
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
        match block_sealed {
            SealedBlockBuilder::GotoCallsite { mut builder, expr } => {
                // Convert to a return.
                let location = ctx.get_location(stable_ptr.untyped());
                let var_usage = expr.unwrap_or_else(|| {
                    generators::StructConstruct {
                        inputs: vec![],
                        ty: unit_ty(ctx.db.upcast()),
                        location,
                    }
                    .add(&mut ctx, &mut builder.statements)
                });
                builder.ret(&mut ctx, var_usage, location)?;
            }
            SealedBlockBuilder::Ends(_) => {}
        }

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
    if let Err(err) = validate_literal(ctx.db.upcast(), ty, value.clone()) {
        ctx.diagnostics.report(stable_ptr, LoweringDiagnosticKind::LiteralError(err));
    }
    let location = ctx.get_location(stable_ptr);
    let u256_ty = get_core_ty_by_name(ctx.db.upcast(), "u256".into(), vec![]);

    if ty == u256_ty {
        let u128_ty = get_core_ty_by_name(ctx.db.upcast(), "u128".into(), vec![]);

        let mask128 = BigInt::from(u128::MAX);
        let low = value & mask128;
        let high = value >> 128;
        let u256 = vec![low, high];

        return Ok(LoweredExpr::AtVariable(
            generators::StructConstruct {
                inputs: u256
                    .into_iter()
                    .map(|value| {
                        generators::Literal { value, ty: u128_ty, location }
                            .add(ctx, &mut builder.statements)
                    })
                    .collect(),
                ty: u256_ty,
                location,
            }
            .add(ctx, &mut builder.statements),
        ));
    }

    Ok(LoweredExpr::AtVariable(
        generators::Literal { value: value.clone(), ty, location }
            .add(ctx, &mut builder.statements),
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
        let chunk_usage = generators::Literal {
            value: BigInt::from_bytes_be(Sign::Plus, chunk),
            ty: bytes31_ty,
            location: ctx.get_location(expr_stable_ptr),
        }
        .add(ctx, &mut builder.statements);

        *data_array_usage = generators::Call {
            function: data_array_append_function,
            inputs: vec![*data_array_usage, chunk_usage],
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

    let pending_word_usage = generators::Literal {
        value: BigInt::from_bytes_be(Sign::Plus, pending_word_bytes),
        ty: felt252_ty,
        location: ctx.get_location(expr_stable_ptr),
    }
    .add(ctx, &mut builder.statements);

    let pending_word_len = expr.value.len() % 31;
    let pending_word_len_usage = generators::Literal {
        value: pending_word_len.into(),
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
    let const_expr =
        &ctx.db.constant_semantic_data(expr.constant_id).map_err(LoweringFlowError::Failed)?.value;
    let semantic::Expr::Literal(const_expr_literal) = const_expr else {
        panic!("Only literal constants are supported.");
    };
    lower_expr_literal(ctx, const_expr_literal, builder)
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

    let (ref_outputs, res) =
        perform_function_call(ctx, builder, expr.function, arg_inputs, ref_tys, expr.ty, location)?;

    // Rebind the ref variables.
    for (ref_arg, output_var) in zip_eq(ref_args_iter, ref_outputs) {
        builder.update_ref(ctx, ref_arg, output_var.var_id);
    }

    Ok(res)
}

/// Creates a LoweredExpr for a function call, taking into consideration external function facades:
/// For external functions, sometimes the high level signature doesn't exactly correspond to the
/// external function returned variables / branches.
fn perform_function_call(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    function: semantic::FunctionId,
    inputs: Vec<VarUsage>,
    extra_ret_tys: Vec<semantic::TypeId>,
    ret_ty: semantic::TypeId,
    location: LocationId,
) -> Result<(Vec<VarUsage>, LoweredExpr), LoweringFlowError> {
    // If the function is not extern, simply call it.
    if function.try_get_extern_function_id(ctx.db.upcast()).is_none() {
        let call_result = generators::Call {
            function: function.lowered(ctx.db),
            inputs,
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
    let ret_tys = extern_facade_return_tys(ctx, ret_ty);
    let call_result = generators::Call {
        function: function.lowered(ctx.db),
        inputs,
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

/// Information about the enum of a match statement. See [extract_concrete_enum].
struct ExtractedEnumDetails {
    concrete_enum_id: semantic::ConcreteEnumId,
    concrete_variants: Vec<semantic::ConcreteVariant>,
    n_snapshots: usize,
}

/// Extracts concrete enum and variants from a match expression. Assumes it is indeed a concrete
/// enum.
fn extract_concrete_enum(
    ctx: &mut LoweringContext<'_, '_>,
    matched_expr: &semantic::Expr,
) -> Result<ExtractedEnumDetails, LoweringFlowError> {
    let ty = matched_expr.ty();
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db.upcast(), ty);

    // Semantic model should have made sure the type is an enum.
    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else {
        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
            matched_expr.stable_ptr().untyped(),
            UnsupportedMatchedType(long_ty.format(ctx.db.upcast())),
        )));
    };
    let concrete_variants =
        ctx.db.concrete_enum_variants(concrete_enum_id).map_err(LoweringFlowError::Failed)?;

    Ok(ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots })
}

/// Extracts concrete enums and variants from a match expression on a tuple of enums.
fn extract_concrete_enum_tuple(
    ctx: &mut LoweringContext<'_, '_>,
    matched_expr: &semantic::Expr,
    types: &[semantic::TypeId],
) -> Result<Vec<ExtractedEnumDetails>, LoweringFlowError> {
    types
        .iter()
        .map(|ty| {
            let (n_snapshots, long_ty) = peel_snapshots(ctx.db.upcast(), *ty);
            let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics
                        .report(matched_expr.stable_ptr().untyped(), UnsupportedMatchedValueTuple),
                ));
            };
            let concrete_variants = ctx
                .db
                .concrete_enum_variants(concrete_enum_id)
                .map_err(LoweringFlowError::Failed)?;
            Ok(ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots })
        })
        .collect()
}

/// The arm and pattern indices of a pattern in a match arm with an or list.
#[derive(Debug, Clone)]
struct PatternPath {
    arm_index: usize,
    pattern_index: usize,
}

/// Returns an option containing the PatternPath of the underscore pattern, if it exists.
fn get_underscore_pattern_path(
    ctx: &mut LoweringContext<'_, '_>,
    arms: &[semantic::MatchArm],
) -> Option<PatternPath> {
    let otherwise_variant = arms
        .iter()
        .enumerate()
        .map(|(arm_index, arm)| {
            arm.patterns
                .iter()
                .position(|pattern| {
                    matches!(ctx.function_body.patterns[*pattern], semantic::Pattern::Otherwise(_))
                })
                .map(|pattern_index| PatternPath { arm_index, pattern_index })
        })
        .find(|option| option.is_some())??;

    for arm in arms.iter().skip(otherwise_variant.arm_index + 1) {
        for pattern in arm.patterns.iter() {
            let pattern = ctx.function_body.patterns[*pattern].clone();
            ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnreachableMatchArm);
        }
    }
    for pattern in
        arms[otherwise_variant.arm_index].patterns.iter().skip(otherwise_variant.pattern_index + 1)
    {
        let pattern = ctx.function_body.patterns[*pattern].clone();
        ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnreachableMatchArm);
    }

    Some(otherwise_variant)
}

/// Returns a map from variants to their corresponding pattern path in a match statement.
fn get_variant_to_arm_map<'a>(
    ctx: &mut LoweringContext<'_, '_>,
    arms: impl Iterator<Item = &'a semantic::MatchArm>,
    concrete_enum_id: semantic::ConcreteEnumId,
) -> LoweringResult<UnorderedHashMap<semantic::ConcreteVariant, PatternPath>> {
    let mut map = UnorderedHashMap::default();
    for (arm_index, arm) in arms.enumerate() {
        for (pattern_index, pattern) in arm.patterns.iter().enumerate() {
            let pattern = ctx.function_body.patterns[*pattern].clone();

            if let semantic::Pattern::Otherwise(_) = pattern {
                break;
            }

            let enum_pattern = try_extract_matches!(&pattern, semantic::Pattern::EnumVariant)
                .ok_or_else(|| {
                    LoweringFlowError::Failed(
                        ctx.diagnostics
                            .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotAVariant),
                    )
                })?
                .clone();

            if enum_pattern.variant.concrete_enum_id != concrete_enum_id {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics
                        .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotAVariant),
                ));
            }

            match map.entry(enum_pattern.variant.clone()) {
                std::collections::hash_map::Entry::Occupied(_) => {
                    ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnreachableMatchArm);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(PatternPath { arm_index, pattern_index });
                }
            };
        }
    }
    Ok(map)
}

/// Represents a path in a match tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
struct MatchingPath {
    /// The variants per member of the tuple matched until this point.
    variants: Vec<semantic::ConcreteVariant>,
}

/// A helper function for [get_variants_to_arm_map_tuple] Inserts the pattern path to the map for
/// each variants list it can match.
fn insert_tuple_path_patterns(
    ctx: &mut LoweringContext<'_, '_>,
    patterns: &[PatternId],
    pattern_path: &PatternPath,
    extracted_enums_details: &[ExtractedEnumDetails],
    mut path: MatchingPath,
    map: &mut UnorderedHashMap<MatchingPath, PatternPath>,
) -> LoweringResult<()> {
    let index = path.variants.len();

    // if the path is the same length as the tuple's patterns, we have reached the end of the path
    if index == patterns.len() {
        match map.entry(path) {
            std::collections::hash_map::Entry::Occupied(_) => {}
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(pattern_path.clone());
            }
        };
        return Ok(());
    }

    let pattern = ctx.function_body.patterns[patterns[index]].clone();

    match pattern {
        Pattern::EnumVariant(enum_pattern) => {
            if enum_pattern.variant.concrete_enum_id
                != extracted_enums_details[index].concrete_enum_id
            {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics
                        .report(enum_pattern.stable_ptr.untyped(), UnsupportedMatchArmNotAVariant),
                ));
            }
            path.variants.push(enum_pattern.variant);
            insert_tuple_path_patterns(
                ctx,
                patterns,
                pattern_path,
                extracted_enums_details,
                path,
                map,
            )
        }
        Pattern::Otherwise(_) => {
            extracted_enums_details[index].concrete_variants.iter().try_for_each(|variant| {
                // TODO(TomerStarkware): Remove the match on the variant options in this case if
                // there's no other conflicting arm.
                let mut path = path.clone();
                path.variants.push(variant.clone());
                insert_tuple_path_patterns(
                    ctx,
                    patterns,
                    pattern_path,
                    extracted_enums_details,
                    path,
                    map,
                )
            })
        }
        _ => Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotAVariant),
        )),
    }
}

/// Returns a map from a matching paths to their corresponding pattern path in a match statement.
fn get_variants_to_arm_map_tuple<'a>(
    ctx: &mut LoweringContext<'_, '_>,
    arms: impl Iterator<Item = &'a semantic::MatchArm>,
    extracted_enums_details: &[ExtractedEnumDetails],
) -> LoweringResult<UnorderedHashMap<MatchingPath, PatternPath>> {
    let mut map = UnorderedHashMap::default();
    for (arm_index, arm) in arms.enumerate() {
        for (pattern_index, pattern) in arm.patterns.iter().enumerate() {
            let pattern = ctx.function_body.patterns[*pattern].clone();
            if let semantic::Pattern::Otherwise(_) = pattern {
                break;
            }
            let patterns =
                try_extract_matches!(&pattern, semantic::Pattern::Tuple).ok_or_else(|| {
                    LoweringFlowError::Failed(
                        ctx.diagnostics
                            .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotAVariant),
                    )
                })?;

            let map_size = map.len();
            insert_tuple_path_patterns(
                ctx,
                &patterns.field_patterns,
                &PatternPath { arm_index, pattern_index },
                extracted_enums_details,
                MatchingPath::default(),
                &mut map,
            )?;
            if map.len() == map_size {
                ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnreachableMatchArm);
            }
        }
    }
    Ok(map)
}

/// Information needed to lower a match on tuple expression.
struct LoweringMatchTupleContext {
    /// The location of the match expression.
    match_location: LocationId,
    /// The index of the underscore pattern, if it exists.
    otherwise_variant: Option<PatternPath>,
    /// A map from variants vector to their corresponding pattern path.
    variants_map: UnorderedHashMap<MatchingPath, PatternPath>,
    /// The tuple's destructured inputs.
    match_inputs: Vec<VarUsage>,
    /// The number of snapshots of the tuple.
    n_snapshots_outer: usize,
    /// The current variants path.
    current_path: MatchingPath,
    /// The current variants' variable ids.
    current_var_ids: Vec<VariableId>,
}

/// Lowers the arm of a match on a tuple expression.
fn lower_tuple_match_arm(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    arms: &[semantic::MatchArm],
    match_tuple_ctx: &mut LoweringMatchTupleContext,
    leaves_builders: &mut Vec<MatchLeafBuilder>,
) -> LoweringResult<()> {
    let pattern_path = match_tuple_ctx
        .variants_map
        .get(&match_tuple_ctx.current_path)
        .or(match_tuple_ctx.otherwise_variant.as_ref())
        .ok_or_else(|| {
            LoweringFlowError::Failed(ctx.diagnostics.report_by_location(
                match_tuple_ctx.match_location.get(ctx.db),
                MissingMatchArm(format!(
                    "({})",
                    match_tuple_ctx.current_path.variants
                        .iter()
                        .map(|variant| variant.id.name(ctx.db.upcast()))
                        .join(", ")
                )),
            ))
        })?;
    let pattern = &arms[pattern_path.arm_index].patterns[pattern_path.pattern_index];
    let pattern = ctx.function_body.patterns[*pattern].clone();
    let patterns = try_extract_matches!(&pattern, semantic::Pattern::Tuple).ok_or_else(|| {
        LoweringFlowError::Failed(
            ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotATuple),
        )
    })?;
    let lowering_inner_pattern_result = patterns
        .field_patterns
        .iter()
        .enumerate()
        .map(|(index, pattern)| {
            let pattern = &ctx.function_body.patterns[*pattern];
            match pattern {
                Pattern::EnumVariant(PatternEnumVariant {
                    inner_pattern: Some(inner_pattern),
                    ..
                }) => {
                    let inner_pattern = ctx.function_body.patterns[*inner_pattern].clone();
                    let pattern_location = ctx.get_location(inner_pattern.stable_ptr().untyped());

                    let variant_expr = LoweredExpr::AtVariable(VarUsage {
                        var_id: match_tuple_ctx.current_var_ids[index],
                        location: pattern_location,
                    });

                    lower_single_pattern(ctx, &mut builder, inner_pattern, variant_expr)
                }
                Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                | Pattern::Otherwise(_) => Ok(()),
                _ => unreachable!(
                    "function `get_variant_to_arm_map` should have reported every other pattern \
                     type"
                ),
            }
        })
        .collect::<LoweringResult<Vec<_>>>()
        .map(|_| ());
    leaves_builders.push(MatchLeafBuilder {
        builder,
        arm_index: pattern_path.arm_index,
        lowerin_result: lowering_inner_pattern_result,
    });
    Ok(())
}

/// Lowers a full decision tree for a match on a tuple expression.
fn lower_full_match_tree(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    arms: &[semantic::MatchArm],
    match_tuple_ctx: &mut LoweringMatchTupleContext,
    extracted_enums_details: &[ExtractedEnumDetails],
    leaves_builders: &mut Vec<MatchLeafBuilder>,
) -> LoweringResult<MatchInfo> {
    let index = match_tuple_ctx.current_path.variants.len();
    let mut arm_var_ids = vec![];
    let block_ids = extracted_enums_details[index]
        .concrete_variants
        .iter()
        .map(|concrete_variant| {
            let mut subscope = create_subscope_with_bound_refs(ctx, builder);
            let block_id = subscope.block_id;
            let var_id = ctx.new_var(VarRequest {
                ty: wrap_in_snapshots(
                    ctx.db.upcast(),
                    concrete_variant.ty,
                    extracted_enums_details[index].n_snapshots + match_tuple_ctx.n_snapshots_outer,
                ),
                location: match_tuple_ctx.match_location,
            });
            arm_var_ids.push(vec![var_id]);

            match_tuple_ctx.current_path.variants.push(concrete_variant.clone());
            match_tuple_ctx.current_var_ids.push(var_id);
            let result = if index + 1 == extracted_enums_details.len() {
                lower_tuple_match_arm(ctx, subscope, arms, match_tuple_ctx, leaves_builders)
            } else {
                lower_full_match_tree(
                    ctx,
                    &mut subscope,
                    arms,
                    match_tuple_ctx,
                    extracted_enums_details,
                    leaves_builders,
                )
                .map(|match_info| {
                    subscope.finalize(ctx, FlatBlockEnd::Match { info: match_info });
                })
            }
            .map(|_| block_id);
            match_tuple_ctx.current_path.variants.pop();
            match_tuple_ctx.current_var_ids.pop();
            result
        })
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()?;
    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: extracted_enums_details[index].concrete_enum_id,
        input: match_tuple_ctx.match_inputs[index],
        arms: zip_eq(
            zip_eq(&extracted_enums_details[index].concrete_variants, block_ids),
            arm_var_ids,
        )
        .map(|((variant_id, block_id), var_ids)| MatchArm {
            arm_selector: MatchArmSelector::VariantId(variant_id.clone()),
            block_id,
            var_ids,
        })
        .collect(),
        location: match_tuple_ctx.match_location,
    });
    Ok(match_info)
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a tuple of
/// enums.
fn lower_expr_match_tuple(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    expr: LoweredExpr,
    matched_expr: &semantic::Expr,
    n_snapshots_outer: usize,
    types: &[semantic::TypeId],
    arms: &[semantic::MatchArm],
) -> LoweringResult<LoweredExpr> {
    let location = expr.location();
    let match_inputs_exprs = if let LoweredExpr::Tuple { exprs, .. } = expr {
        exprs
    } else {
        let reqs = types
            .iter()
            .map(|ty| VarRequest {
                ty: wrap_in_snapshots(ctx.db.upcast(), *ty, n_snapshots_outer),
                location,
            })
            .collect();
        generators::StructDestructure {
            input: expr.as_var_usage(ctx, builder)?.var_id,
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

    let match_inputs = match_inputs_exprs
        .into_iter()
        .map(|expr| expr.as_var_usage(ctx, builder))
        .collect::<LoweringResult<Vec<_>>>()?;
    let extracted_enums_details = extract_concrete_enum_tuple(ctx, matched_expr, types)?;

    let otherwise_variant = get_underscore_pattern_path(ctx, arms);

    let variants_map = get_variants_to_arm_map_tuple(
        ctx,
        arms.iter().take(
            otherwise_variant
                .as_ref()
                .map(|PatternPath { arm_index, .. }| *arm_index)
                .unwrap_or(arms.len()),
        ),
        extracted_enums_details.as_slice(),
    )?;

    let mut arms_vec = vec![];
    let mut match_tuple_ctx = LoweringMatchTupleContext {
        match_location: location,
        otherwise_variant,
        variants_map,
        match_inputs,
        n_snapshots_outer,
        current_path: MatchingPath::default(),
        current_var_ids: vec![],
    };
    let match_info = lower_full_match_tree(
        ctx,
        builder,
        arms,
        &mut match_tuple_ctx,
        &extracted_enums_details,
        &mut arms_vec,
    )?;
    let empty_match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id: extracted_enums_details[0].concrete_enum_id,
        input: match_tuple_ctx.match_inputs[0],
        arms: vec![],
        location,
    });
    let sealed_blocks = group_match_arms(ctx, empty_match_info, location, arms, arms_vec)?;

    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Lowers an expression of type [semantic::ExprMatch].
fn lower_expr_match(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, builder, expr.matched_expr)?;

    let matched_expr = ctx.function_body.exprs[expr.matched_expr].clone();
    let ty = matched_expr.ty();
    let (n_snapshots, long_type_id) = peel_snapshots(ctx.db.upcast(), ty);

    if let Some(types) = try_extract_matches!(long_type_id, TypeLongId::Tuple) {
        return lower_expr_match_tuple(
            ctx,
            builder,
            lowered_expr,
            &matched_expr,
            n_snapshots,
            &types,
            &expr.arms,
        );
    }

    if ty == ctx.db.core_felt252_ty() {
        let match_input = lowered_expr.as_var_usage(ctx, builder)?;
        return lower_expr_match_felt252(ctx, expr, match_input, builder);
    }
    if let Some(convert_function) =
        corelib::get_convert_to_felt252_libfunc_name_by_type(ctx.db.upcast(), ty)
    {
        let match_input = lowered_expr.as_var_usage(ctx, builder)?;
        let ret_ty = corelib::core_felt252_ty(ctx.db.upcast());
        let call_result = generators::Call {
            function: convert_function.lowered(ctx.db),
            inputs: vec![match_input],
            extra_ret_tys: vec![],
            ret_tys: vec![ret_ty],
            location,
        }
        .add(ctx, &mut builder.statements);

        return lower_expr_match_felt252(
            ctx,
            expr,
            call_result.returns.into_iter().next().unwrap(),
            builder,
        );
    }

    // TODO(spapini): Use diagnostics.
    // TODO(spapini): Handle more than just enums.
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, builder, extern_enum, &expr.arms);
    }

    let ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots } =
        extract_concrete_enum(ctx, &matched_expr)?;
    let match_input = lowered_expr.as_var_usage(ctx, builder)?;

    // Merge arm blocks.
    let otherwise_variant = get_underscore_pattern_path(ctx, &expr.arms);
    let variant_map = get_variant_to_arm_map(
        ctx,
        expr.arms.iter().take(
            otherwise_variant
                .as_ref()
                .map(|PatternPath { arm_index, .. }| *arm_index)
                .unwrap_or(expr.arms.len()),
        ),
        concrete_enum_id,
    )?;

    let mut arm_var_ids = vec![];
    let mut block_ids = vec![];
    let varinats_block_builders = concrete_variants
        .iter()
        .map(|concrete_variant| {
            let PatternPath { arm_index, pattern_index } = variant_map
                .get(concrete_variant)
                .or(otherwise_variant.as_ref())
                .ok_or_else(|| {
                    LoweringFlowError::Failed(ctx.diagnostics.report(
                        expr.stable_ptr.untyped(),
                        MissingMatchArm(format!("{}", concrete_variant.id.name(ctx.db.upcast()))),
                    ))
                })?;
            let arm = &expr.arms[*arm_index];

            let mut subscope = create_subscope(ctx, builder);

            let pattern = &ctx.function_body.patterns[arm.patterns[*pattern_index]];
            let block_id = subscope.block_id;
            block_ids.push(block_id);

            let lowering_inner_pattern_result = match pattern {
                Pattern::EnumVariant(PatternEnumVariant {
                    inner_pattern: Some(inner_pattern),
                    ..
                }) => {
                    let inner_pattern = ctx.function_body.patterns[*inner_pattern].clone();
                    let pattern_location = ctx.get_location(inner_pattern.stable_ptr().untyped());

                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location: pattern_location,
                    });
                    arm_var_ids.push(vec![var_id]);
                    let variant_expr =
                        LoweredExpr::AtVariable(VarUsage { var_id, location: pattern_location });

                    lower_single_pattern(ctx, &mut subscope, inner_pattern, variant_expr)
                }
                Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                | Pattern::Otherwise(_) => {
                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location: ctx.get_location(pattern.stable_ptr().untyped()),
                    });
                    arm_var_ids.push(vec![var_id]);
                    Ok(())
                }
                _ => unreachable!(
                    "function `get_variant_to_arm_map` should have reported every other pattern \
                     type"
                ),
            };
            Ok(MatchLeafBuilder {
                arm_index: *arm_index,
                lowerin_result: lowering_inner_pattern_result,
                builder: subscope,
            })
        })
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()?;

    let empty_match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: vec![],
        location,
    });

    let sealed_blocks =
        group_match_arms(ctx, empty_match_info, location, &expr.arms, varinats_block_builders)?;

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
            .map(|((variant_id, block_id), var_ids)| MatchArm {
                arm_selector: MatchArmSelector::VariantId(variant_id),
                block_id,
                var_ids,
            })
            .collect(),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Lowers a match expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_match(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    extern_enum: LoweredExprExternEnum,
    match_arms: &[semantic::MatchArm],
) -> LoweringResult<LoweredExpr> {
    log::trace!("Started lowering of an optimized extern match.");
    let location = extern_enum.location;
    let concrete_variants = ctx
        .db
        .concrete_enum_variants(extern_enum.concrete_enum_id)
        .map_err(LoweringFlowError::Failed)?;

    // Merge arm blocks.
    let otherwise_variant = get_underscore_pattern_path(ctx, match_arms);

    let variant_map = get_variant_to_arm_map(
        ctx,
        match_arms.iter().take(
            otherwise_variant
                .as_ref()
                .map(|PatternPath { arm_index, .. }| *arm_index)
                .unwrap_or(match_arms.len()),
        ),
        extern_enum.concrete_enum_id,
    )?;
    let mut arm_var_ids = vec![];
    let mut block_ids = vec![];

    let varinats_block_builders = concrete_variants
        .iter()
        .map(|concrete_variant| {
            let mut subscope = create_subscope(ctx, builder);
            let block_id = subscope.block_id;
            block_ids.push(block_id);

            let input_tys =
                match_extern_variant_arm_input_types(ctx, concrete_variant.ty, &extern_enum);
            let mut input_vars = input_tys
                .into_iter()
                .map(|ty| ctx.new_var(VarRequest { ty, location }))
                .collect_vec();
            arm_var_ids.push(input_vars.clone());

            // Bind the arm inputs to implicits and semantic variables.
            match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope);

            let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, input_vars, location);

            let PatternPath { arm_index, pattern_index } = variant_map
                .get(concrete_variant)
                .or(otherwise_variant.as_ref())
                .ok_or_else(|| {
                    LoweringFlowError::Failed(ctx.diagnostics.report_by_location(
                        location.get(ctx.db),
                        MissingMatchArm(format!("{}", concrete_variant.id.name(ctx.db.upcast()))),
                    ))
                })?;

            let arm = &match_arms[*arm_index];
            let pattern = &ctx.function_body.patterns[arm.patterns[*pattern_index]];

            let lowering_inner_pattern_result = match pattern {
                Pattern::EnumVariant(PatternEnumVariant {
                    inner_pattern: Some(inner_pattern),
                    ..
                }) => lower_single_pattern(
                    ctx,
                    &mut subscope,
                    ctx.function_body.patterns[*inner_pattern].clone(),
                    variant_expr,
                ),
                Pattern::EnumVariant(PatternEnumVariant { inner_pattern: None, .. })
                | Pattern::Otherwise(_) => Ok(()),
                _ => unreachable!(
                    "function `get_variant_to_arm_map` should have reported every other pattern \
                     type"
                ),
            };
            Ok(MatchLeafBuilder {
                arm_index: *arm_index,
                lowerin_result: lowering_inner_pattern_result,
                builder: subscope,
            })
        })
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<LoweringResult<Vec<_>>>()?;

    let empty_match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: vec![],
        arms: vec![],
        location,
    });
    let sealed_blocks =
        group_match_arms(ctx, empty_match_info, location, match_arms, varinats_block_builders)?;
    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: extern_enum.inputs,
        arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
            .map(|((variant_id, block_id), var_ids)| MatchArm {
                arm_selector: MatchArmSelector::VariantId(variant_id),
                block_id,
                var_ids,
            })
            .collect(),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Represents a leaf in match tree, with the arm index it belongs to.
struct MatchLeafBuilder {
    arm_index: usize,
    lowerin_result: LoweringResult<()>,
    builder: BlockBuilder,
}
/// Groups match arms of different variants to their corresponding arms blocks and lowers
/// the arms expression.
fn group_match_arms(
    ctx: &mut LoweringContext<'_, '_>,
    empty_match_info: MatchInfo,
    location: LocationId,
    arms: &[semantic::MatchArm],
    varinats_block_builders: Vec<MatchLeafBuilder>,
) -> LoweringResult<Vec<SealedBlockBuilder>> {
    varinats_block_builders
        .into_iter()
        .sorted_by_key(|MatchLeafBuilder { arm_index, .. }| *arm_index)
        .group_by(|MatchLeafBuilder { arm_index, .. }| *arm_index)
        .into_iter()
        .map(|(arm_index, group)| {
            let arm = &arms[arm_index];
            let mut lowering_inner_pattern_results_and_subscopes = group
                .map(|MatchLeafBuilder { lowerin_result, builder, .. }| (lowerin_result, builder))
                .collect::<Vec<_>>();

            // If the arm has only one pattern, there is no need to create a parent scope.
            if lowering_inner_pattern_results_and_subscopes.len() == 1 {
                let (lowering_inner_pattern_result, subscope) =
                    lowering_inner_pattern_results_and_subscopes.pop().unwrap();

                return match lowering_inner_pattern_result {
                    Ok(_) => {
                        // Lower the arm expression.
                        lower_tail_expr(ctx, subscope, arm.expression)
                    }
                    Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
                }
                .map_err(LoweringFlowError::Failed);
            }

            // A parent block builder where the the variables of each pattern are introduced.
            // The parent block should have the same semantics and changed_member_paths as any of
            // the child blocks.
            let mut outer_subscope = lowering_inner_pattern_results_and_subscopes[0]
                .1
                .sibling_block_builder(alloc_empty_block(ctx));

            let sealed_blocks: Vec<_> = lowering_inner_pattern_results_and_subscopes
                .into_iter()
                .map(|(lowering_inner_pattern_result, subscope)| {
                    // Use the first pattern for the location of the for variable assignment block.
                    let pattern = &ctx.function_body.patterns[arm.patterns[0]];
                    match lowering_inner_pattern_result {
                        Ok(_) => lowered_expr_to_block_scope_end(
                            ctx,
                            subscope,
                            Ok(LoweredExpr::Tuple {
                                exprs: vec![],
                                location: ctx.get_location(pattern.stable_ptr().untyped()),
                            }),
                        ),
                        Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
                    }
                    .map_err(LoweringFlowError::Failed)
                })
                .collect::<LoweringResult<Vec<_>>>()?;

            outer_subscope.merge_and_end_with_match(
                ctx,
                empty_match_info.clone(),
                sealed_blocks,
                location,
            )?;
            lower_tail_expr(ctx, outer_subscope, arm.expression).map_err(LoweringFlowError::Failed)
        })
        .collect()
}

/// Lowers the [semantic::MatchArm] of an expression of type [semantic::ExprMatch] where the matched
/// expression is a felt252.
fn lower_expr_felt252_arm(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    builder: &mut BlockBuilder,
    arm_index: usize,
    pattern_index: usize,
    branches_block_builders: &mut Vec<MatchLeafBuilder>,
) -> LoweringResult<MatchInfo> {
    if pattern_index == expr.arms[arm_index].patterns.len() {
        return lower_expr_felt252_arm(
            ctx,
            expr,
            match_input,
            builder,
            arm_index + 1,
            0,
            branches_block_builders,
        );
    }

    let location = ctx.get_location(expr.stable_ptr.untyped());
    let arm = &expr.arms[arm_index];
    let semantic_db = ctx.db.upcast();

    let main_block = create_subscope_with_bound_refs(ctx, builder);
    let main_block_id = main_block.block_id;

    let mut else_block = create_subscope_with_bound_refs(ctx, builder);
    let block_else_id = else_block.block_id;

    let pattern = &ctx.function_body.patterns[arm.patterns[pattern_index]];
    let semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. }) = pattern else {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotALiteral),
        ));
    };

    let if_input = if literal.value == 0.into() {
        match_input
    } else {
        let ret_ty = corelib::core_felt252_ty(ctx.db.upcast());
        // TODO(TomerStarkware): Use the same type of literal as the input, without the cast to
        // felt252.
        let lowered_arm_val = lower_expr_literal(
            ctx,
            &semantic::ExprLiteral {
                stable_ptr: literal.stable_ptr,
                value: literal.value.clone(),
                ty: ret_ty,
            },
            builder,
        )?
        .as_var_usage(ctx, builder)?;

        let call_result = generators::Call {
            function: corelib::felt252_sub(ctx.db.upcast()).lowered(ctx.db),
            inputs: vec![match_input, lowered_arm_val],
            extra_ret_tys: vec![],
            ret_tys: vec![ret_ty],
            location,
        }
        .add(ctx, &mut builder.statements);
        call_result.returns.into_iter().next().unwrap()
    };

    let non_zero_type =
        corelib::core_nonzero_ty(semantic_db, corelib::core_felt252_ty(semantic_db));
    let else_block_input_var_id = ctx.new_var(VarRequest { ty: non_zero_type, location });

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(semantic_db).lowered(ctx.db),
        inputs: vec![if_input],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_zero_variant(
                    semantic_db,
                )),
                block_id: main_block_id,
                var_ids: vec![],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::jump_nz_nonzero_variant(
                    semantic_db,
                )),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
        ],
        location,
    });
    branches_block_builders.push(MatchLeafBuilder {
        arm_index,
        lowerin_result: Ok(()),
        builder: main_block,
    });
    if pattern_index + 1 == expr.arms[arm_index].patterns.len() && arm_index == expr.arms.len() - 2
    {
        branches_block_builders.push(MatchLeafBuilder {
            arm_index: arm_index + 1,
            lowerin_result: Ok(()),
            builder: else_block,
        });
    } else {
        let match_info = lower_expr_felt252_arm(
            ctx,
            expr,
            match_input,
            &mut else_block,
            arm_index,
            pattern_index + 1,
            branches_block_builders,
        )?;

        // we can use finalize here because the else block is an inner block of the match expression
        // and does not have sibling block it goes to.
        else_block.finalize(ctx, FlatBlockEnd::Match { info: match_info });
    }
    Ok(match_info)
}

/// lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt252,
/// using an index enum.
fn lower_expr_match_index_enum(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    builder: &BlockBuilder,
    literals_to_arm_map: &UnorderedHashMap<usize, usize>,
    branches_block_builders: &mut Vec<MatchLeafBuilder>,
) -> LoweringResult<MatchInfo> {
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let semantic_db = ctx.db.upcast();
    let unit_type = unit_ty(semantic_db);
    let mut arm_var_ids = vec![];
    let mut block_ids = vec![];

    for index in 0..literals_to_arm_map.len() {
        let subscope = create_subscope_with_bound_refs(ctx, builder);
        let block_id = subscope.block_id;
        block_ids.push(block_id);

        let arm_index = literals_to_arm_map[&index];

        let var_id = ctx.new_var(VarRequest { ty: unit_type, location });
        arm_var_ids.push(vec![var_id]);

        // Lower the arm expression.
        branches_block_builders.push(MatchLeafBuilder {
            arm_index,
            lowerin_result: Ok(()),
            builder: subscope,
        });
    }

    let arms = zip_eq(block_ids, arm_var_ids)
        .enumerate()
        .map(|(value, (block_id, var_ids))| MatchArm {
            arm_selector: MatchArmSelector::Value(ValueSelectorArm { value }),
            block_id,
            var_ids,
        })
        .collect();
    let match_info = MatchInfo::Value(MatchEnumValue {
        num_of_arms: literals_to_arm_map.len(),
        arms,
        input: match_input,
        location,
    });
    Ok(match_info)
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt252.
/// using an index enum to create a jump table.
fn lower_expr_match_felt252(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match-felt252 expression.");
    if expr.arms.is_empty() {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(expr.stable_ptr.untyped(), NonExhaustiveMatchFelt252),
        ));
    }
    let mut max = 0;
    let mut literals_to_arm_map = UnorderedHashMap::new();
    let mut otherwise_exist = false;
    for (arm_index, arm) in expr.arms.iter().enumerate() {
        for pattern in arm.patterns.iter() {
            let pattern = &ctx.function_body.patterns[*pattern];
            if otherwise_exist {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnreachableMatchArm),
                ));
            }
            match pattern {
                semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. }) => {
                    let Some(literal) = literal.value.to_usize() else {
                        return Err(LoweringFlowError::Failed(
                            ctx.diagnostics.report(
                                expr.stable_ptr.untyped(),
                                UnsupportedMatchArmNonSequential,
                            ),
                        ));
                    };
                    if otherwise_exist || literals_to_arm_map.insert(literal, arm_index).is_some() {
                        return Err(LoweringFlowError::Failed(
                            ctx.diagnostics
                                .report(pattern.stable_ptr().untyped(), UnreachableMatchArm),
                        ));
                    }
                    if literal > max {
                        max = literal;
                    }
                }
                semantic::Pattern::Otherwise(_) => otherwise_exist = true,
                _ => {
                    return Err(LoweringFlowError::Failed(
                        ctx.diagnostics
                            .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotALiteral),
                    ));
                }
            }
        }
    }

    if !otherwise_exist {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(expr.stable_ptr.untyped(), NonExhaustiveMatchFelt252),
        ));
    }
    if max + 1 != literals_to_arm_map.len() {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(expr.stable_ptr.untyped(), UnsupportedMatchArmNonSequential),
        ));
    };
    let location = ctx.get_location(expr.stable_ptr.untyped());

    let mut arms_vec = vec![];

    let empty_match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(ctx.db.upcast()).lowered(ctx.db),
        inputs: vec![match_input],
        arms: vec![],
        location,
    });

    if max <= numeric_match_optimization_threshold(ctx) {
        let match_info =
            lower_expr_felt252_arm(ctx, expr, match_input, builder, 0, 0, &mut arms_vec)?;

        let sealed_blocks =
            group_match_arms(ctx, empty_match_info, location, &expr.arms, arms_vec)?;

        return builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location);
    }
    let semantic_db = ctx.db.upcast();

    let felt252_ty = core_felt252_ty(semantic_db);
    let bounded_int_ty = corelib::bounded_int_ty(semantic_db, 0.into(), max.into());

    let function_id =
        corelib::core_downcast(semantic_db, felt252_ty, bounded_int_ty).lowered(ctx.db);

    let in_range_block_input_var_id = ctx.new_var(VarRequest { ty: bounded_int_ty, location });

    let in_range_block = create_subscope_with_bound_refs(ctx, builder);
    let in_range_block_id = in_range_block.block_id;
    let inner_match_info = lower_expr_match_index_enum(
        ctx,
        expr,
        VarUsage { var_id: in_range_block_input_var_id, location: match_input.location },
        &in_range_block,
        &literals_to_arm_map,
        &mut arms_vec,
    )?;
    in_range_block.finalize(ctx, FlatBlockEnd::Match { info: inner_match_info });

    let otherwise_block = create_subscope_with_bound_refs(ctx, builder);
    let otherwise_block_id = otherwise_block.block_id;

    arms_vec.push(MatchLeafBuilder {
        arm_index: expr.arms.len() - 1,
        lowerin_result: Ok(()),
        builder: otherwise_block,
    });

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: function_id,
        inputs: vec![match_input],
        arms: vec![
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_some_variant(
                    semantic_db,
                    GenericArgumentId::Type(bounded_int_ty),
                )),
                block_id: in_range_block_id,
                var_ids: vec![in_range_block_input_var_id],
            },
            MatchArm {
                arm_selector: MatchArmSelector::VariantId(corelib::option_none_variant(
                    semantic_db,
                    GenericArgumentId::Type(bounded_int_ty),
                )),
                block_id: otherwise_block_id,
                var_ids: vec![],
            },
        ],
        location,
    });
    let sealed_blocks = group_match_arms(ctx, empty_match_info, location, &expr.arms, arms_vec)?;
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Returns the threshold for the number of arms for optimising numeric match expressions, by using
/// a jump table instead of an if-else construct.
fn numeric_match_optimization_threshold(ctx: &mut LoweringContext<'_, '_>) -> usize {
    // Use [usize::max] as the default value, so that the optimization is not used by default.
    // TODO(TomerStarkware): Set the default to be optimal on `sierra-minor-update` branch.
    ctx.db
        .get_flag(FlagId::new(ctx.db.upcast(), "numeric_match_optimization_min_arms_threshold"))
        .map(|flag| match *flag {
            Flag::NumericMatchOptimizationMinArmsThreshold(threshold) => threshold,
            _ => panic!("Wrong type flag `{flag:?}`."),
        })
        .unwrap_or(usize::MAX)
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
    let member_expr = UnorderedHashMap::<_, _>::from_iter(expr.members.iter().cloned());
    let mut base_members_usages = if members.len() != member_expr.len() {
        // Semantic model should have made sure base struct exist if some members are missing.
        let base_struct_usage = lower_expr_to_var_usage(ctx, builder, expr.base_struct.unwrap())?;

        StructDestructure {
            input: base_struct_usage.var_id,
            var_reqs: members
                .iter()
                .map(|(_, member)| VarRequest { ty: member.ty, location })
                .collect(),
        }
        .add(ctx, &mut builder.statements)
    } else {
        vec![]
    };
    base_members_usages.reverse();
    Ok(LoweredExpr::AtVariable(
        generators::StructConstruct {
            inputs: members
                .into_iter()
                .map(|(_, member)| {
                    if member_expr.contains_key(&member.id) {
                        base_members_usages.pop();
                        lower_expr_to_var_usage(ctx, builder, member_expr[&member.id])
                    } else {
                        Ok(VarUsage { var_id: base_members_usages.pop().unwrap(), location })
                    }
                })
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
