use block_builder::BlockBuilder;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{extract_matches, try_extract_matches};
use itertools::{chain, zip_eq, Itertools};
use num_bigint::{BigInt, Sign};
use semantic::corelib::{
    core_felt252_ty, core_submodule, get_core_function_id, get_core_ty_by_name, get_function_id,
    never_ty, unit_ty, validate_literal,
};
use semantic::items::enm::SemanticEnumEx;
use semantic::items::structure::SemanticStructEx;
use semantic::literals::try_extract_minus_literal;
use semantic::types::{peel_snapshots, wrap_in_snapshots};
use semantic::{
    ConcreteTypeId, ExprFunctionCallArg, ExprPropagateError, ExprVarMemberPath, GenericArgumentId,
    TypeLongId,
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
use crate::{
    BlockId, FlatLowered, MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo, VarUsage, VariableId,
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
    let main_lowering =
        lower_function(&mut encapsulating_ctx, function_id, signature.into(), block_expr_id)?;
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

/// Lowers a loop inner function into [FlatLowered].
/// Similar to `lower_function`, but adds a recursive call.
// TODO(spapini): Unite with `lower_function`.
pub fn lower_loop_function(
    encapsulating_ctx: &mut EncapsulatingLoweringContext<'_>,
    function_id: FunctionWithBodyId,
    signature: Signature,
    expr: &semantic::ExprLoop,
) -> Maybe<FlatLowered> {
    let mut ctx = LoweringContext::new(encapsulating_ctx, function_id, signature)?;
    ctx.current_loop_expr = Some(expr.clone());

    // Fetch body block expr.
    let semantic_block =
        extract_matches!(&ctx.function_body.exprs[expr.body], semantic::Expr::Block).clone();

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
        let block_expr = (|| {
            lower_expr_block(&mut ctx, &mut builder, &semantic_block)?;
            // Add recursive call.
            let signature = ctx.signature.clone();
            call_loop_func(&mut ctx, signature, &mut builder, expr)
        })();
        let block_sealed = lowered_expr_to_block_scope_end(&mut ctx, builder, block_expr)?;

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
                // Emit diagnostic fo the rest of the statements with unreachable.
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
            let loop_expr = ctx.current_loop_expr.clone().unwrap();
            let lowered_expr = call_loop_func(ctx, ctx.signature.clone(), builder, &loop_expr)?;
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
        semantic::Pattern::Literal(_) | semantic::Pattern::StringLiteral(_) => unreachable!(),
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
            let mut required_members = UnorderedHashMap::from_iter(
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
            for (var_id, (_, member)) in
                generator.add(ctx, &mut builder.statements).into_iter().zip(members.into_iter())
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
        semantic::Pattern::EnumVariant(_) => unreachable!(),
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
        semantic::Expr::Loop(expr) => lower_expr_loop(ctx, expr, builder),
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
    expr: &semantic::ExprLoop,
    builder: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    let usage = &ctx.block_usages.block_usages[expr.body];

    // Determine signature.
    let params = usage.usage.iter().map(|(_, expr)| expr.clone()).collect_vec();
    let extra_rets = usage.changes.iter().map(|(_, expr)| expr.clone()).collect_vec();

    let signature = Signature {
        params,
        extra_rets,
        return_type: expr.ty,
        implicits: vec![],
        panicable: ctx.signature.panicable,
    };

    // Get the function id.
    let function = ctx.db.intern_lowering_function_with_body(FunctionWithBodyLongId::Generated {
        parent: ctx.semantic_function_id,
        element: expr.body,
    });

    // Generate the function.
    let encapsulating_ctx = std::mem::take(&mut ctx.encapsulating_ctx).unwrap();
    let lowered = lower_loop_function(encapsulating_ctx, function, signature.clone(), expr)
        .map_err(LoweringFlowError::Failed)?;
    // TODO(spapini): Recursive call.
    encapsulating_ctx.lowerings.insert(expr.body, lowered);
    ctx.encapsulating_ctx = Some(encapsulating_ctx);
    ctx.current_loop_expr = Some(expr.clone());

    call_loop_func(ctx, signature, builder, expr)
}

/// Adds a call to an inner loop-generated function.
fn call_loop_func(
    ctx: &mut LoweringContext<'_, '_>,
    signature: Signature,
    builder: &mut BlockBuilder,
    expr: &semantic::ExprLoop,
) -> LoweringResult<LoweredExpr> {
    let stable_ptr = expr.stable_ptr.untyped();
    let location = ctx.get_location(expr.stable_ptr.untyped());

    // Call it.
    let function = ctx.db.intern_lowering_function(FunctionLongId::Generated(GeneratedFunction {
        parent: ctx.concrete_function_id.base_semantic_function(ctx.db),
        element: expr.body,
    }));
    let inputs = signature
        .params
        .into_iter()
        .map(|param| {
            builder.get_ref(ctx, &param).ok_or_else(|| {
                LoweringFlowError::Failed(ctx.diagnostics.report(stable_ptr, MemberPathLoop))
            })
        })
        .collect::<LoweringResult<Vec<_>>>()?;
    let extra_ret_tys = signature.extra_rets.iter().map(|path| path.ty()).collect_vec();
    let call_result =
        generators::Call { function, inputs, extra_ret_tys, ret_tys: vec![expr.ty], location }
            .add(ctx, &mut builder.statements);

    // Rebind the ref variables.
    for (ref_arg, output_var) in zip_eq(&signature.extra_rets, call_result.extra_outputs) {
        builder.update_ref(ctx, ref_arg, output_var.var_id);
    }

    Ok(LoweredExpr::AtVariable(call_result.returns.into_iter().next().unwrap()))
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

    if ctx.function_body.exprs[expr.matched_expr].ty() == ctx.db.core_felt252_ty() {
        let match_input = lowered_expr.as_var_usage(ctx, builder)?;
        return lower_expr_match_felt252(ctx, expr, match_input, builder);
    }

    // TODO(spapini): Use diagnostics.
    // TODO(spapini): Handle more than just enums.
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, builder, extern_enum, &expr.arms);
    }

    let ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots } =
        extract_concrete_enum(ctx, expr)?;
    let match_input = lowered_expr.as_var_usage(ctx, builder)?;

    // Merge arm blocks.

    let mut arm_var_ids = vec![];
    let (sealed_blocks, block_ids): (Vec<_>, Vec<_>) = zip_eq(&concrete_variants, &expr.arms)
        .map(|(concrete_variant, arm)| {
            let mut subscope = create_subscope_with_bound_refs(ctx, builder);
            let block_id = subscope.block_id;
            let pattern = ctx.function_body.patterns[arm.pattern].clone();

            let enum_pattern = try_extract_matches!(&pattern, semantic::Pattern::EnumVariant)
                .ok_or_else(|| {
                    LoweringFlowError::Failed(
                        ctx.diagnostics
                            .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotAVariant),
                    )
                })?;
            if &enum_pattern.variant != concrete_variant {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics
                        .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmOutOfOrder),
                ));
            }

            let lowering_inner_pattern_result = match &enum_pattern.inner_pattern {
                Some(inner_pattern) => {
                    let inner_pattern = ctx.function_body.patterns[*inner_pattern].clone();
                    let pattern_location = ctx.get_location(inner_pattern.stable_ptr().untyped());

                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location: pattern_location,
                    });
                    arm_var_ids.push(vec![var_id]);
                    let variant_expr =
                        LoweredExpr::AtVariable(VarUsage { var_id, location: pattern_location });

                    lower_single_pattern(ctx, &mut subscope, inner_pattern.clone(), variant_expr)
                }
                None => {
                    let var_id = ctx.new_var(VarRequest {
                        ty: wrap_in_snapshots(ctx.db.upcast(), concrete_variant.ty, n_snapshots),
                        location: ctx.get_location(enum_pattern.stable_ptr.untyped()),
                    });
                    arm_var_ids.push(vec![var_id]);
                    Ok(())
                }
            };

            match lowering_inner_pattern_result {
                Ok(_) => {
                    // Lower the arm expression.
                    lower_tail_expr(ctx, subscope, arm.expression)
                }
                Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
            }
            .map_err(LoweringFlowError::Failed)
            .map(|sb| (sb, block_id))
        })
        .collect::<LoweringResult<Vec<_>>>()?
        .into_iter()
        .unzip();

    let match_info = MatchInfo::Enum(MatchEnumInfo {
        concrete_enum_id,
        input: match_input,
        arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
            .map(|((variant_id, block_id), var_ids)| MatchArm { variant_id, block_id, var_ids })
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
    if match_arms.len() != concrete_variants.len() {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report_by_location(location.get(ctx.db), UnsupportedMatchArms),
        ));
    }
    // Merge arm blocks.
    let mut arm_var_ids = vec![];

    let (sealed_blocks, block_ids): (Vec<_>, Vec<_>) = zip_eq(&concrete_variants, match_arms)
        .map(|(concrete_variant, arm)| {
            let mut subscope = create_subscope(ctx, builder);
            let block_id = subscope.block_id;

            let input_tys =
                match_extern_variant_arm_input_types(ctx, concrete_variant.ty, &extern_enum);
            let mut input_vars = input_tys
                .into_iter()
                .map(|ty| ctx.new_var(VarRequest { ty, location }))
                .collect_vec();
            arm_var_ids.push(input_vars.clone());
            let pattern = ctx.function_body.patterns[arm.pattern].clone();

            let enum_pattern = try_extract_matches!(&pattern, semantic::Pattern::EnumVariant)
                .ok_or_else(|| {
                    LoweringFlowError::Failed(
                        ctx.diagnostics
                            .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmNotAVariant),
                    )
                })?;
            if &enum_pattern.variant != concrete_variant {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics
                        .report(pattern.stable_ptr().untyped(), UnsupportedMatchArmOutOfOrder),
                ));
            }

            // Bind the arm inputs to implicits and semantic variables.
            match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope);

            let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, input_vars, location);
            let lowering_inner_pattern_result = match &enum_pattern.inner_pattern {
                Some(inner_pattern) => lower_single_pattern(
                    ctx,
                    &mut subscope,
                    ctx.function_body.patterns[*inner_pattern].clone(),
                    variant_expr,
                ),
                None => Ok(()),
            };

            match lowering_inner_pattern_result {
                Ok(_) => {
                    // Lower the arm expression.
                    lower_tail_expr(ctx, subscope, arm.expression)
                }
                Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
            }
            .map_err(LoweringFlowError::Failed)
            .map(|sb| (sb, block_id))
        })
        .collect::<LoweringResult<Vec<_>>>()?
        .into_iter()
        .unzip();

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: extern_enum.function.lowered(ctx.db),
        inputs: extern_enum.inputs,
        arms: zip_eq(zip_eq(concrete_variants, block_ids), arm_var_ids)
            .map(|((variant_id, block_id), var_ids)| MatchArm { variant_id, block_id, var_ids })
            .collect(),
        location,
    });
    builder.merge_and_end_with_match(ctx, match_info, sealed_blocks, location)
}

/// Lowers the [semantic::MatchArm] of an expression of type [semantic::ExprMatch] where the matched
/// expression is a felt252 and returns a sealed block builder of the arm block.
fn lower_felt252_arm_block(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    mut builder: BlockBuilder,
    index: usize,
) -> Maybe<SealedBlockBuilder> {
    let arm = &expr.arms[index];
    if index == expr.arms.len() - 1 {
        return lower_tail_expr(ctx, builder, arm.expression);
    }
    let lowered = lower_expr_felt252_arm(ctx, expr, match_input, &mut builder, index);
    lowered_expr_to_block_scope_end(ctx, builder, lowered)
}
/// Lowers the [semantic::MatchArm] of an expression of type [semantic::ExprMatch] where the matched
/// expression is a felt252.
fn lower_expr_felt252_arm(
    ctx: &mut LoweringContext<'_, '_>,
    expr: &semantic::ExprMatch,
    match_input: VarUsage,
    builder: &mut BlockBuilder,
    index: usize,
) -> LoweringResult<LoweredExpr> {
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let arm = &expr.arms[index];
    let semantic_db = ctx.db.upcast();

    let pattern = ctx.function_body.patterns[arm.pattern].clone();
    let semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. }) = pattern else {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(pattern.stable_ptr().untyped(), UnsupportedMatchedValue),
        ));
    };

    if literal.value != index.into() {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(literal.stable_ptr.untyped(), UnsupportedMatchArmNonSequential),
        ));
    }

    let if_input = if literal.value == 0.into() {
        match_input
    } else {
        let lowered_arm_val =
            lower_expr_literal(ctx, &literal, builder)?.as_var_usage(ctx, builder)?;
        let ret_ty = corelib::core_felt252_ty(ctx.db.upcast());
        let call_result = generators::Call {
            function: corelib::felt252_sub(ctx.db.upcast()).lowered(ctx.db),
            inputs: vec![lowered_arm_val, match_input],
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

    let block_id: BlockId = alloc_empty_block(ctx);
    let match_arm_content_block =
        lower_tail_expr(ctx, builder.child_block_builder(block_id), arm.expression)
            .map_err(LoweringFlowError::Failed)?;

    let else_block = create_subscope_with_bound_refs(ctx, builder);
    let block_else_id = else_block.block_id;

    let block_else = lower_felt252_arm_block(ctx, expr, match_input, else_block, index + 1)
        .map_err(LoweringFlowError::Failed)?;

    let match_info = MatchInfo::Extern(MatchExternInfo {
        function: corelib::core_felt252_is_zero(semantic_db).lowered(ctx.db),
        inputs: vec![if_input],
        arms: vec![
            MatchArm {
                variant_id: corelib::jump_nz_zero_variant(semantic_db),
                block_id,
                var_ids: vec![],
            },
            MatchArm {
                variant_id: corelib::jump_nz_nonzero_variant(semantic_db),
                block_id: block_else_id,
                var_ids: vec![else_block_input_var_id],
            },
        ],
        location,
    });
    builder.merge_and_end_with_match(
        ctx,
        match_info,
        vec![match_arm_content_block, block_else],
        location,
    )
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt252.
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
    let pattern = ctx.function_body.patterns[expr.arms.last().unwrap().pattern].clone();
    let semantic::Pattern::Otherwise(_) = pattern else {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(pattern.stable_ptr().untyped(), NonExhaustiveMatchFelt252),
        ));
    };
    lower_expr_felt252_arm(ctx, expr, match_input, builder, 0)
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
    expr: &semantic::ExprMatch,
) -> Result<ExtractedEnumDetails, LoweringFlowError> {
    let matched_expr = &ctx.function_body.exprs[expr.matched_expr];
    let ty = matched_expr.ty();
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db.upcast(), ty);

    // Semantic model should have made sure the type is an enum.
    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = long_ty else {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(matched_expr.stable_ptr().untyped(), UnsupportedMatchedValue),
        ));
    };
    let enum_id = concrete_enum_id.enum_id(ctx.db.upcast());
    let variants = ctx.db.enum_variants(enum_id).map_err(LoweringFlowError::Failed)?;
    let concrete_variants = variants
        .values()
        .map(|variant_id| {
            let variant =
                ctx.db.variant_semantic(enum_id, *variant_id).map_err(LoweringFlowError::Failed)?;

            ctx.db
                .concrete_enum_variant(concrete_enum_id, &variant)
                .map_err(LoweringFlowError::Failed)
        })
        .collect::<Result<Vec<_>, _>>()?;

    if expr.arms.len() != concrete_variants.len() {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(expr.stable_ptr.untyped(), UnsupportedMatchArms),
        ));
    }

    Ok(ExtractedEnumDetails { concrete_enum_id, concrete_variants, n_snapshots })
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
    let member_expr = UnorderedHashMap::from_iter(expr.members.iter().cloned());
    Ok(LoweredExpr::AtVariable(
        generators::StructConstruct {
            inputs: members
                .into_iter()
                .map(|(_, member)| lower_expr_to_var_usage(ctx, builder, member_expr[&member.id]))
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
                variant_id: ok_variant.clone(),
                block_id: block_ok_id,
                var_ids: vec![expr_var],
            },
            MatchArm {
                variant_id: err_variant.clone(),
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
                variant_id: ok_variant.clone(),
                block_id: block_ok_id,
                var_ids: block_ok_input_vars,
            },
            MatchArm {
                variant_id: err_variant.clone(),
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
