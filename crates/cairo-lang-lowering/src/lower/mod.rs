use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::{skip_diagnostic, DiagnosticAdded, Maybe, ToMaybe};
use cairo_lang_semantic as semantic;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{extract_matches, try_extract_matches};
use itertools::{chain, zip_eq};
use num_traits::Zero;
use scope::BlockBuilder;
use semantic::corelib::{
    core_felt_ty, core_jump_nz_func, core_nonzero_ty, get_core_function_id,
    jump_nz_nonzero_variant, jump_nz_zero_variant, unit_ty,
};
use semantic::items::enm::SemanticEnumEx;
use semantic::{ConcreteTypeId, ExprPropagateError, TypeLongId};

use self::context::{
    lowering_flow_error_to_sealed_block, LoweredExpr, LoweredExprExternEnum, LoweringContext,
    LoweringFlowError,
};
use self::external::{extern_facade_expr, extern_facade_return_tys};
use self::lower_if::lower_expr_if;
use self::scope::SealedBlockBuilder;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::lower::context::{LoweringContextBuilder, LoweringResult, VarRequest};
use crate::lower::scope::merge_sealed;
use crate::{Statement, StatementMatchEnum, StatementMatchExtern, StructuredLowered, VariableId};
pub mod generators;

pub mod context;
mod external;
pub mod implicits;
mod lower_if;
mod scope;

/// Lowers a semantic free function.
pub fn lower(db: &dyn LoweringGroup, function_id: FunctionWithBodyId) -> Maybe<StructuredLowered> {
    log::trace!("Lowering a free function.");
    let is_empty_semantic_diagnostics = db.function_declaration_diagnostics(function_id).is_empty()
        && db.function_body_diagnostics(function_id).is_empty();
    let function_def = db.function_body(function_id)?;
    let signature = db.function_with_body_signature(function_id)?;

    let implicits = db.function_with_body_all_implicits_vec(function_id)?;
    // Params.
    let input_semantic_vars: Vec<semantic::Variable> =
        signature.params.iter().cloned().map(semantic::Variable::Param).collect();

    let lowering_builder = LoweringContextBuilder::new(db, function_id)?;
    let mut ctx = lowering_builder.ctx()?;
    let signature_location = ctx.get_location(signature.stable_ptr.untyped());

    // TODO(spapini): Build semantic_defs in semantic model.
    for semantic_var in input_semantic_vars {
        ctx.semantic_defs.insert(semantic_var.id(), semantic_var);
    }

    // Fetch body block expr.
    let semantic_block =
        extract_matches!(&function_def.exprs[function_def.body_expr], semantic::Expr::Block);

    // Initialize scope.
    let mut scope = BlockBuilder::root(&ctx);
    for ty in &implicits {
        let var = scope.add_input(&mut ctx, VarRequest { ty: *ty, location: signature_location });
        scope.put_implicit(&mut ctx, *ty, var);
    }
    for param in ctx.signature.params.clone() {
        let location = ctx.get_location(param.stable_ptr.untyped());
        let semantic = semantic::Variable::Param(param);
        let var = scope.add_input(&mut ctx, VarRequest { ty: semantic.ty(), location });
        scope.put_semantic(&mut ctx, semantic.id(), var);
    }
    scope.bind_refs();
    let root = if is_empty_semantic_diagnostics {
        let maybe_sealed_block = lower_block(&mut ctx, scope, semantic_block);
        maybe_sealed_block.and_then(|block_sealed| {
            let block = match block_sealed {
                SealedBlockBuilder::GotoCallsite { mut scope, expr } => {
                    // Convert to a return.
                    let var = expr.unwrap_or_else(|| {
                        generators::StructConstruct {
                            inputs: vec![],
                            ty: unit_ty(ctx.db.upcast()),
                            location: ctx.get_location(semantic_block.stable_ptr.untyped()),
                        }
                        .add(&mut ctx, &mut scope)
                    });
                    scope.ret(&mut ctx, var)?
                }
                SealedBlockBuilder::Ends(block) => block,
            };
            Ok(ctx.blocks.alloc(block))
        })
    } else {
        Err(DiagnosticAdded)
    };
    Ok(StructuredLowered {
        diagnostics: ctx.diagnostics.build(),
        root,
        variables: ctx.variables,
        blocks: ctx.blocks,
    })
}

/// Lowers a semantic block.
fn lower_block(
    ctx: &mut LoweringContext<'_>,
    mut scope: BlockBuilder,
    semantic_block: &semantic::ExprBlock,
) -> Maybe<SealedBlockBuilder> {
    let block_expr = lower_expr_block(ctx, &mut scope, semantic_block);
    lowered_expr_to_block_scope_end(ctx, scope, block_expr)
}

/// Lowers a semantic block.
fn lower_expr_block(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    expr_block: &semantic::ExprBlock,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a block.");
    for (i, stmt_id) in expr_block.statements.iter().enumerate() {
        let stmt = &ctx.function_body.statements[*stmt_id];
        let Err(err) = lower_statement(ctx, scope, stmt) else { continue; };
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
        .map(|expr| lower_expr(ctx, scope, expr))
        .unwrap_or_else(|| Ok(LoweredExpr::Tuple { exprs: vec![], location }))
}

/// Lowers an expression that is either a complete block, or the end (tail expression) of a
/// block.
pub fn lower_tail_expr(
    ctx: &mut LoweringContext<'_>,
    mut scope: BlockBuilder,
    expr: semantic::ExprId,
) -> Maybe<SealedBlockBuilder> {
    log::trace!("Lowering a tail expression.");
    let lowered_expr = lower_expr(ctx, &mut scope, expr);
    lowered_expr_to_block_scope_end(ctx, scope, lowered_expr)
}

/// Converts [`LoweringResult<LoweredExpr>`] into `BlockScopeEnd`.
pub fn lowered_expr_to_block_scope_end(
    ctx: &mut LoweringContext<'_>,
    mut scope: BlockBuilder,
    lowered_expr: LoweringResult<LoweredExpr>,
) -> Maybe<SealedBlockBuilder> {
    Ok(match lowered_expr {
        Ok(LoweredExpr::Tuple { exprs, .. }) if exprs.is_empty() => scope.goto_callsite(None),
        Ok(lowered_expr) => match lowered_expr.var(ctx, &mut scope) {
            Ok(var) => scope.goto_callsite(Some(var)),
            Err(err) => lowering_flow_error_to_sealed_block(ctx, scope, err)?,
        },
        Err(err) => lowering_flow_error_to_sealed_block(ctx, scope, err)?,
    })
}

/// Lowers a semantic statement.
pub fn lower_statement(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    stmt: &semantic::Statement,
) -> Result<(), LoweringFlowError> {
    match stmt {
        semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
            log::trace!("Lowering an expression statement.");
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            // The LoweredExpr must be evaluated now to push/bring back variables in case it is
            // LoweredExpr::ExternEnum.
            match lowered_expr {
                LoweredExpr::ExternEnum(x) => {
                    x.var(ctx, scope)?;
                }
                LoweredExpr::AtVariable(_) | LoweredExpr::Tuple { .. } => {}
            }
        }
        semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
            log::trace!("Lowering a let statement.");
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            lower_single_pattern(ctx, scope, pattern, lowered_expr)?
        }
        semantic::Statement::Return(semantic::StatementReturn { expr, stable_ptr: _ }) => {
            log::trace!("Lowering a return statement.");
            let ret_var = lower_expr(ctx, scope, *expr)?.var(ctx, scope)?;
            return Err(LoweringFlowError::Return(ret_var));
        }
    }
    Ok(())
}

// TODO(spapini): Separate match pattern from non-match (single) patterns in the semantic
// model.
/// Lowers a single-pattern (pattern that does not appear in a match. This includes structs,
/// tuples, variables, etc...
/// Adds the bound variables to the scope.
/// Note that single patterns are the only way to bind new local variables in the semantic
/// model.
fn lower_single_pattern(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    pattern: &semantic::Pattern,
    lowered_expr: LoweredExpr,
) -> Result<(), LoweringFlowError> {
    log::trace!("Lowering a single pattern.");
    match pattern {
        semantic::Pattern::Literal(_) => unreachable!(),
        semantic::Pattern::Variable(semantic::PatternVariable {
            name: _,
            var: sem_var,
            stable_ptr,
        }) => {
            let sem_var = semantic::Variable::Local(sem_var.clone());
            // Deposit the owned variable in the semantic variables store.
            let var = lowered_expr.var(ctx, scope)?;
            // Override variable location.
            ctx.variables[var].location = ctx.get_location(stable_ptr.untyped());
            scope.put_semantic(ctx, sem_var.id(), var);
            // TODO(spapini): Build semantic_defs in semantic model.
            ctx.semantic_defs.insert(sem_var.id(), sem_var);
        }
        semantic::Pattern::Struct(strct) => {
            let members = ctx.db.struct_members(strct.id).map_err(LoweringFlowError::Failed)?;
            let mut required_members = UnorderedHashMap::from_iter(
                strct.field_patterns.iter().map(|(member, pattern)| (member.id, pattern)),
            );
            let generator = generators::StructDestructure {
                input: lowered_expr.var(ctx, scope)?,
                var_reqs: members
                    .iter()
                    .map(|(_, member)| VarRequest {
                        ty: member.ty,
                        location: ctx.get_location(
                            required_members
                                .get(&member.id)
                                .map(|pattern| pattern.stable_ptr().untyped())
                                .unwrap_or_else(|| strct.stable_ptr.untyped()),
                        ),
                    })
                    .collect(),
            };
            for (var, (_, member)) in generator.add(ctx, scope).into_iter().zip(members.into_iter())
            {
                if let Some(member_pattern) = required_members.remove(&member.id) {
                    lower_single_pattern(ctx, scope, member_pattern, LoweredExpr::AtVariable(var))?;
                }
            }
        }
        semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty, stable_ptr }) => {
            let location = ctx.get_location(stable_ptr.untyped());
            let outputs = if let LoweredExpr::Tuple { exprs, .. } = lowered_expr {
                exprs
            } else {
                let reqs = extract_matches!(ctx.db.lookup_intern_type(*ty), TypeLongId::Tuple)
                    .into_iter()
                    .map(|ty| VarRequest { ty, location })
                    .collect();
                generators::StructDestructure {
                    input: lowered_expr.var(ctx, scope)?,
                    var_reqs: reqs,
                }
                .add(ctx, scope)
                .into_iter()
                .map(LoweredExpr::AtVariable)
                .collect()
            };
            for (var, pattern) in zip_eq(outputs, field_patterns) {
                lower_single_pattern(ctx, scope, pattern, var)?;
            }
        }
        semantic::Pattern::EnumVariant(_) => unreachable!(),
        semantic::Pattern::Otherwise(_) => {}
    }
    Ok(())
}

/// Lowers a semantic expression.
fn lower_expr(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    expr_id: semantic::ExprId,
) -> LoweringResult<LoweredExpr> {
    let expr = &ctx.function_body.exprs[expr_id];
    match expr {
        semantic::Expr::Constant(expr) => lower_expr_constant(ctx, expr, scope),
        semantic::Expr::Tuple(expr) => lower_expr_tuple(ctx, expr, scope),
        semantic::Expr::Assignment(expr) => lower_expr_assignment(ctx, expr, scope),
        semantic::Expr::Block(expr) => lower_expr_block(ctx, scope, expr),
        semantic::Expr::FunctionCall(expr) => lower_expr_function_call(ctx, expr, scope),
        semantic::Expr::Match(expr) => lower_expr_match(ctx, expr, scope),
        semantic::Expr::If(expr) => lower_expr_if(ctx, scope, expr),
        semantic::Expr::Var(expr) => {
            log::trace!("Lowering a variable: {:?}", expr.debug(&ctx.expr_formatter));
            Ok(LoweredExpr::AtVariable(scope.get_semantic(expr.var)))
        }
        semantic::Expr::Literal(expr) => lower_expr_literal(ctx, expr, scope),
        semantic::Expr::MemberAccess(expr) => lower_expr_member_access(ctx, expr, scope),
        semantic::Expr::StructCtor(expr) => lower_expr_struct_ctor(ctx, expr, scope),
        semantic::Expr::EnumVariantCtor(expr) => lower_expr_enum_ctor(ctx, expr, scope),
        semantic::Expr::PropagateError(expr) => lower_expr_error_propagate(ctx, expr, scope),
        semantic::Expr::Missing(semantic::ExprMissing { diag_added, .. }) => {
            Err(LoweringFlowError::Failed(*diag_added))
        }
    }
}

fn lower_expr_literal(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprLiteral,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a literal: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::Literal { value: expr.value.clone(), ty: expr.ty, location }.add(ctx, scope),
    ))
}

fn lower_expr_constant(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprConstant,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a constant: {:?}", expr.debug(&ctx.expr_formatter));
    let const_expr =
        &ctx.db.constant_semantic_data(expr.constant_id).map_err(LoweringFlowError::Failed)?.value;
    let semantic::Expr::Literal(const_expr_literal) = const_expr else {
        panic!("Only literal constants are supported.");
    };
    lower_expr_literal(ctx, const_expr_literal, scope)
}

/// Lowers an expression of type [semantic::ExprTuple].
fn lower_expr_tuple(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprTuple,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a tuple: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let inputs = expr
        .items
        .iter()
        .map(|arg_expr_id| lower_expr(ctx, scope, *arg_expr_id))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(LoweredExpr::Tuple { exprs: inputs, location })
}

/// Lowers an expression of type [semantic::ExprFunctionCall].
fn lower_expr_function_call(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprFunctionCall,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a function call expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());

    // TODO(spapini): Use the correct stable pointer.
    let arg_inputs = lower_exprs_as_vars(ctx, &expr.args, scope)?;
    let (ref_tys, ref_inputs): (_, Vec<VariableId>) = expr
        .ref_args
        .iter()
        .map(|semantic_var_id| {
            Ok((ctx.semantic_defs[*semantic_var_id].ty(), scope.get_semantic(*semantic_var_id)))
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .unzip();
    let callee_implicit_types =
        ctx.db.function_all_implicits(expr.function).map_err(LoweringFlowError::Failed)?;
    let implicits = callee_implicit_types.iter().map(|ty| scope.get_implicit(*ty));
    // TODO(orizi): Support ref args that are not the first arguments.
    let inputs = chain!(implicits, ref_inputs, arg_inputs.into_iter()).collect();

    // If the function is panic(), do something special.
    if expr.function == get_core_function_id(ctx.db.upcast(), "panic".into(), vec![]) {
        let [input] = <[_; 1]>::try_from(inputs).ok().unwrap();
        return Err(LoweringFlowError::Panic(input));
    }

    // The following is relevant only to extern functions.
    if expr.function.try_get_extern_function_id(ctx.db.upcast()).is_some() {
        if let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
            ctx.db.lookup_intern_type(expr.ty)
        {
            let lowered_expr = LoweredExprExternEnum {
                function: expr.function,
                concrete_enum_id,
                inputs,
                ref_args: expr.ref_args.clone(),
                implicits: callee_implicit_types,
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

    let (implicit_outputs, ref_outputs, res) =
        perform_function_call(ctx, scope, expr.function, inputs, ref_tys, expr.ty, location)?;

    // Rebind the implicits.
    for (implicit_type, implicit_output) in zip_eq(callee_implicit_types, implicit_outputs) {
        scope.put_implicit(ctx, implicit_type, implicit_output);
    }
    // Rebind the ref variables.
    for (semantic_var_id, output_var) in zip_eq(&expr.ref_args, ref_outputs) {
        scope.put_semantic(ctx, *semantic_var_id, output_var);
    }

    // Finalize call statement after ref rebinding.
    scope.finalize_statement();

    Ok(res)
}

/// Creates a LoweredExpr for a function call, taking into consideration external function facades:
/// For external functions, sometimes the high level signature doesn't exactly correspond to the
/// external function returned variables / branches.
fn perform_function_call(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    function: semantic::FunctionId,
    inputs: Vec<VariableId>,
    ref_tys: Vec<semantic::TypeId>,
    ret_ty: semantic::TypeId,
    location: StableLocation,
) -> Result<(Vec<VariableId>, Vec<VariableId>, LoweredExpr), LoweringFlowError> {
    // If the function is not extern, simply call it.
    if function.try_get_extern_function_id(ctx.db.upcast()).is_none() {
        let call_result =
            generators::Call { function, inputs, ref_tys, ret_tys: vec![ret_ty], location }
                .add(ctx, scope);
        let res = LoweredExpr::AtVariable(call_result.returns.into_iter().next().unwrap());
        return Ok((call_result.implicit_outputs, call_result.ref_outputs, res));
    };

    // Extern function.
    let ret_tys = extern_facade_return_tys(ctx, ret_ty);
    let call_result =
        generators::Call { function, inputs, ref_tys, ret_tys, location }.add(ctx, scope);
    Ok((
        call_result.implicit_outputs,
        call_result.ref_outputs,
        extern_facade_expr(ctx, ret_ty, call_result.returns, location),
    ))
}

/// Lowers an expression of type [semantic::ExprMatch].
fn lower_expr_match(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, scope, expr.matched_expr)?;

    if ctx.function_body.exprs[expr.matched_expr].ty() == ctx.db.core_felt_ty() {
        let var = lowered_expr.var(ctx, scope)?;
        return lower_expr_match_felt(ctx, expr, var, scope);
    }

    // TODO(spapini): Use diagnostics.
    // TODO(spapini): Handle more than just enums.
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, scope, extern_enum, &expr.arms);
    }

    let (concrete_enum_id, concrete_variants) = extract_concrete_enum(ctx, expr)?;
    let expr_var = lowered_expr.var(ctx, scope)?;

    // Merge arm blocks.
    let sealed_blocks = zip_eq(&concrete_variants, &expr.arms)
        .map(|(concrete_variant, arm)| {
            let mut subscope = scope.subscope_with_bound_refs();

            // TODO(spapini): Make a better diagnostic.
            let enum_pattern = try_extract_matches!(&arm.pattern, semantic::Pattern::EnumVariant)
                .ok_or_else(|| {
                LoweringFlowError::Failed(
                    ctx.diagnostics.report(expr.stable_ptr.untyped(), UnsupportedMatchArm),
                )
            })?;
            // TODO(spapini): Make a better diagnostic.
            if &enum_pattern.variant != concrete_variant {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics.report(expr.stable_ptr.untyped(), UnsupportedMatchArm),
                ));
            }

            let pattern_location =
                ctx.get_location(enum_pattern.inner_pattern.stable_ptr().untyped());
            let variant_expr = LoweredExpr::AtVariable(subscope.add_input(
                ctx,
                VarRequest { ty: concrete_variant.ty, location: pattern_location },
            ));

            match lower_single_pattern(
                ctx,
                &mut subscope,
                &enum_pattern.inner_pattern,
                variant_expr,
            ) {
                Ok(_) => {
                    // Lower the arm expression.
                    lower_tail_expr(ctx, subscope, arm.expression)
                }
                Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
            }
            .map_err(LoweringFlowError::Failed)
        })
        .collect::<LoweringResult<_>>()?;
    let merged = merge_sealed(ctx, scope, sealed_blocks, location);
    let arms = zip_eq(concrete_variants, merged.blocks).collect();
    scope.push_finalized_statement(Statement::MatchEnum(StatementMatchEnum {
        concrete_enum_id,
        input: expr_var,
        arms,
    }));
    merged.expr
}

/// Lowers a match expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_match(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
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
        return Err(LoweringFlowError::Failed(skip_diagnostic()));
    }
    // Merge arm blocks.
    let sealed_blocks = zip_eq(&concrete_variants, match_arms)
        .map(|(concrete_variant, arm)| {
            let mut subscope = scope.subscope();
            let input_tys =
                match_extern_variant_arm_input_types(ctx, concrete_variant.ty, &extern_enum);
            let mut input_vars = input_tys
                .into_iter()
                .map(|ty| subscope.add_input(ctx, VarRequest { ty, location }))
                .collect();

            // TODO(spapini): Make a better diagnostic.
            let enum_pattern = try_extract_matches!(&arm.pattern, semantic::Pattern::EnumVariant)
                .ok_or_else(|| {
                LoweringFlowError::Failed(
                    ctx.diagnostics.report_by_location(location, UnsupportedMatchArm),
                )
            })?;
            // TODO(spapini): Make a better diagnostic.
            if &enum_pattern.variant != concrete_variant {
                return Err(LoweringFlowError::Failed(
                    ctx.diagnostics.report_by_location(location, UnsupportedMatchArm),
                ));
            }

            // Bind the arm inputs to implicits and semantic variables.
            match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope);

            let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, input_vars, location);
            match lower_single_pattern(
                ctx,
                &mut subscope,
                &enum_pattern.inner_pattern,
                variant_expr,
            ) {
                Ok(_) => {
                    // Lower the arm expression.
                    lower_tail_expr(ctx, subscope, arm.expression)
                }
                Err(err) => lowering_flow_error_to_sealed_block(ctx, subscope, err),
            }
            .map_err(LoweringFlowError::Failed)
        })
        .collect::<LoweringResult<_>>()?;

    let merged = merge_sealed(ctx, scope, sealed_blocks, location);
    let arms = zip_eq(concrete_variants, merged.blocks).collect();

    // Emit the statement.
    scope.push_finalized_statement(Statement::MatchExtern(StatementMatchExtern {
        function: extern_enum.function,
        inputs: extern_enum.inputs,
        arms,
    }));
    merged.expr
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt.
/// Currently only a simple match-zero is supported.
fn lower_expr_match_felt(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
    expr_var: VariableId,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a match-felt expression.");
    let location = ctx.get_location(expr.stable_ptr.untyped());
    // Check that the match has the expected form.
    let (literal, block0, block_otherwise) = if let [
        semantic::MatchArm {
            pattern: semantic::Pattern::Literal(semantic::PatternLiteral { literal, .. }),
            expression: block0,
        },
        semantic::MatchArm {
            pattern: semantic::Pattern::Otherwise(_),
            expression: block_otherwise,
        },
    ] = &expr.arms[..]
    {
        (literal, block0, block_otherwise)
    } else {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(expr.stable_ptr.untyped(), OnlyMatchZeroIsSupported),
        ));
    };

    // Make sure literal is 0.
    if !literal.value.is_zero() {
        return Err(LoweringFlowError::Failed(
            ctx.diagnostics.report(literal.stable_ptr.untyped(), NonZeroValueInMatch),
        ));
    }

    let semantic_db = ctx.db.upcast();

    // Lower both blocks.
    let mut subscope_nz = scope.subscope_with_bound_refs();
    subscope_nz.add_input(
        ctx,
        VarRequest { ty: core_nonzero_ty(semantic_db, core_felt_ty(semantic_db)), location },
    );

    let sealed_blocks = vec![
        lower_tail_expr(ctx, scope.subscope_with_bound_refs(), *block0)
            .map_err(LoweringFlowError::Failed)?,
        lower_tail_expr(ctx, subscope_nz, *block_otherwise).map_err(LoweringFlowError::Failed)?,
    ];
    let merged = merge_sealed(ctx, scope, sealed_blocks, location);

    let concrete_variants =
        vec![jump_nz_zero_variant(ctx.db.upcast()), jump_nz_nonzero_variant(ctx.db.upcast())];
    let arms = zip_eq(concrete_variants, merged.blocks).collect();

    // Emit the statement.
    scope.push_finalized_statement(Statement::MatchExtern(StatementMatchExtern {
        function: core_jump_nz_func(semantic_db),
        inputs: vec![expr_var],
        arms,
    }));
    merged.expr
}

/// Extracts concrete enum and variants from a match expression. Assumes it is indeed a concrete
/// enum.
fn extract_concrete_enum(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
) -> Result<(semantic::ConcreteEnumId, Vec<semantic::ConcreteVariant>), LoweringFlowError> {
    let concrete_ty = try_extract_matches!(
        ctx.db.lookup_intern_type(ctx.function_body.exprs[expr.matched_expr].ty()),
        TypeLongId::Concrete
    )
    .to_maybe()
    .map_err(LoweringFlowError::Failed)?;
    let concrete_enum_id = try_extract_matches!(concrete_ty, ConcreteTypeId::Enum)
        .to_maybe()
        .map_err(LoweringFlowError::Failed)?;
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
            ctx.diagnostics.report(expr.stable_ptr.untyped(), UnsupportedMatch),
        ));
    }
    Ok((concrete_enum_id, concrete_variants))
}

/// Lowers a sequence of expressions and return them all. If the flow ended in the middle,
/// propagates that flow error without returning any variable.
fn lower_exprs_as_vars(
    ctx: &mut LoweringContext<'_>,
    exprs: &[semantic::ExprId],
    scope: &mut BlockBuilder,
) -> Result<Vec<VariableId>, LoweringFlowError> {
    exprs
        .iter()
        .map(|arg_expr_id| lower_expr(ctx, scope, *arg_expr_id)?.var(ctx, scope))
        .collect::<Result<Vec<_>, _>>()
}

/// Lowers an expression of type [semantic::ExprEnumVariantCtor].
fn lower_expr_enum_ctor(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprEnumVariantCtor,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!(
        "Started lowering of an enum c'tor expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    Ok(LoweredExpr::AtVariable(
        generators::EnumConstruct {
            input: lower_expr(ctx, scope, expr.value_expr)?.var(ctx, scope)?,
            variant: expr.variant.clone(),
            location,
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprMemberAccess].
fn lower_expr_member_access(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMemberAccess,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a member-access expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let members = ctx.db.struct_members(expr.struct_id).map_err(LoweringFlowError::Failed)?;
    let member_idx = members
        .iter()
        .position(|(_, member)| member.id == expr.member)
        .to_maybe()
        .map_err(LoweringFlowError::Failed)?;
    Ok(LoweredExpr::AtVariable(
        generators::StructMemberAccess {
            input: lower_expr(ctx, scope, expr.expr)?.var(ctx, scope)?,
            member_tys: members.into_iter().map(|(_, member)| member.ty).collect(),
            member_idx,
            location,
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprStructCtor].
fn lower_expr_struct_ctor(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprStructCtor,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Lowering a struct c'tor expression: {:?}", expr.debug(&ctx.expr_formatter));
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let members = ctx.db.struct_members(expr.struct_id).map_err(LoweringFlowError::Failed)?;
    let member_expr = UnorderedHashMap::from_iter(expr.members.iter().cloned());
    Ok(LoweredExpr::AtVariable(
        generators::StructConstruct {
            inputs: members
                .into_iter()
                .map(|(_, member)| lower_expr(ctx, scope, member_expr[member.id])?.var(ctx, scope))
                .collect::<Result<Vec<_>, _>>()?,
            ty: expr.ty,
            location,
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprPropagateError].
fn lower_expr_error_propagate(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprPropagateError,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!(
        "Started lowering of an error-propagate expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let lowered_expr = lower_expr(ctx, scope, expr.inner)?;
    let ExprPropagateError { ok_variant, err_variant, func_err_variant, .. } = expr;
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_error_propagate(
            ctx,
            scope,
            extern_enum,
            ok_variant,
            err_variant,
            func_err_variant,
            location,
        );
    }

    let var = lowered_expr.var(ctx, scope)?;
    // Ok arm.
    let mut subscope_ok = scope.subscope_with_bound_refs();
    let expr_var = subscope_ok.add_input(ctx, VarRequest { ty: ok_variant.ty, location });
    let sealed_block_ok = subscope_ok.goto_callsite(Some(expr_var));

    // Err arm.
    let mut subscope_err = scope.subscope_with_bound_refs();
    let err_value = subscope_err.add_input(ctx, VarRequest { ty: err_variant.ty, location });
    let err_res =
        generators::EnumConstruct { input: err_value, variant: func_err_variant.clone(), location }
            .add(ctx, &mut subscope_err);
    let sealed_block_err = subscope_err.ret(ctx, err_res).map_err(LoweringFlowError::Failed)?;

    // Merge blocks.
    let merged = merge_sealed(ctx, scope, vec![sealed_block_ok, sealed_block_err.into()], location);
    let block_ok = merged.blocks[0];
    let block_err = merged.blocks[1];

    let arms = vec![(ok_variant.clone(), block_ok), (err_variant.clone(), block_err)];

    // Emit the statement.
    scope.push_finalized_statement(Statement::MatchEnum(StatementMatchEnum {
        concrete_enum_id: ok_variant.concrete_enum_id,
        input: var,
        arms,
    }));
    merged.expr
}

/// Lowers an error propagation expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_error_propagate(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    extern_enum: LoweredExprExternEnum,
    ok_variant: &semantic::ConcreteVariant,
    err_variant: &semantic::ConcreteVariant,
    func_err_variant: &semantic::ConcreteVariant,
    location: StableLocation,
) -> LoweringResult<LoweredExpr> {
    log::trace!("Started lowering of an optimized error-propagate expression.");

    // Ok arm.
    let mut subscope_ok = scope.subscope();
    let input_tys = match_extern_variant_arm_input_types(ctx, ok_variant.ty, &extern_enum);
    let mut input_vars = input_tys
        .into_iter()
        .map(|ty| subscope_ok.add_input(ctx, VarRequest { ty, location }))
        .collect();
    match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope_ok);
    let expr =
        extern_facade_expr(ctx, ok_variant.ty, input_vars, location).var(ctx, &mut subscope_ok)?;
    let sealed_block_ok = subscope_ok.goto_callsite(Some(expr));

    // Err arm.
    let mut subscope_err = scope.subscope();
    let input_tys = match_extern_variant_arm_input_types(ctx, err_variant.ty, &extern_enum);
    let mut input_vars = input_tys
        .into_iter()
        .map(|ty| subscope_err.add_input(ctx, VarRequest { ty, location }))
        .collect();
    match_extern_arm_ref_args_bind(ctx, &mut input_vars, &extern_enum, &mut subscope_err);
    let expr = extern_facade_expr(ctx, err_variant.ty, input_vars, location);
    let input = expr.var(ctx, &mut subscope_err)?;
    let err_res = generators::EnumConstruct { input, variant: func_err_variant.clone(), location }
        .add(ctx, &mut subscope_err);
    let sealed_block_err = subscope_err.ret(ctx, err_res).map_err(LoweringFlowError::Failed)?;

    // Merge.
    let merged = merge_sealed(ctx, scope, vec![sealed_block_ok, sealed_block_err.into()], location);
    let block_ok = merged.blocks[0];
    let block_err = merged.blocks[1];

    let arms = vec![(ok_variant.clone(), block_ok), (err_variant.clone(), block_err)];
    // Emit the statement.
    scope.push_finalized_statement(Statement::MatchExtern(StatementMatchExtern {
        function: extern_enum.function,
        inputs: extern_enum.inputs,
        arms,
    }));
    merged.expr
}

/// Returns the input types for an extern match variant arm.
fn match_extern_variant_arm_input_types(
    ctx: &mut LoweringContext<'_>,
    ty: semantic::TypeId,
    extern_enum: &LoweredExprExternEnum,
) -> Vec<semantic::TypeId> {
    let variant_input_tys = extern_facade_return_tys(ctx, ty);
    let ref_tys =
        extern_enum.ref_args.iter().map(|semantic_var_id| ctx.semantic_defs[*semantic_var_id].ty());
    chain!(extern_enum.implicits.clone(), ref_tys, variant_input_tys.into_iter()).collect()
}

/// Binds input references and implicits when matching on extern functions.
fn match_extern_arm_ref_args_bind(
    ctx: &mut LoweringContext<'_>,
    arm_inputs: &mut Vec<VariableId>,
    extern_enum: &LoweredExprExternEnum,
    subscope: &mut BlockBuilder,
) {
    let implicit_outputs: Vec<_> = arm_inputs.drain(0..extern_enum.implicits.len()).collect();
    // Bind the implicits.
    for (ty, output_var) in zip_eq(&extern_enum.implicits, implicit_outputs) {
        subscope.put_implicit(ctx, *ty, output_var);
    }
    let ref_outputs: Vec<_> = arm_inputs.drain(0..extern_enum.ref_args.len()).collect();
    // Bind the ref parameters.
    for (semantic_var_id, output_var) in zip_eq(&extern_enum.ref_args, ref_outputs) {
        subscope.put_semantic(ctx, *semantic_var_id, output_var);
    }
    subscope.bind_refs();
}

/// Lowers an expression of type [semantic::ExprAssignment].
fn lower_expr_assignment(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprAssignment,
    scope: &mut BlockBuilder,
) -> LoweringResult<LoweredExpr> {
    log::trace!(
        "Started lowering of an assignment expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let location = ctx.get_location(expr.stable_ptr.untyped());
    let var = lower_expr(ctx, scope, expr.rhs)?.var(ctx, scope)?;
    scope.put_semantic(ctx, expr.var, var);
    Ok(LoweredExpr::Tuple { exprs: vec![], location })
}
