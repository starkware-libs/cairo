use defs::ids::{FreeFunctionId, GenericFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_traits::Zero;
use scope::{BlockScope, BlockScopeEnd};
use semantic::corelib::{core_felt_ty, core_jump_nz_func, core_nonzero_ty};
use semantic::db::SemanticGroup;
use semantic::items::enm::SemanticEnumEx;
use semantic::{ConcreteTypeId, Mutability, TypeLongId, VarId};
use syntax::node::ids::SyntaxStablePtrId;
use utils::unordered_hash_map::UnorderedHashMap;
use utils::{extract_matches, try_extract_matches};

use self::context::{
    LoweredExpr, LoweredExprExternEnum, LoweringContext, LoweringFlowError,
    StatementLoweringFlowError,
};
use self::external::{extern_facade_expr, extern_facade_return_tys};
use self::lower_if::lower_expr_if;
use self::scope::{generators, BlockFlowMerger};
use self::variables::LivingVar;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{Block, BlockId, Variable};

mod context;
mod external;
mod lower_if;
mod scope;
mod semantic_map;
mod variables;

/// A lowered function code.
#[derive(Debug, PartialEq, Eq)]
pub struct Lowered {
    /// Diagnostics produced while lowering.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    /// Block id for the start of the lowered function.
    pub root: Option<BlockId>,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
}

/// Lowers a semantic free function.
pub fn lower(db: &dyn SemanticGroup, free_function_id: FreeFunctionId) -> Option<Lowered> {
    let function_def = db.free_function_definition(free_function_id)?;

    let signature = db.free_function_declaration_signature(free_function_id)?;

    // Params.
    let ref_params: Vec<_> = signature
        .all_params()
        .filter(|param| param.mutability == Mutability::Reference)
        .map(|param| VarId::Param(param.id))
        .collect();
    let input_semantic_vars: Vec<semantic::Variable> =
        signature.all_params().cloned().map(semantic::Variable::Param).collect();
    let (input_semantic_var_ids, input_semantic_var_tys): (Vec<_>, Vec<_>) = input_semantic_vars
        .iter()
        .map(|semantic_var| (semantic_var.id(), semantic_var.ty()))
        .unzip();

    let mut ctx = LoweringContext {
        db,
        function_def: &function_def,
        diagnostics: LoweringDiagnostics::new(free_function_id.module(db.upcast())),
        variables: Arena::default(),
        blocks: Arena::default(),
        semantic_defs: UnorderedHashMap::default(),
        ref_params: &ref_params,
    };

    // TODO(spapini): Build semantic_defs in semantic model.
    for semantic_var in input_semantic_vars {
        ctx.semantic_defs.insert(semantic_var.id(), semantic_var);
    }

    // Fetch body block expr.
    let semantic_block =
        extract_matches!(&function_def.exprs[function_def.body], semantic::Expr::Block);
    // Lower block to a BlockSealed.
    let (block_sealed_opt, mut merger_finalized) =
        BlockFlowMerger::with_root(&mut ctx, &ref_params, |ctx, merger| {
            merger.run_in_subscope(ctx, input_semantic_var_tys, |ctx, scope, variables| {
                // Initialize params.
                for (semantic_var_id, var) in zip_eq(input_semantic_var_ids, variables) {
                    scope.put_semantic_variable(semantic_var_id, var);
                }
                lower_block(ctx, scope, semantic_block)
            })
        });
    let root = block_sealed_opt
        .map(|block_sealed| merger_finalized.finalize_block(&mut ctx, block_sealed).block);
    Some(Lowered {
        diagnostics: ctx.diagnostics.build(),
        root,
        variables: ctx.variables,
        blocks: ctx.blocks,
    })
}

/// Lowers a semantic block.
fn lower_block(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr_block: &semantic::ExprBlock,
) -> Option<BlockScopeEnd> {
    for (i, stmt_id) in expr_block.statements.iter().enumerate() {
        let stmt = &ctx.function_def.statements[*stmt_id];
        let lowered_stmt = lower_statement(ctx, scope, stmt);

        // If flow is not reachable anymore, no need to continue emitting statements.
        match lowered_stmt {
            Ok(()) => {}
            Err(StatementLoweringFlowError::Failed) => return None,
            Err(StatementLoweringFlowError::End(end)) => {
                // TODO(spapini): We might want to report unreachable for expr that abruptly
                // ends, e.g. `5 + {return; 6}`.
                if i + 1 < expr_block.statements.len() {
                    let start_stmt = &ctx.function_def.statements[expr_block.statements[i + 1]];
                    let end_stmt =
                        &ctx.function_def.statements[*expr_block.statements.last().unwrap()];
                    // Emit diagnostic fo the rest of the statements with unreachable.
                    ctx.diagnostics.report(
                        start_stmt.stable_ptr().untyped(),
                        Unreachable { last_statement_ptr: end_stmt.stable_ptr().untyped() },
                    );
                }
                return Some(end);
            }
        };
    }

    // Determine correct block end.
    match expr_block.tail {
        Some(tail_expr) => lower_tail_expr(ctx, scope, tail_expr),
        None => Some(BlockScopeEnd::Callsite(None)),
    }
}

/// Lowers an expression that is either a complete block, or the end (tail expreesion) of a
/// block.
pub fn lower_tail_expr(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: semantic::ExprId,
) -> Option<BlockScopeEnd> {
    let lowered_expr = lower_expr(ctx, scope, expr);
    lowered_expr_to_block_scope_end(ctx, scope, lowered_expr)
}

/// Converts [Result<LoweredExpr, LoweringFlowError>] into `BlockScopeEnd`.
pub fn lowered_expr_to_block_scope_end(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    lowered_expr: Result<LoweredExpr, LoweringFlowError>,
) -> Option<BlockScopeEnd> {
    Some(match lowered_expr {
        Ok(LoweredExpr::Tuple(tys)) if tys.is_empty() => BlockScopeEnd::Callsite(None),
        Ok(lowered_expr) => BlockScopeEnd::Callsite(Some(lowered_expr.var(ctx, scope))),
        Err(LoweringFlowError::Unreachable) => BlockScopeEnd::Unreachable,
        Err(LoweringFlowError::Failed) => {
            return None;
        }
    })
}

/// Lowers a semantic statement.
pub fn lower_statement(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    stmt: &semantic::Statement,
) -> Result<(), StatementLoweringFlowError> {
    match stmt {
        semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
            lower_expr(ctx, scope, *expr)?;
        }
        semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            lower_single_pattern(ctx, scope, pattern, lowered_expr)
        }
        semantic::Statement::Return(semantic::StatementReturn { expr, stable_ptr: _ }) => {
            // Lower return expr.
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            let value_vars = match lowered_expr {
                LoweredExpr::Tuple(tys) if tys.is_empty() => vec![],
                _ => vec![lowered_expr.var(ctx, scope)],
            };
            // Find variables to output for ref vars.
            let ref_vars = ctx
                .ref_params
                .iter()
                .map(|semantic_var_id| {
                    use_semantic_var(
                        ctx,
                        scope,
                        *semantic_var_id,
                        semantic_var_id.untyped_stable_ptr(ctx.db.upcast()),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            let return_vars = chain!(ref_vars, value_vars).collect();
            return Err(StatementLoweringFlowError::End(BlockScopeEnd::Return(return_vars)));
        }
    }
    Ok(())
}

// TODO:(spapini): Separate match pattern from non-match (single) patterns in the semantic
// model.
/// Lowers a single-pattern (pattern that does not appear in a match. This includes structs,
/// tuples, variables, etc...
/// Adds the bound variables to the scope.
/// Note that single patterns are the only way to bind new local variables in the semantic
/// model.
fn lower_single_pattern(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    pattern: &semantic::Pattern,
    lowered_expr: LoweredExpr,
) {
    match pattern {
        semantic::Pattern::Literal(_) => unreachable!(),
        semantic::Pattern::Variable(semantic::PatternVariable { name: _, var: sem_var }) => {
            let sem_var = semantic::Variable::Local(sem_var.clone());
            // Deposit the owned variable in the semantic variables store.
            let var = lowered_expr.var(ctx, scope);
            scope.put_semantic_variable(sem_var.id(), var);
            // TODO(spapini): Build semantic_defs in semantic model.
            ctx.semantic_defs.insert(sem_var.id(), sem_var);
        }
        semantic::Pattern::Struct(_) => todo!(),
        semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty }) => {
            let outputs = if let LoweredExpr::Tuple(exprs) = lowered_expr {
                exprs
            } else {
                let tys = extract_matches!(ctx.db.lookup_intern_type(*ty), TypeLongId::Tuple);
                generators::TupleDestruct { input: lowered_expr.var(ctx, scope), tys }
                    .add(ctx, scope)
                    .into_iter()
                    .map(LoweredExpr::AtVariable)
                    .collect()
            };
            for (var, pattern) in zip_eq(outputs, field_patterns) {
                lower_single_pattern(ctx, scope, pattern, var);
            }
        }
        semantic::Pattern::Enum(_) => unreachable!(),
        semantic::Pattern::Otherwise(_) => {}
    }
}

/// Lowers a semantic expression.
fn lower_expr(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr_id: semantic::ExprId,
) -> Result<LoweredExpr, LoweringFlowError> {
    let expr = &ctx.function_def.exprs[expr_id];
    match expr {
        semantic::Expr::Tuple(expr) => lower_expr_tuple(ctx, expr, scope),
        semantic::Expr::Assignment(expr) => lower_expr_assignment(ctx, expr, scope),
        semantic::Expr::Block(expr) => lower_expr_block(ctx, scope, expr),
        semantic::Expr::FunctionCall(expr) => lower_expr_function_call(ctx, expr, scope),
        semantic::Expr::Match(expr) => lower_expr_match(ctx, expr, scope),
        semantic::Expr::If(expr) => lower_expr_if(ctx, scope, expr),
        semantic::Expr::Var(expr) => Ok(LoweredExpr::AtVariable(use_semantic_var(
            ctx,
            scope,
            expr.var,
            expr.stable_ptr.untyped(),
        )?)),
        semantic::Expr::Literal(expr) => Ok(LoweredExpr::AtVariable(
            generators::Literal { value: expr.value.clone(), ty: expr.ty }.add(ctx, scope),
        )),
        semantic::Expr::MemberAccess(_) => todo!(),
        semantic::Expr::StructCtor(_) => todo!(),
        semantic::Expr::EnumVariantCtor(expr) => lower_expr_enum_ctor(ctx, expr, scope),
        semantic::Expr::Missing(_) => Err(LoweringFlowError::Failed),
    }
}

/// Lowers an expression of type [semantic::ExprTuple].
fn lower_expr_tuple(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprTuple,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    let inputs = expr
        .items
        .iter()
        .map(|arg_expr_id| lower_expr(ctx, scope, *arg_expr_id))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(LoweredExpr::Tuple(inputs))
}

/// Lowers an expression of type [semantic::ExprBlock].
fn lower_expr_block(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: &semantic::ExprBlock,
) -> Result<LoweredExpr, LoweringFlowError> {
    let (block_sealed, mut finalized_merger) =
        BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
            merger.run_in_subscope(ctx, vec![], |ctx, subscope, _| lower_block(ctx, subscope, expr))
        });
    let block_sealed = block_sealed.ok_or(LoweringFlowError::Failed)?;
    let block_finalized = finalized_merger.finalize_block(ctx, block_sealed);

    // Emit the statement.
    let call_block_generator =
        generators::CallBlock { block: block_finalized.block, end_info: finalized_merger.end_info };
    let block_result = call_block_generator.add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}

/// Lowers an expression of type [semantic::ExprFunctionCall].
fn lower_expr_function_call(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprFunctionCall,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    // TODO(spapini): Use the correct stable pointer.
    let (ref_tys, ref_inputs): (_, Vec<LivingVar>) = expr
        .ref_args
        .iter()
        .map(|semantic_var_id| {
            Ok((
                ctx.semantic_defs[*semantic_var_id].ty(),
                take_semantic_var(ctx, scope, *semantic_var_id, expr.stable_ptr.untyped())?,
            ))
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .unzip();
    // TODO(orizi): Support ref args that are not the first arguments.
    let arg_inputs = lower_exprs_as_vars(ctx, &expr.args, scope)?;
    let inputs = chain!(ref_inputs, arg_inputs.into_iter()).collect();

    if let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
        ctx.db.lookup_intern_type(expr.ty)
    {
        return Ok(LoweredExpr::ExternEnum(LoweredExprExternEnum {
            function: expr.function,
            concrete_enum_id,
            inputs,
            ref_args: expr.ref_args.clone(),
        }));
    }

    let (ref_outputs, res) =
        perform_function_call(ctx, scope, expr.function, inputs, ref_tys, expr.ty);

    // Rebind the ref variables.
    for (semantic_var_id, output_var) in zip_eq(&expr.ref_args, ref_outputs) {
        scope.put_semantic_variable(*semantic_var_id, output_var);
    }
    Ok(res)
}

/// Creates a LoweredExpr for a function call, taking into consideration external function facades:
/// For external functions, sometimes the high level signature doesn't exactly correspond to the
/// external function returned variables / branches.
fn perform_function_call(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    function: semantic::FunctionId,
    inputs: Vec<LivingVar>,
    ref_tys: Vec<semantic::TypeId>,
    ret_ty: semantic::TypeId,
) -> (Vec<LivingVar>, LoweredExpr) {
    // If the function is not extern, simply call it.
    if !matches!(
        ctx.db.lookup_intern_function(function),
        semantic::FunctionLongId::Concrete(semantic::ConcreteFunction {
            generic_function: GenericFunctionId::Extern(_),
            ..
        })
    ) {
        let call_result =
            generators::Call { function, inputs, ref_tys, ret_tys: vec![ret_ty] }.add(ctx, scope);
        let ref_outputs = call_result.ref_outputs;
        let res = LoweredExpr::AtVariable(call_result.returns.into_iter().next().unwrap());
        return (ref_outputs, res);
    };

    // Extern function
    let ret_tys = extern_facade_return_tys(ctx, ret_ty);
    let call_result = generators::Call { function, inputs, ref_tys, ret_tys }.add(ctx, scope);
    let ref_outputs = call_result.ref_outputs;
    (ref_outputs, extern_facade_expr(ctx, ret_ty, call_result.returns))
}

/// Lowers an expression of type [semantic::ExprMatch].
fn lower_expr_match(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    let lowered_expr = lower_expr(ctx, scope, expr.matched_expr)?;

    if ctx.function_def.exprs[expr.matched_expr].ty() == ctx.db.core_felt_ty() {
        let var = lowered_expr.var(ctx, scope);
        return lower_expr_match_felt(ctx, expr, var, scope);
    }

    // TODO(spapini): Use diagnostics.
    // TODO(spapini): Handle more than just enums.
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_match(ctx, scope, extern_enum, &expr.arms);
    }

    let (concrete_enum_id, concrete_variants) = extract_concrete_enum(ctx, expr)?;
    let expr_var = lowered_expr.var(ctx, scope);

    // Merge arm blocks.
    let (res, mut finalized_merger) =
        BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| -> Result<_, LoweringFlowError> {
            // Create a sealed block for each arm.
            let block_opts =
                zip_eq(&concrete_variants, &expr.arms).map(|(concrete_variant, arm)| {
                    let semantic_var_id = extract_var_pattern(&arm.pattern, concrete_variant)?;
                    // Create a scope for the arm block.
                    merger.run_in_subscope(
                        ctx,
                        vec![concrete_variant.ty],
                        |ctx, subscope, variables| {
                            // Bind the arm input variable to the semantic variable.
                            let [var] = <[_; 1]>::try_from(variables).ok().unwrap();
                            subscope.put_semantic_variable(semantic_var_id, var);

                            // Lower the arm expression.
                            lower_tail_expr(ctx, subscope, arm.expression)
                        },
                    )
                });
            block_opts.collect::<Option<Vec<_>>>().ok_or(LoweringFlowError::Failed)
        });
    let finalized_blocks =
        res?.into_iter().map(|sealed| finalized_merger.finalize_block(ctx, sealed).block);

    let arms = zip_eq(concrete_variants, finalized_blocks).collect();

    // Emit the statement.
    let match_generator = generators::MatchEnum {
        input: expr_var,
        concrete_enum_id,
        arms,
        end_info: finalized_merger.end_info,
    };
    let block_result = match_generator.add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}

/// Lowers a match expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_match(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    extern_enum: LoweredExprExternEnum,
    match_arms: &[semantic::MatchArm],
) -> Result<LoweredExpr, LoweringFlowError> {
    let concrete_variants = ctx.db.concrete_enum_variants(extern_enum.concrete_enum_id).unwrap();
    if match_arms.len() != concrete_variants.len() {
        return Err(LoweringFlowError::Failed);
    }
    // Merge arm blocks.
    let (blocks, mut finalized_merger) = BlockFlowMerger::with(
        ctx,
        scope,
        &extern_enum.ref_args,
        |ctx, merger| -> Result<_, LoweringFlowError> {
            // Create a sealed block for each arm.
            let block_opts =
                zip_eq(&concrete_variants, match_arms).map(|(concrete_variant, arm)| {
                    let variant_input_tys = extern_facade_return_tys(ctx, concrete_variant.ty);
                    let ref_tys = extern_enum
                        .ref_args
                        .iter()
                        .map(|semantic_var_id| ctx.semantic_defs[*semantic_var_id].ty());
                    let input_tys = chain!(ref_tys, variant_input_tys.into_iter()).collect();

                    // Create a scope for the arm block.
                    merger.run_in_subscope(ctx, input_tys, |ctx, subscope, mut arm_inputs| {
                        let ref_outputs: Vec<_> =
                            arm_inputs.drain(0..extern_enum.ref_args.len()).collect();
                        let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, arm_inputs);
                        let enum_pattern =
                            try_extract_matches!(&arm.pattern, semantic::Pattern::Enum)?;
                        // TODO(spapini): Convert to a diagnostic.
                        assert_eq!(&enum_pattern.variant, concrete_variant, "Wrong variant");
                        lower_single_pattern(
                            ctx,
                            subscope,
                            &enum_pattern.inner_pattern,
                            variant_expr,
                        );

                        // Rebind the ref variables.
                        for (semantic_var_id, output_var) in
                            zip_eq(&extern_enum.ref_args, ref_outputs)
                        {
                            subscope.put_semantic_variable(*semantic_var_id, output_var);
                        }

                        // Lower the arm expression.
                        lower_tail_expr(ctx, subscope, arm.expression)
                    })
                });
            block_opts.collect::<Option<Vec<_>>>().ok_or(LoweringFlowError::Failed)
        },
    );
    let arms = blocks?
        .into_iter()
        .map(|sealed| finalized_merger.finalize_block(ctx, sealed).block)
        .collect();
    let block_result = generators::MatchExtern {
        function: extern_enum.function,
        inputs: extern_enum.inputs,
        arms,
        end_info: finalized_merger.end_info,
    }
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt.
/// Currently only a simple match-zero is supported.
fn lower_expr_match_felt(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
    expr_var: LivingVar,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
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
        ctx.diagnostics.report(expr.stable_ptr.untyped(), OnlyMatchZeroIsSupported);
        return Err(LoweringFlowError::Failed);
    };

    // Make sure literal is 0.
    if !literal.value.is_zero() {
        ctx.diagnostics.report(literal.stable_ptr.untyped(), NonZeroValueInMatch);
        return Err(LoweringFlowError::Failed);
    }

    // Lower both blocks.
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
        let block0_end = merger.run_in_subscope(ctx, vec![], |ctx, subscope, _| {
            lower_tail_expr(ctx, subscope, *block0)
        });
        let non_zero_type = core_nonzero_ty(ctx.db, core_felt_ty(ctx.db));
        let block_otherwise_end =
            merger.run_in_subscope(ctx, vec![non_zero_type], |ctx, subscope, _| {
                lower_tail_expr(ctx, subscope, *block_otherwise)
            });
        Some((block0_end, block_otherwise_end))
    });
    let (block0_sealed, block_otherwise_sealed) = res.ok_or(LoweringFlowError::Failed)?;
    let block0_finalized =
        finalized_merger.finalize_block(ctx, block0_sealed.ok_or(LoweringFlowError::Failed)?);
    let block_otherwise_finalized = finalized_merger
        .finalize_block(ctx, block_otherwise_sealed.ok_or(LoweringFlowError::Failed)?);

    // Emit the statement.
    let match_generator = generators::MatchExtern {
        function: core_jump_nz_func(ctx.db),
        inputs: vec![expr_var],
        arms: vec![block0_finalized.block, block_otherwise_finalized.block],
        end_info: finalized_merger.end_info,
    };
    let block_result = match_generator.add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
}

/// Extracts concrete enum and variants from a match expression. Assumes it is indeed a concrete
/// enum.
fn extract_concrete_enum(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
) -> Result<(semantic::ConcreteEnumId, Vec<semantic::ConcreteVariant>), LoweringFlowError> {
    let concrete_ty = try_extract_matches!(
        ctx.db.lookup_intern_type(ctx.function_def.exprs[expr.matched_expr].ty()),
        TypeLongId::Concrete
    )
    .ok_or(LoweringFlowError::Failed)?;
    let concrete_enum_id =
        try_extract_matches!(concrete_ty, ConcreteTypeId::Enum).ok_or(LoweringFlowError::Failed)?;
    let enum_id = concrete_enum_id.enum_id(ctx.db);
    let variants = ctx.db.enum_variants(enum_id).ok_or(LoweringFlowError::Failed)?;
    let concrete_variants = variants
        .values()
        .map(|variant_id| {
            let variant =
                ctx.db.variant_semantic(enum_id, *variant_id).ok_or(LoweringFlowError::Failed)?;

            ctx.db
                .concrete_enum_variant(concrete_enum_id, &variant)
                .ok_or(LoweringFlowError::Failed)
        })
        .collect::<Result<Vec<_>, _>>()?;

    assert_eq!(expr.arms.len(), concrete_variants.len(), "Wrong number of arms.");
    Ok((concrete_enum_id, concrete_variants))
}

/// Lowers a sequence of expressions and return them all. If the flow ended in the middle,
/// propagates that flow error without returning any variable.
fn lower_exprs_as_vars(
    ctx: &mut LoweringContext<'_>,
    exprs: &[semantic::ExprId],
    scope: &mut BlockScope,
) -> Result<Vec<LivingVar>, LoweringFlowError> {
    exprs
        .iter()
        .map(|arg_expr_id| Ok(lower_expr(ctx, scope, *arg_expr_id)?.var(ctx, scope)))
        .collect::<Result<Vec<_>, _>>()
}

/// Lowers an expression of type [semantic::ExprEnumVariantCtor].
fn lower_expr_enum_ctor(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprEnumVariantCtor,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    Ok(LoweredExpr::AtVariable(
        generators::EnumConstruct {
            input: lower_expr(ctx, scope, expr.value_expr)?.var(ctx, scope),
            variant: expr.variant.clone(),
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprAssignment].
fn lower_expr_assignment(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprAssignment,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    scope.try_ensure_semantic_variable(ctx, expr.var);
    let var = lower_expr(ctx, scope, expr.rhs)?.var(ctx, scope);
    scope.put_semantic_variable(expr.var, var);
    Ok(LoweredExpr::Tuple(vec![]))
}

/// Retrieves a LivingVar that corresponds to a semantic var in the current scope.
/// Moves it if necessary. If it is already moved, fails and emits a diagnostic.
fn use_semantic_var(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    semantic_var: semantic::VarId,
    stable_ptr: SyntaxStablePtrId,
) -> Result<LivingVar, LoweringFlowError> {
    scope.use_semantic_variable(ctx, semantic_var).take_var().ok_or_else(|| {
        ctx.diagnostics.report(stable_ptr, VariableMoved);
        LoweringFlowError::Failed
    })
}

/// Retrieves a LivingVar that corresponds to a semantic var in the current scope.
/// Always moves. If it is already moved, fails and emits a diagnostic.
fn take_semantic_var(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    semantic_var: semantic::VarId,
    stable_ptr: SyntaxStablePtrId,
) -> Result<LivingVar, LoweringFlowError> {
    scope.take_semantic_variable(ctx, semantic_var).take_var().ok_or_else(|| {
        ctx.diagnostics.report(stable_ptr, VariableMoved);
        LoweringFlowError::Failed
    })
}

/// Converts a CallBlockResult for a LoweredExpr.
/// Some statements end with a CallBlockResult (CallBlock, Match, etc..), which represents all
/// the information of the "ending" of the call.
/// Binds the semantic variables from the call
/// Returns the proper flow error if needed.
fn lowered_expr_from_block_result(
    scope: &mut BlockScope,
    block_result: generators::CallBlockResult,
    pushed_semantic_vars: Vec<semantic::VarId>,
) -> Result<LoweredExpr, LoweringFlowError> {
    match block_result {
        generators::CallBlockResult::Callsite { maybe_output, pushes } => {
            for (semantic_var_id, var) in zip_eq(pushed_semantic_vars, pushes) {
                scope.put_semantic_variable(semantic_var_id, var);
            }
            Ok(match maybe_output {
                Some(output) => LoweredExpr::AtVariable(output),
                None => LoweredExpr::Tuple(vec![]),
            })
        }
        generators::CallBlockResult::End => Err(LoweringFlowError::Unreachable),
    }
}

/// Checks that the pattern is an enum pattern for a concrete variant, and returns the semantic
/// variable id.
fn extract_var_pattern(
    pattern: &semantic::Pattern,
    concrete_variant: &semantic::ConcreteVariant,
) -> Option<semantic::VarId> {
    let enum_pattern = try_extract_matches!(&pattern, semantic::Pattern::Enum)?;
    assert_eq!(&enum_pattern.variant, concrete_variant, "Wrong variant");
    let var_pattern =
        try_extract_matches!(&*enum_pattern.inner_pattern, semantic::Pattern::Variable)?;
    let semantic_var_id = semantic::VarId::Local(var_pattern.var.id);
    Some(semantic_var_id)
}
