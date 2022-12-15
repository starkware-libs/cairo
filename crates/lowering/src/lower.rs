use debug::DebugWithDb;
use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::{skip_diagnostic, Diagnostics, Maybe, ToMaybe};
use id_arena::Arena;
use itertools::{chain, zip_eq, Itertools};
use num_traits::Zero;
use scope::{BlockScope, BlockScopeEnd};
use semantic::corelib::{
    core_felt_ty, core_jump_nz_func, core_nonzero_ty, get_core_function_id,
    get_enum_concrete_variant, get_panic_ty, jump_nz_nonzero_variant, jump_nz_zero_variant,
};
use semantic::expr::fmt::ExprFormatter;
use semantic::items::enm::SemanticEnumEx;
use semantic::items::imp::ImplLookupContext;
use semantic::{ConcreteTypeId, GenericArgumentId, Mutability, TypeLongId, VarId};
use syntax::node::ids::SyntaxStablePtrId;
use utils::unordered_hash_map::UnorderedHashMap;
use utils::{extract_matches, try_extract_matches};

use self::context::{
    lowering_flow_error_to_block_scope_end, LoweredExpr, LoweredExprExternEnum, LoweringContext,
    LoweringFlowError, StatementLoweringFlowError,
};
use self::external::{extern_facade_expr, extern_facade_return_tys};
use self::lower_if::lower_expr_if;
use self::scope::{generators, BlockFlowMerger, BlockMergerFinalized};
use self::variables::LivingVar;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{Block, BlockId, Variable};

mod context;
mod external;
pub mod implicits;
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
    pub root: Maybe<BlockId>,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
}

/// Lowers a semantic free function.
pub fn lower(db: &dyn LoweringGroup, free_function_id: FreeFunctionId) -> Maybe<Lowered> {
    log::trace!("Lowering a free function.");
    let function_def = db.free_function_definition(free_function_id)?;
    let generic_params = db.free_function_declaration_generic_params(free_function_id)?;
    let signature = db.free_function_declaration_signature(free_function_id)?;

    let implicits = db.free_function_all_implicits_vec(free_function_id)?;
    // Params.
    let ref_params = signature
        .params
        .iter()
        .filter(|param| param.mutability == Mutability::Reference)
        .map(|param| VarId::Param(param.id))
        .collect_vec();
    let input_semantic_vars: Vec<semantic::Variable> =
        signature.params.iter().cloned().map(semantic::Variable::Param).collect();
    let (input_semantic_var_ids, input_var_tys): (Vec<_>, Vec<_>) = input_semantic_vars
        .iter()
        .map(|semantic_var| (semantic_var.id(), semantic_var.ty()))
        .unzip();
    let input_var_tys = chain!(implicits.clone(), input_var_tys).collect();

    let implicits_ref = &implicits;
    let mut ctx = LoweringContext {
        db,
        function_def: &function_def,
        signature,
        may_panic: db.free_function_may_panic(free_function_id)?,
        diagnostics: LoweringDiagnostics::new(free_function_id.module_file(db.upcast())),
        variables: Arena::default(),
        blocks: Arena::default(),
        semantic_defs: UnorderedHashMap::default(),
        ref_params: &ref_params,
        implicits: implicits_ref,
        lookup_context: ImplLookupContext {
            module_id: free_function_id.module(db.upcast()),
            extra_modules: vec![],
            generic_params,
        },
        expr_formatter: ExprFormatter { db: db.upcast(), free_function_id },
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
            merger.run_in_subscope(ctx, input_var_tys, |ctx, scope, variables| {
                let mut variables_iter = variables.into_iter();
                for ty in implicits_ref {
                    let var = variables_iter.next().to_maybe()?;
                    scope.put_implicit(*ty, var);
                }

                // Initialize implicits and params.
                for (semantic_var_id, var) in zip_eq(input_semantic_var_ids, variables_iter) {
                    scope.put_semantic_variable(semantic_var_id, var);
                }
                lower_block(ctx, scope, semantic_block, true)
            })
        });
    let root = block_sealed_opt
        .map(|block_sealed| merger_finalized.finalize_block(&mut ctx, block_sealed).block);
    Ok(Lowered {
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
    root: bool,
) -> Maybe<BlockScopeEnd> {
    log::trace!("Lowering a block.");
    for (i, stmt_id) in expr_block.statements.iter().enumerate() {
        let stmt = &ctx.function_def.statements[*stmt_id];
        let lowered_stmt = lower_statement(ctx, scope, stmt);

        // If flow is not reachable anymore, no need to continue emitting statements.
        match lowered_stmt {
            Ok(()) => {}
            Err(StatementLoweringFlowError::Failed(diag_added)) => return Err(diag_added),
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
                return Ok(end);
            }
        };
    }

    // Determine correct block end.
    match expr_block.tail {
        None if !root => Ok(BlockScopeEnd::Callsite(None)),
        _ => lower_tail_expr(ctx, scope, expr_block.tail, root),
    }
}

/// Lowers an expression that is either a complete block, or the end (tail expreesion) of a
/// block.
pub fn lower_tail_expr(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    expr: Option<semantic::ExprId>,
    root: bool,
) -> Maybe<BlockScopeEnd> {
    log::trace!("Lowering a tail expression.");
    let mut lowered_expr = if let Some(expr) = expr {
        lower_expr(ctx, scope, expr)
    } else {
        Ok(LoweredExpr::Tuple(vec![]))
    };
    if root {
        lowered_expr = lowered_expr.and_then(|expr| maybe_wrap_with_panic(ctx, expr, scope));
    }
    lowered_expr_to_block_scope_end(ctx, scope, lowered_expr, root)
}

/// Converts [Result<LoweredExpr, LoweringFlowError>] into `BlockScopeEnd`.
pub fn lowered_expr_to_block_scope_end(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    lowered_expr: Result<LoweredExpr, LoweringFlowError>,
    root: bool,
) -> Maybe<BlockScopeEnd> {
    Ok(match lowered_expr {
        Ok(LoweredExpr::Tuple(tys)) if !root && tys.is_empty() => BlockScopeEnd::Callsite(None),
        Ok(lowered_expr) => match lowered_expr.var(ctx, scope) {
            Ok(var) => BlockScopeEnd::Callsite(Some(var)),
            Err(err) => lowering_flow_error_to_block_scope_end(err)?,
        },
        Err(err) => lowering_flow_error_to_block_scope_end(err)?,
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
            log::trace!("Lowering an expression statement.");
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            // The LoweredExpr must be evaluated now to push/bring back variables in case it is
            // LoweredExpr::ExternEnum.
            match lowered_expr {
                LoweredExpr::ExternEnum(x) => {
                    x.var(ctx, scope)?;
                }
                LoweredExpr::AtVariable(_) | LoweredExpr::Tuple(_) => {}
            }
        }
        semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
            log::trace!("Lowering a let statement.");
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            lower_single_pattern(ctx, scope, pattern, lowered_expr)?
        }
        semantic::Statement::Return(semantic::StatementReturn { expr, stable_ptr: _ }) => {
            log::trace!("Lowering a return statement.");
            let lowered_expr = lower_expr(ctx, scope, *expr)?;
            let return_vars = get_full_return_vars(ctx, scope, lowered_expr)?;
            return Err(StatementLoweringFlowError::End(BlockScopeEnd::Return(return_vars)));
        }
    }
    Ok(())
}

/// Returns the return variables, prefixed by the reference params. Wraps with PanicResult if
/// needed.
fn get_full_return_vars(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    value_expr: LoweredExpr,
) -> Result<Vec<LivingVar>, StatementLoweringFlowError> {
    let lowered_expr = maybe_wrap_with_panic(ctx, value_expr, scope)?;
    let value_vars = match lowered_expr {
        LoweredExpr::Tuple(tys) if tys.is_empty() => vec![],
        _ => vec![lowered_expr.var(ctx, scope)?],
    };
    Ok(get_plain_full_return_vars(ctx, scope, value_vars)?)
}

/// Wraps a LoweredExpr with PanicResult::Ok id the current function panics.
fn maybe_wrap_with_panic(
    ctx: &mut LoweringContext<'_>,
    value_expr: LoweredExpr,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    let lowered_expr = if ctx.may_panic {
        let variant = get_enum_concrete_variant(
            ctx.db.upcast(),
            "PanicResult",
            vec![GenericArgumentId::Type(value_expr.ty(ctx))],
            "Ok",
        );
        LoweredExpr::AtVariable(
            generators::EnumConstruct { input: value_expr.var(ctx, scope)?, variant }
                .add(ctx, scope),
        )
    } else {
        value_expr
    };
    Ok(lowered_expr)
}

/// Returns the return variables, prefixed by the reference params, without wrapping with
/// PanicResult.
fn get_plain_full_return_vars(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    value_vars: Vec<LivingVar>,
) -> Result<Vec<LivingVar>, LoweringFlowError> {
    let implicit_vars = ctx
        .implicits
        .iter()
        .map(|ty| scope.take_implicit(*ty))
        .collect::<Option<Vec<_>>>()
        .to_maybe()
        .map_err(LoweringFlowError::Failed)?;

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
    Ok(chain!(implicit_vars, ref_vars, value_vars).collect())
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
    scope: &mut BlockScope,
    pattern: &semantic::Pattern,
    lowered_expr: LoweredExpr,
) -> Result<(), LoweringFlowError> {
    log::trace!("Lowering a single pattern.");
    match pattern {
        semantic::Pattern::Literal(_) => unreachable!(),
        semantic::Pattern::Variable(semantic::PatternVariable { name: _, var: sem_var }) => {
            let sem_var = semantic::Variable::Local(sem_var.clone());
            // Deposit the owned variable in the semantic variables store.
            let var = lowered_expr.var(ctx, scope)?;
            scope.put_semantic_variable(sem_var.id(), var);
            // TODO(spapini): Build semantic_defs in semantic model.
            ctx.semantic_defs.insert(sem_var.id(), sem_var);
        }
        semantic::Pattern::Struct(strct) => {
            let members = ctx.db.struct_members(strct.id).unwrap();
            let mut required_members = UnorderedHashMap::from_iter(
                strct.field_patterns.iter().map(|(member, pattern)| (member.id, pattern)),
            );
            let generator = generators::StructDestructure {
                input: lowered_expr.var(ctx, scope)?,
                tys: members.iter().map(|(_, member)| member.ty).collect(),
            };
            for (var, (_, member)) in generator.add(ctx, scope).into_iter().zip(members.into_iter())
            {
                if let Some(member_pattern) = required_members.remove(&member.id) {
                    lower_single_pattern(ctx, scope, member_pattern, LoweredExpr::AtVariable(var))?;
                }
            }
        }
        semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty }) => {
            let outputs = if let LoweredExpr::Tuple(exprs) = lowered_expr {
                exprs
            } else {
                let tys = extract_matches!(ctx.db.lookup_intern_type(*ty), TypeLongId::Tuple);
                generators::StructDestructure { input: lowered_expr.var(ctx, scope)?, tys }
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
        semantic::Expr::Var(expr) => {
            log::trace!("Lowering a variable: {:?}", expr.debug(&ctx.expr_formatter));
            Ok(LoweredExpr::AtVariable(use_semantic_var(
                ctx,
                scope,
                expr.var,
                expr.stable_ptr.untyped(),
            )?))
        }
        semantic::Expr::Literal(expr) => {
            log::trace!("Lowering a literal: {:?}", expr.debug(&ctx.expr_formatter));
            Ok(LoweredExpr::AtVariable(
                generators::Literal { value: expr.value.clone(), ty: expr.ty }.add(ctx, scope),
            ))
        }
        semantic::Expr::MemberAccess(expr) => lower_expr_member_access(ctx, expr, scope),
        semantic::Expr::StructCtor(expr) => lower_expr_struct_ctor(ctx, expr, scope),
        semantic::Expr::EnumVariantCtor(expr) => lower_expr_enum_ctor(ctx, expr, scope),
        semantic::Expr::PropagateError(expr) => lower_expr_error_propagate(ctx, expr, scope),
        semantic::Expr::Missing(semantic::ExprMissing { diag_added, .. }) => {
            Err(LoweringFlowError::Failed(*diag_added))
        }
    }
}

/// Lowers an expression of type [semantic::ExprTuple].
fn lower_expr_tuple(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprTuple,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a tuple: {:?}", expr.debug(&ctx.expr_formatter));
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
    log::trace!("Lowering a block expression: {:?}", expr.debug(&ctx.expr_formatter));
    let (block_sealed, mut finalized_merger) =
        BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
            merger.run_in_subscope(ctx, vec![], |ctx, subscope, _| {
                lower_block(ctx, subscope, expr, false)
            })
        });
    let block_sealed = block_sealed.map_err(LoweringFlowError::Failed)?;
    let block_finalized = finalized_merger.finalize_block(ctx, block_sealed);

    // Emit the statement.
    let block_result = (generators::CallBlock {
        block: block_finalized.block,
        end_info: finalized_merger.end_info.clone(),
    })
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
}

/// Lowers an expression of type [semantic::ExprFunctionCall].
fn lower_expr_function_call(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprFunctionCall,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a function call expression: {:?}", expr.debug(&ctx.expr_formatter));

    // TODO(spapini): Use the correct stable pointer.
    let arg_inputs = lower_exprs_as_vars(ctx, &expr.args, scope)?;
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
    let callee_implicit_types =
        ctx.db.function_all_implicits(expr.function).map_err(LoweringFlowError::Failed)?;
    let implicits = callee_implicit_types
        .iter()
        .map(|ty| scope.take_implicit(*ty))
        .collect::<Option<Vec<_>>>()
        .to_maybe()
        .map_err(LoweringFlowError::Failed)?;
    // TODO(orizi): Support ref args that are not the first arguments.
    let inputs = chain!(implicits, ref_inputs, arg_inputs.into_iter()).collect();

    // If the function is panic(), do something special.
    if expr.function == get_core_function_id(ctx.db.upcast(), "panic".into(), vec![]) {
        let [input] = <[_; 1]>::try_from(inputs).ok().unwrap();
        return lower_panic(ctx, scope, input);
    }

    // The following is relevant only to extern functions.
    if let Some(extern_function_id) = expr.function.try_get_extern_function_id(ctx.db.upcast()) {
        if let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Enum(concrete_enum_id)) =
            ctx.db.lookup_intern_type(expr.ty)
        {
            let lowered_expr = LoweredExprExternEnum {
                function: expr.function,
                concrete_enum_id,
                inputs,
                ref_args: expr.ref_args.clone(),
                implicits: callee_implicit_types,
            };

            if let Ok(refs) = ctx.db.extern_function_declaration_refs(extern_function_id) {
                if !refs.is_empty() {
                    // Don't optimize in case the extern function has ref parameters.
                    //
                    // TODO(yuval): This is a temporary measure as there is a problem when a match
                    // arm returns(moves) a variable that was passed to the
                    // libfunc call in the match as a reference. To fix it, we
                    // need: to ensure that if one arm uses a variable, all arms either use it or
                    // drop it (all refs must be passed to all arms as inputs). Then, if a var that
                    // was passed to the libfunc as a ref parameter is returned by one of the arms,
                    // it must be rebound to do that (today it is returned as the same var id).
                    return Ok(LoweredExpr::AtVariable(lowered_expr.var(ctx, scope)?));
                }
            }

            // It is still unknown whether we directly match on this enum result, or store it to a
            // variable. Thus we can't perform the call. Performing it and pushing/bringing-back
            // variables are done on the 2 places where this result is used:
            // 1. [lower_optimized_extern_match]
            // 2. [context::LoweredExprExternEnum::var]
            return Ok(LoweredExpr::ExternEnum(lowered_expr));
        }
    }

    let may_panic = ctx.db.function_may_panic(expr.function).map_err(LoweringFlowError::Failed)?;
    let expr_ty = if may_panic { get_panic_ty(ctx.db.upcast(), expr.ty) } else { expr.ty };

    let (implicit_outputs, ref_outputs, res) =
        perform_function_call(ctx, scope, expr.function, inputs, ref_tys, expr_ty)?;

    // Rebind the implicits.
    for (implicit_type, implicit_output) in zip_eq(callee_implicit_types, implicit_outputs) {
        scope.put_implicit(implicit_type, implicit_output);
    }
    // Rebind the ref variables.
    for (semantic_var_id, output_var) in zip_eq(&expr.ref_args, ref_outputs) {
        scope.put_semantic_variable(*semantic_var_id, output_var);
    }

    if may_panic {
        return lower_panic_error_propagate(ctx, scope, res, expr.ty);
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
) -> Result<(Vec<LivingVar>, Vec<LivingVar>, LoweredExpr), LoweringFlowError> {
    // If the function is not extern, simply call it.
    if function.try_get_extern_function_id(ctx.db.upcast()).is_none() {
        let call_result =
            generators::Call { function, inputs, ref_tys, ret_tys: vec![ret_ty] }.add(ctx, scope);
        let res = LoweredExpr::AtVariable(call_result.returns.into_iter().next().unwrap());
        return Ok((call_result.implicit_outputs, call_result.ref_outputs, res));
    };

    // Extern function.
    let ret_tys = extern_facade_return_tys(ctx, ret_ty);
    let call_result = generators::Call { function, inputs, ref_tys, ret_tys }.add(ctx, scope);
    Ok((
        call_result.implicit_outputs,
        call_result.ref_outputs,
        extern_facade_expr(ctx, ret_ty, call_result.returns),
    ))
}

/// Lowers a panic(data) expr.
fn lower_panic(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    data_var: LivingVar,
) -> Result<LoweredExpr, LoweringFlowError> {
    let func_err_variant = get_enum_concrete_variant(
        ctx.db.upcast(),
        "PanicResult",
        vec![GenericArgumentId::Type(ctx.signature.return_type)],
        "Err",
    );
    let value_var =
        generators::EnumConstruct { input: data_var, variant: func_err_variant }.add(ctx, scope);
    let res = get_plain_full_return_vars(ctx, scope, vec![value_var])?;
    Err(LoweringFlowError::Return(res))
}

/// Lowers an expression of type [semantic::ExprMatch].
fn lower_expr_match(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a match expression: {:?}", expr.debug(&ctx.expr_formatter));
    let lowered_expr = lower_expr(ctx, scope, expr.matched_expr)?;

    if ctx.function_def.exprs[expr.matched_expr].ty() == ctx.db.core_felt_ty() {
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
    let (res, mut finalized_merger) =
        BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| -> Result<_, LoweringFlowError> {
            // Create a sealed block for each arm.
            let block_opts =
                zip_eq(&concrete_variants, &expr.arms).map(|(concrete_variant, arm)| {
                    let input_tys = vec![concrete_variant.ty];

                    // Create a scope for the arm block.
                    merger.run_in_subscope(ctx, input_tys, |ctx, subscope, arm_inputs| {
                        // TODO(spapini): Convert to a diagnostic.
                        let enum_pattern =
                            extract_matches!(&arm.pattern, semantic::Pattern::EnumVariant);
                        // TODO(spapini): Convert to a diagnostic.
                        assert_eq!(&enum_pattern.variant, concrete_variant, "Wrong variant");

                        assert_eq!(arm_inputs.len(), 1);
                        let variant_expr =
                            LoweredExpr::AtVariable(arm_inputs.into_iter().next().unwrap());
                        match lower_single_pattern(
                            ctx,
                            subscope,
                            &enum_pattern.inner_pattern,
                            variant_expr,
                        ) {
                            Ok(_) => {
                                // Lower the arm expression.
                                lower_tail_expr(ctx, subscope, Some(arm.expression), false)
                            }
                            Err(err) => lowering_flow_error_to_block_scope_end(err),
                        }
                    })
                });
            block_opts.collect::<Maybe<Vec<_>>>().map_err(LoweringFlowError::Failed)
        });
    let finalized_blocks =
        res?.into_iter().map(|sealed| finalized_merger.finalize_block(ctx, sealed).block);

    let arms = zip_eq(concrete_variants, finalized_blocks).collect();

    // Emit the statement.
    let block_result = (generators::MatchEnum {
        input: expr_var,
        concrete_enum_id,
        arms,
        end_info: finalized_merger.end_info.clone(),
    })
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
}

/// Lowers a match expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_match(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    extern_enum: LoweredExprExternEnum,
    match_arms: &[semantic::MatchArm],
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Started lowering of an optimized extern match.");
    let concrete_variants = ctx.db.concrete_enum_variants(extern_enum.concrete_enum_id).unwrap();
    if match_arms.len() != concrete_variants.len() {
        return Err(LoweringFlowError::Failed(skip_diagnostic()));
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
                    let input_tys = match_extern_variant_arm_input_types(
                        ctx,
                        concrete_variant.ty,
                        &extern_enum,
                    );

                    // Create a scope for the arm block.
                    merger.run_in_subscope(ctx, input_tys, |ctx, subscope, mut arm_inputs| {
                        // TODO(spapini): Convert to a diagnostic.
                        let enum_pattern =
                            extract_matches!(&arm.pattern, semantic::Pattern::EnumVariant);
                        // TODO(spapini): Convert to a diagnostic.
                        assert_eq!(&enum_pattern.variant, concrete_variant, "Wrong variant");

                        // Bind the arm inputs to implicits and semantic variables.
                        match_extern_arm_ref_args_bind(&mut arm_inputs, &extern_enum, subscope);

                        let variant_expr = extern_facade_expr(ctx, concrete_variant.ty, arm_inputs);
                        match lower_single_pattern(
                            ctx,
                            subscope,
                            &enum_pattern.inner_pattern,
                            variant_expr,
                        ) {
                            Ok(_) => {
                                // Lower the arm expression.
                                lower_tail_expr(ctx, subscope, Some(arm.expression), false)
                            }
                            Err(err) => lowering_flow_error_to_block_scope_end(err),
                        }
                    })
                });
            block_opts.collect::<Maybe<Vec<_>>>().map_err(LoweringFlowError::Failed)
        },
    );

    let finalized_blocks = blocks?
        .into_iter()
        .map(|sealed| finalized_merger.finalize_block(ctx, sealed).block)
        .collect_vec();
    let arms = zip_eq(concrete_variants, finalized_blocks).collect();

    // Emit the statement.
    let block_result = generators::MatchExtern {
        function: extern_enum.function,
        inputs: extern_enum.inputs,
        arms,
        end_info: finalized_merger.end_info.clone(),
    }
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
}

/// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt.
/// Currently only a simple match-zero is supported.
fn lower_expr_match_felt(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMatch,
    expr_var: LivingVar,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a match-felt expression.");
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
    let (res, mut finalized_merger) = BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| {
        let block0_end = merger.run_in_subscope(ctx, vec![], |ctx, subscope, _| {
            lower_tail_expr(ctx, subscope, Some(*block0), false)
        });
        let non_zero_type = core_nonzero_ty(semantic_db, core_felt_ty(semantic_db));
        let block_otherwise_end =
            merger.run_in_subscope(ctx, vec![non_zero_type], |ctx, subscope, _| {
                lower_tail_expr(ctx, subscope, Some(*block_otherwise), false)
            });
        Ok((block0_end, block_otherwise_end))
    });
    let (block0_sealed, block_otherwise_sealed) = res.map_err(LoweringFlowError::Failed)?;
    let block0_finalized =
        finalized_merger.finalize_block(ctx, block0_sealed.map_err(LoweringFlowError::Failed)?);
    let block_otherwise_finalized = finalized_merger
        .finalize_block(ctx, block_otherwise_sealed.map_err(LoweringFlowError::Failed)?);

    let concrete_variants =
        vec![jump_nz_zero_variant(ctx.db.upcast()), jump_nz_nonzero_variant(ctx.db.upcast())];
    let arms = zip_eq(concrete_variants, [block0_finalized.block, block_otherwise_finalized.block])
        .collect();

    // Emit the statement.
    let block_result = (generators::MatchExtern {
        function: core_jump_nz_func(semantic_db),
        inputs: vec![expr_var],
        arms,
        end_info: finalized_merger.end_info.clone(),
    })
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
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
        .map(|arg_expr_id| lower_expr(ctx, scope, *arg_expr_id)?.var(ctx, scope))
        .collect::<Result<Vec<_>, _>>()
}

/// Lowers an expression of type [semantic::ExprEnumVariantCtor].
fn lower_expr_enum_ctor(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprEnumVariantCtor,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!(
        "Started lowering of an enum c'tor expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    Ok(LoweredExpr::AtVariable(
        generators::EnumConstruct {
            input: lower_expr(ctx, scope, expr.value_expr)?.var(ctx, scope)?,
            variant: expr.variant.clone(),
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprMemberAccess].
fn lower_expr_member_access(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprMemberAccess,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a member-access expression: {:?}", expr.debug(&ctx.expr_formatter));
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
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprStructCtor].
fn lower_expr_struct_ctor(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprStructCtor,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Lowering a struct c'tor expression: {:?}", expr.debug(&ctx.expr_formatter));
    let members = ctx.db.struct_members(expr.struct_id).map_err(LoweringFlowError::Failed)?;
    let member_expr = UnorderedHashMap::from_iter(expr.members.iter().cloned());
    Ok(LoweredExpr::AtVariable(
        generators::StructConstruct {
            inputs: members
                .into_iter()
                .map(|(_, member)| lower_expr(ctx, scope, member_expr[member.id])?.var(ctx, scope))
                .collect::<Result<Vec<_>, _>>()?,
            ty: expr.ty,
        }
        .add(ctx, scope),
    ))
}

/// Lowers an expression of type [semantic::ExprPropagateError].
fn lower_panic_error_propagate(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    lowered_expr: LoweredExpr,
    ty: semantic::TypeId,
) -> Result<LoweredExpr, LoweringFlowError> {
    let ok_variant = get_enum_concrete_variant(
        ctx.db.upcast(),
        "PanicResult",
        vec![GenericArgumentId::Type(ty)],
        "Ok",
    );
    let err_variant = get_enum_concrete_variant(
        ctx.db.upcast(),
        "PanicResult",
        vec![GenericArgumentId::Type(ty)],
        "Err",
    );
    let func_err_variant = get_enum_concrete_variant(
        ctx.db.upcast(),
        "PanicResult",
        vec![GenericArgumentId::Type(ctx.signature.return_type)],
        "Err",
    );
    lower_error_propagate(
        ctx,
        scope,
        lowered_expr,
        &ok_variant,
        &err_variant,
        &func_err_variant,
        true,
    )
}

/// Lowers an expression of type [semantic::ExprPropagateError].
fn lower_expr_error_propagate(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprPropagateError,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!(
        "Started lowering of an error-propagate expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    let lowered_expr = lower_expr(ctx, scope, expr.inner)?;
    lower_error_propagate(
        ctx,
        scope,
        lowered_expr,
        &expr.ok_variant,
        &expr.err_variant,
        &expr.func_err_variant,
        false,
    )
}

/// Lowers an error propagation.
fn lower_error_propagate(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    lowered_expr: LoweredExpr,
    ok_variant: &semantic::ConcreteVariant,
    err_variant: &semantic::ConcreteVariant,
    func_err_variant: &semantic::ConcreteVariant,
    panic_error: bool,
) -> Result<LoweredExpr, LoweringFlowError> {
    if let LoweredExpr::ExternEnum(extern_enum) = lowered_expr {
        return lower_optimized_extern_error_propagate(
            ctx,
            scope,
            extern_enum,
            ok_variant,
            err_variant,
            func_err_variant,
            panic_error,
        );
    }

    let var = lowered_expr.var(ctx, scope)?;
    // Merge arm blocks.
    let (res, mut finalized_merger) =
        BlockFlowMerger::with(ctx, scope, &[], |ctx, merger| -> Result<_, LoweringFlowError> {
            Ok([
                merger
                    .run_in_subscope(ctx, vec![ok_variant.ty], |_ctx, _subscope, arm_inputs| {
                        let [var] = <[_; 1]>::try_from(arm_inputs).ok().unwrap();
                        Ok(BlockScopeEnd::Callsite(Some(var)))
                    })
                    .map_err(LoweringFlowError::Failed)?,
                merger
                    .run_in_subscope(ctx, vec![err_variant.ty], |ctx, subscope, arm_inputs| {
                        let [var] = <[_; 1]>::try_from(arm_inputs).ok().unwrap();
                        let value_var = generators::EnumConstruct {
                            input: var,
                            variant: func_err_variant.clone(),
                        }
                        .add(ctx, subscope);
                        let res = if panic_error {
                            get_plain_full_return_vars(ctx, subscope, vec![value_var])
                                .ok()
                                .to_maybe()?
                        } else {
                            get_full_return_vars(ctx, subscope, LoweredExpr::AtVariable(value_var))
                                .ok()
                                .to_maybe()?
                        };
                        Ok(BlockScopeEnd::Return(res))
                    })
                    .map_err(LoweringFlowError::Failed)?,
            ])
        });
    let finalized_blocks = res?.map(|sealed| finalized_merger.finalize_block(ctx, sealed).block);

    let arms = zip_eq([ok_variant.clone(), err_variant.clone()], finalized_blocks).collect();

    // Emit the statement.
    let block_result = (generators::MatchEnum {
        input: var,
        concrete_enum_id: ok_variant.concrete_enum_id,
        arms,
        end_info: finalized_merger.end_info.clone(),
    })
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
}

/// Lowers an error propagation expression on a LoweredExpr::ExternEnum lowered expression.
fn lower_optimized_extern_error_propagate(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    extern_enum: LoweredExprExternEnum,
    ok_variant: &semantic::ConcreteVariant,
    err_variant: &semantic::ConcreteVariant,
    func_err_variant: &semantic::ConcreteVariant,
    panic_error: bool,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!("Started lowering of an optimized error-propagate expression.");
    let (blocks, mut finalized_merger) = BlockFlowMerger::with(
        ctx,
        scope,
        &extern_enum.ref_args,
        |ctx, merger| -> Result<_, LoweringFlowError> {
            Ok([
                {
                    let input_tys =
                        match_extern_variant_arm_input_types(ctx, ok_variant.ty, &extern_enum);
                    merger
                        .run_in_subscope(ctx, input_tys, |ctx, subscope, mut arm_inputs| {
                            match_extern_arm_ref_args_bind(&mut arm_inputs, &extern_enum, subscope);

                            let variant_expr = extern_facade_expr(ctx, ok_variant.ty, arm_inputs);
                            Ok(BlockScopeEnd::Callsite(Some(
                                variant_expr.var(ctx, subscope).ok().to_maybe()?,
                            )))
                        })
                        .map_err(LoweringFlowError::Failed)?
                },
                {
                    let input_tys =
                        match_extern_variant_arm_input_types(ctx, err_variant.ty, &extern_enum);
                    merger
                        .run_in_subscope(ctx, input_tys, |ctx, subscope, mut arm_inputs| {
                            match_extern_arm_ref_args_bind(&mut arm_inputs, &extern_enum, subscope);
                            let variant_expr = extern_facade_expr(ctx, err_variant.ty, arm_inputs);
                            let input = variant_expr.var(ctx, subscope).ok().to_maybe()?;
                            let value_var = generators::EnumConstruct {
                                input,
                                variant: func_err_variant.clone(),
                            }
                            .add(ctx, subscope);
                            let res = if panic_error {
                                get_plain_full_return_vars(ctx, subscope, vec![value_var])
                                    .ok()
                                    .to_maybe()?
                            } else {
                                get_full_return_vars(
                                    ctx,
                                    subscope,
                                    LoweredExpr::AtVariable(value_var),
                                )
                                .ok()
                                .to_maybe()?
                            };
                            Ok(BlockScopeEnd::Return(res))
                        })
                        .map_err(LoweringFlowError::Failed)?
                },
            ])
        },
    );
    let finalized_blocks =
        blocks?.map(|sealed| finalized_merger.finalize_block(ctx, sealed).block).to_vec();
    let arms = zip_eq(vec![ok_variant.clone(), err_variant.clone()], finalized_blocks).collect();

    let block_result = generators::MatchExtern {
        function: extern_enum.function,
        inputs: extern_enum.inputs,
        arms,
        end_info: finalized_merger.end_info.clone(),
    }
    .add(ctx, scope);
    lowered_expr_from_block_result(scope, block_result, finalized_merger)
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
    arm_inputs: &mut Vec<LivingVar>,
    extern_enum: &LoweredExprExternEnum,
    subscope: &mut BlockScope,
) {
    let implicit_outputs: Vec<_> = arm_inputs.drain(0..extern_enum.implicits.len()).collect();
    // Bind the implicits.
    for (ty, output_var) in zip_eq(&extern_enum.implicits, implicit_outputs) {
        subscope.put_implicit(*ty, output_var);
    }
    let ref_outputs: Vec<_> = arm_inputs.drain(0..extern_enum.ref_args.len()).collect();
    // Bind the ref variables.
    for (semantic_var_id, output_var) in zip_eq(&extern_enum.ref_args, ref_outputs) {
        subscope.put_semantic_variable(*semantic_var_id, output_var);
    }
}

/// Lowers an expression of type [semantic::ExprAssignment].
fn lower_expr_assignment(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprAssignment,
    scope: &mut BlockScope,
) -> Result<LoweredExpr, LoweringFlowError> {
    log::trace!(
        "Started lowering of an assignment expression: {:?}",
        expr.debug(&ctx.expr_formatter)
    );
    scope.try_ensure_semantic_variable(ctx, expr.var);
    let var = lower_expr(ctx, scope, expr.rhs)?.var(ctx, scope)?;
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
    scope
        .use_semantic_variable(ctx, semantic_var)
        .take_var()
        .ok_or_else(|| LoweringFlowError::Failed(ctx.diagnostics.report(stable_ptr, VariableMoved)))
}

/// Retrieves a LivingVar that corresponds to a semantic var in the current scope.
/// Always moves. If it is already moved, fails and emits a diagnostic.
fn take_semantic_var(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    semantic_var: semantic::VarId,
    stable_ptr: SyntaxStablePtrId,
) -> Result<LivingVar, LoweringFlowError> {
    scope
        .take_semantic_variable(ctx, semantic_var)
        .take_var()
        .ok_or_else(|| LoweringFlowError::Failed(ctx.diagnostics.report(stable_ptr, VariableMoved)))
}

/// Converts a CallBlockResult for a LoweredExpr.
/// Some statements end with a CallBlockResult (CallBlock, Match, etc..), which represents all
/// the information of the "ending" of the call.
/// Binds the semantic variables from the call
/// Returns the proper flow error if needed.
fn lowered_expr_from_block_result(
    scope: &mut BlockScope,
    block_result: generators::CallBlockResult,
    finalized_merger: BlockMergerFinalized,
) -> Result<LoweredExpr, LoweringFlowError> {
    match block_result {
        generators::CallBlockResult::Callsite { maybe_output, pushes } => {
            let mut pushes_iter = pushes.into_iter();
            for implicit_type in finalized_merger.outer_implicit_info.pushes {
                let var = pushes_iter.next().unwrap();
                scope.put_implicit(implicit_type, var);
                scope.mark_implicit_changed(implicit_type);
            }
            for (semantic_var_id, var) in
                zip_eq(finalized_merger.outer_var_info.pushes, pushes_iter)
            {
                scope.put_semantic_variable(semantic_var_id, var);
            }

            // Bring back the unused implicits.
            for (ty, implicit_var) in finalized_merger.outer_implicit_info.unchanged {
                scope.put_implicit(ty, implicit_var);
            }

            // Bring back the untouched semantic vars.
            for (semantic_var_id, var) in finalized_merger.outer_var_info.bring_back {
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
