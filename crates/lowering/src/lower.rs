use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use itertools::{chain, zip_eq};
use scope::{BlockScope, BlockScopeEnd};
use semantic::corelib::{
    core_bool_enum, core_felt_ty, core_jump_nz_func, core_nonzero_ty, false_variant, true_variant,
    unit_ty,
};
use semantic::db::SemanticGroup;
use semantic::items::enm::SemanticEnumEx;
use semantic::{ConcreteTypeId, TypeLongId};
use syntax::node::ids::SyntaxStablePtrId;
use utils::unordered_hash_map::UnorderedHashMap;
use utils::{extract_matches, try_extract_matches};

use self::context::LoweringContext;
use self::scope::{generators, BlockFlowMerger, ContextLender};
use self::variables::LivingVar;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{Block, BlockId, Variable};

mod context;
mod scope;
mod semantic_map;
mod variables;

/// Context for lowering a function.
pub struct Lowerer<'db> {
    #[allow(dead_code)]
    ref_params: &'db [semantic::VarId],
    ctx: LoweringContext<'db>,
}

impl<'db> ContextLender<'db> for Lowerer<'db> {
    fn ctx(&mut self) -> &mut LoweringContext<'db> {
        &mut self.ctx
    }
}

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

impl<'db> Lowerer<'db> {
    /// Lowers a semantic free function.
    pub fn lower(db: &dyn SemanticGroup, free_function_id: FreeFunctionId) -> Option<Lowered> {
        let function_def = db.free_function_definition(free_function_id)?;
        let ctx = LoweringContext {
            db,
            function_def: &*function_def,
            diagnostics: LoweringDiagnostics::new(free_function_id.module(db.upcast())),
            variables: Arena::default(),
            blocks: Arena::default(),
            semantic_defs: UnorderedHashMap::default(),
        };

        let signature = db.free_function_declaration_signature(free_function_id)?;

        // Params.
        let input_semantic_vars: Vec<_> =
            signature.params.iter().map(|param| semantic::Variable::Param(param.clone())).collect();
        let (input_semantic_var_ids, input_semantic_var_tys): (Vec<_>, Vec<_>) =
            input_semantic_vars
                .iter()
                .map(|semantic_var| (semantic_var.id(), semantic_var.ty()))
                .unzip();
        let ref_params: Vec<_> = input_semantic_vars
            .iter()
            .filter_map(|var| if var.modifiers().is_ref { Some(var.id()) } else { None })
            .collect();
        let mut lowerer = Lowerer { ref_params: &ref_params, ctx };

        // TODO(spapini): Build semantic_defs in semantic model.
        for semantic_var in input_semantic_vars {
            lowerer.ctx.semantic_defs.insert(semantic_var.id(), semantic_var);
        }

        // Fetch body block expr.
        let semantic_block =
            extract_matches!(&function_def.exprs[function_def.body], semantic::Expr::Block);
        // Lower block to a BlockSealed.
        let (block_sealed_opt, mut merger_finalized) =
            BlockFlowMerger::with_root(&mut lowerer, &ref_params, |lowerer, merger| {
                merger.run_in_subscope(
                    lowerer,
                    input_semantic_var_tys,
                    |lowerer, scope, variables| {
                        // Initialize params.
                        for (semantic_var_id, var) in zip_eq(input_semantic_var_ids, variables) {
                            scope.put_semantic_variable(semantic_var_id, var);
                        }
                        lowerer.lower_block(scope, semantic_block)
                    },
                )
            });
        // Root block must not push anything.
        let root = block_sealed_opt.map(|block_sealed| {
            merger_finalized.finalize_block(&mut lowerer.ctx, block_sealed).block
        });
        let LoweringContext { diagnostics, variables, blocks, .. } = lowerer.ctx;
        Some(Lowered { diagnostics: diagnostics.build(), root, variables, blocks })
    }

    /// Lowers a semantic block.
    fn lower_block(
        &mut self,
        scope: &mut BlockScope,
        expr_block: &semantic::ExprBlock,
    ) -> Option<BlockScopeEnd> {
        for (i, stmt_id) in expr_block.statements.iter().enumerate() {
            let stmt = &self.ctx.function_def.statements[*stmt_id];
            let lowered_stmt = self.lower_statement(scope, stmt);

            // If flow is not reachable anymore, no need to continue emitting statements.
            match lowered_stmt {
                Ok(()) => {}
                Err(StatementLoweringFlowError::Failed) => return None,
                Err(StatementLoweringFlowError::End(end)) => {
                    // TODO(spapini): We might want to report unreachable for expr that abruptly
                    // ends, e.g. `5 + {return; 6}`.
                    if i + 1 < expr_block.statements.len() {
                        let start_stmt =
                            &self.ctx.function_def.statements[expr_block.statements[i + 1]];
                        let end_stmt = &self.ctx.function_def.statements
                            [*expr_block.statements.last().unwrap()];
                        // Emit diagnostic fo the rest of the statements with unreachable.
                        self.ctx.diagnostics.report(
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
            Some(tail_expr) => self.lower_tail_expr(scope, tail_expr),
            None => Some(BlockScopeEnd::Callsite(None)),
        }
    }

    /// Lowers an expression that is either a complete block, or the end (tail expreesion) of a
    /// block.
    pub fn lower_tail_expr(
        &mut self,
        scope: &mut BlockScope,
        expr: semantic::ExprId,
    ) -> Option<BlockScopeEnd> {
        let lowered_expr = self.lower_expr(scope, expr);
        Some(match lowered_expr {
            Ok(LoweredExpr::AtVariable(var)) => BlockScopeEnd::Callsite(Some(var)),
            Ok(LoweredExpr::Unit) => BlockScopeEnd::Callsite(None),
            Err(LoweringFlowError::Unreachable) => BlockScopeEnd::Unreachable,
            Err(LoweringFlowError::Failed) => {
                return None;
            }
        })
    }

    /// Lowers a semantic statement.
    pub fn lower_statement(
        &mut self,
        scope: &mut BlockScope,
        stmt: &semantic::Statement,
    ) -> Result<(), StatementLoweringFlowError> {
        match stmt {
            semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
                self.lower_expr(scope, *expr)?;
            }
            semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
                let lowered_expr = self.lower_expr(scope, *expr)?;
                let var = match lowered_expr {
                    LoweredExpr::AtVariable(var) => var,
                    LoweredExpr::Unit => self.unit_var(scope),
                };
                self.lower_single_pattern(scope, pattern, var)
            }
            semantic::Statement::Return(semantic::StatementReturn { expr, stable_ptr: _ }) => {
                // Lower return expr.
                let lowered_expr = self.lower_expr(scope, *expr)?;
                let value_vars = match lowered_expr {
                    LoweredExpr::AtVariable(var) => vec![var],
                    LoweredExpr::Unit => vec![],
                };
                // Find variables to output for ref vars.
                let ref_vars = self
                    .ref_params
                    .iter()
                    .map(|semantic_var_id| {
                        self.use_semantic_var(
                            scope,
                            *semantic_var_id,
                            semantic_var_id.untyped_stable_ptr(self.ctx.db.upcast()),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let return_vars = chain!(value_vars, ref_vars).collect();
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
        &mut self,
        scope: &mut BlockScope,
        pattern: &semantic::Pattern,
        var: LivingVar,
    ) {
        match pattern {
            semantic::Pattern::Literal(_) => unreachable!(),
            semantic::Pattern::Variable(semantic::PatternVariable { name: _, var: sem_var }) => {
                let sem_var = semantic::Variable::Local(sem_var.clone());
                // Deposit the owned variable in the semantic variables store.
                scope.put_semantic_variable(sem_var.id(), var);
                // TODO(spapini): Build semantic_defs in semantic model.
                self.ctx.semantic_defs.insert(sem_var.id(), sem_var);
            }
            semantic::Pattern::Struct(_) => todo!(),
            semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty }) => {
                let tys = extract_matches!(self.ctx.db.lookup_intern_type(*ty), TypeLongId::Tuple);
                let outputs =
                    generators::TupleDestruct { input: var, tys }.add(&mut self.ctx, scope);
                for (var, pattern) in zip_eq(outputs, field_patterns) {
                    self.lower_single_pattern(scope, pattern, var);
                }
            }
            semantic::Pattern::Enum(_) => unreachable!(),
            semantic::Pattern::Otherwise(_) => {}
        }
    }

    /// Lowers a semantic expression.
    fn lower_expr(
        &mut self,
        scope: &mut BlockScope,
        expr_id: semantic::ExprId,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        let expr = &self.ctx.function_def.exprs[expr_id];
        match expr {
            semantic::Expr::Tuple(expr) => self.lower_expr_tuple(expr, scope),
            semantic::Expr::Assignment(expr) => self.lower_expr_assignment(expr, scope),
            semantic::Expr::Block(expr) => self.lower_expr_block(scope, expr),
            semantic::Expr::FunctionCall(expr) => self.lower_expr_function_call(expr, scope),
            semantic::Expr::Match(expr) => self.lower_expr_match(expr, scope),
            semantic::Expr::If(expr) => self.lower_expr_if(scope, expr),
            semantic::Expr::Var(expr) => Ok(LoweredExpr::AtVariable(self.use_semantic_var(
                scope,
                expr.var,
                expr.stable_ptr.untyped(),
            )?)),
            semantic::Expr::Literal(expr) => Ok(LoweredExpr::AtVariable(
                generators::Literal { value: expr.value, ty: expr.ty }.add(&mut self.ctx, scope),
            )),
            semantic::Expr::MemberAccess(_) => todo!(),
            semantic::Expr::StructCtor(_) => todo!(),
            semantic::Expr::EnumVariantCtor(expr) => self.lower_expr_enum_ctor(expr, scope),
            semantic::Expr::Missing(_) => Err(LoweringFlowError::Failed),
        }
    }

    /// Lowers an expression of type [semantic::ExprTuple].
    fn lower_expr_tuple(
        &mut self,
        expr: &semantic::ExprTuple,
        scope: &mut BlockScope,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        let inputs = self.lower_exprs_as_vars(&expr.items, scope)?;
        Ok(LoweredExpr::AtVariable(
            generators::TupleConstruct { inputs, ty: expr.ty }.add(&mut self.ctx, scope),
        ))
    }

    /// Lowers an expression of type [semantic::ExprBlock].
    fn lower_expr_block(
        &mut self,
        scope: &mut BlockScope,
        expr: &semantic::ExprBlock,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        let (block_sealed, mut finalized_merger) =
            BlockFlowMerger::with(self, scope, |lowerer, merger| {
                merger.run_in_subscope(lowerer, vec![], |lowerer, subscope, _| {
                    lowerer.lower_block(subscope, expr)
                })
            });
        let block_sealed = block_sealed.ok_or(LoweringFlowError::Failed)?;
        let block_finalized = finalized_merger.finalize_block(&mut self.ctx, block_sealed);

        // Emit the statement.
        let call_block_generator = generators::CallBlock {
            block: block_finalized.block,
            end_info: finalized_merger.end_info,
        };
        let block_result = call_block_generator.add(&mut self.ctx, scope);
        lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
    }

    /// Lowers an expression of type [semantic::ExprFunctionCall].
    fn lower_expr_function_call(
        &mut self,
        expr: &semantic::ExprFunctionCall,
        scope: &mut BlockScope,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        // TODO(spapini): Use the correct stable pointer.
        let (ref_tys, ref_inputs): (_, Vec<LivingVar>) = expr
            .ref_args
            .iter()
            .map(|semantic_var_id| {
                Ok((
                    self.ctx.semantic_defs[*semantic_var_id].ty(),
                    self.use_semantic_var(scope, *semantic_var_id, expr.stable_ptr.untyped())?,
                ))
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .unzip();
        let arg_inputs = self.lower_exprs_as_vars(&expr.args, scope)?;
        let inputs = chain!(ref_inputs, arg_inputs.into_iter()).collect();
        let call_result =
            generators::Call { function: expr.function, inputs, ref_tys, ret_ty: expr.ty }
                .add(&mut self.ctx, scope);
        // Rebind the ref variables.
        for (semantic_var_id, output_var) in zip_eq(&expr.ref_args, call_result.ref_outputs) {
            scope.put_semantic_variable(*semantic_var_id, output_var);
        }
        Ok(LoweredExpr::AtVariable(call_result.output))
    }

    /// Lowers an expression of type [semantic::ExprMatch].
    fn lower_expr_match(
        &mut self,
        expr: &semantic::ExprMatch,
        scope: &mut BlockScope,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        let expr_var =
            extract_matches!(self.lower_expr(scope, expr.matched_expr)?, LoweredExpr::AtVariable);

        if self.ctx.function_def.exprs[expr.matched_expr].ty() == self.ctx.db.core_felt_ty() {
            return self.lower_expr_match_felt(expr, expr_var, scope);
        }

        // TODO(spapini): Use diagnostics.
        // TODO(spapini): Handle more than just enums.
        let (concrete_enum_id, concrete_variants) = self.extract_concrete_enum(expr)?;

        // Merge arm blocks.
        let (res, mut finalized_merger) =
            BlockFlowMerger::with(self, scope, |lowerer, merger| -> Result<_, LoweringFlowError> {
                // Create a sealed block for each arm.
                let block_opts =
                    zip_eq(&concrete_variants, &expr.arms).map(|(concrete_variant, arm)| {
                        let semantic_var_id = extract_var_pattern(&arm.pattern, concrete_variant)?;
                        // Create a scope for the arm block.
                        merger.run_in_subscope(
                            lowerer,
                            vec![concrete_variant.ty],
                            |lowerer, subscope, variables| {
                                // Bind the arm input variable to the semantic variable.
                                let [var] = <[_; 1]>::try_from(variables).ok().unwrap();
                                subscope.put_semantic_variable(semantic_var_id, var);

                                // Lower the block.
                                lowerer.lower_block(
                                    subscope,
                                    extract_matches!(
                                        &lowerer.ctx.function_def.exprs[arm.expression],
                                        semantic::Expr::Block
                                    ),
                                )
                            },
                        )
                    });
                block_opts.collect::<Option<Vec<_>>>().ok_or(LoweringFlowError::Failed)
            });
        let finalized_blocks = res?
            .into_iter()
            .map(|sealed| finalized_merger.finalize_block(&mut self.ctx, sealed).block);

        let arms = zip_eq(concrete_variants, finalized_blocks).collect();

        // Emit the statement.
        let match_generator = generators::MatchEnum {
            input: expr_var,
            concrete_enum_id,
            arms,
            end_info: finalized_merger.end_info,
        };
        let block_result = match_generator.add(&mut self.ctx, scope);
        lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
    }

    /// Lowers an expression of type [semantic::ExprMatch] where the matched expression is a felt.
    /// Currently only a simple match-zero is supported.
    fn lower_expr_match_felt(
        &mut self,
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
            self.ctx.diagnostics.report(expr.stable_ptr.untyped(), OnlyMatchZeroIsSupported);
            return Err(LoweringFlowError::Failed);
        };

        // Make sure literal is 0.
        if literal.value != 0 {
            self.ctx.diagnostics.report(literal.stable_ptr.untyped(), NonZeroValueInMatch);
            return Err(LoweringFlowError::Failed);
        }

        // Lower both blocks.
        let (res, mut finalized_merger) = BlockFlowMerger::with(self, scope, |lowerer, merger| {
            let block0_end = merger.run_in_subscope(lowerer, vec![], |lowerer, subscope, _| {
                lowerer.lower_tail_expr(subscope, *block0)
            });
            let non_zero_type = core_nonzero_ty(self.ctx.db, core_felt_ty(self.ctx.db));
            let block_otherwise_end =
                merger.run_in_subscope(lowerer, vec![non_zero_type], |lowerer, subscope, _| {
                    lowerer.lower_tail_expr(subscope, *block_otherwise)
                });
            Some((block0_end, block_otherwise_end))
        });
        let (block0_sealed, block_otherwise_sealed) = res.ok_or(LoweringFlowError::Failed)?;
        let block0_finalized = finalized_merger
            .finalize_block(&mut self.ctx, block0_sealed.ok_or(LoweringFlowError::Failed)?);
        let block_otherwise_finalized = finalized_merger.finalize_block(
            &mut self.ctx,
            block_otherwise_sealed.ok_or(LoweringFlowError::Failed)?,
        );

        // Emit the statement.
        let match_generator = generators::MatchExtern {
            function: core_jump_nz_func(self.ctx.db),
            inputs: vec![expr_var],
            arms: vec![block0_finalized.block, block_otherwise_finalized.block],
            end_info: finalized_merger.end_info,
        };
        let block_result = match_generator.add(&mut self.ctx, scope);
        lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
    }

    /// Lowers an expression of type [semantic::ExprIf].
    fn lower_expr_if(
        &mut self,
        scope: &mut BlockScope,
        expr: &semantic::ExprIf,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        // The condition cannot be unit.
        let condition_var =
            extract_matches!(self.lower_expr(scope, expr.condition)?, LoweredExpr::AtVariable);

        // Lower both blocks.
        let unit_ty = unit_ty(self.ctx.db);
        let (res, mut finalized_merger) = BlockFlowMerger::with(self, scope, |lowerer, merger| {
            let [main_block_scope, else_block_scope] =
                [expr.if_block, expr.else_block].map(|block_expr| {
                    merger.run_in_subscope(lowerer, vec![unit_ty], |lowerer, subscope, _| {
                        lowerer.lower_block(
                            subscope,
                            extract_matches!(
                                &lowerer.ctx.function_def.exprs[block_expr],
                                semantic::Expr::Block
                            ),
                        )
                    })
                });
            Some((main_block_scope, else_block_scope))
        });
        let (main_block_sealed, else_block_sealed) = res.ok_or(LoweringFlowError::Failed)?;
        let main_finalized = finalized_merger
            .finalize_block(&mut self.ctx, main_block_sealed.ok_or(LoweringFlowError::Failed)?);
        let else_finalized = finalized_merger
            .finalize_block(&mut self.ctx, else_block_sealed.ok_or(LoweringFlowError::Failed)?);

        // Emit the statement.
        let match_generator = generators::MatchEnum {
            input: condition_var,
            concrete_enum_id: core_bool_enum(self.ctx.db),
            arms: vec![
                (true_variant(self.ctx.db), main_finalized.block),
                (false_variant(self.ctx.db), else_finalized.block),
            ],
            end_info: finalized_merger.end_info,
        };
        let block_result = match_generator.add(&mut self.ctx, scope);
        lowered_expr_from_block_result(scope, block_result, finalized_merger.pushes)
    }

    /// Extracts concrete enum and variants from a match expression. Assumes it is indeed a concrete
    /// enum.
    fn extract_concrete_enum(
        &mut self,
        expr: &semantic::ExprMatch,
    ) -> Result<(semantic::ConcreteEnumId, Vec<semantic::ConcreteVariant>), LoweringFlowError> {
        let concrete_ty = try_extract_matches!(
            self.ctx.db.lookup_intern_type(self.ctx.function_def.exprs[expr.matched_expr].ty()),
            TypeLongId::Concrete
        )
        .ok_or(LoweringFlowError::Failed)?;
        let concrete_enum_id = try_extract_matches!(concrete_ty, ConcreteTypeId::Enum)
            .ok_or(LoweringFlowError::Failed)?;
        let enum_id = concrete_enum_id.enum_id(self.ctx.db);
        let variants = self.ctx.db.enum_variants(enum_id).ok_or(LoweringFlowError::Failed)?;
        let concrete_variants = variants
            .values()
            .map(|variant_id| {
                let variant = self
                    .ctx
                    .db
                    .variant_semantic(enum_id, *variant_id)
                    .ok_or(LoweringFlowError::Failed)?;

                self.ctx
                    .db
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
        &mut self,
        exprs: &[semantic::ExprId],
        scope: &mut BlockScope,
    ) -> Result<Vec<LivingVar>, LoweringFlowError> {
        exprs
            .iter()
            .map(|arg_expr_id| Ok(self.lower_expr(scope, *arg_expr_id)?.var(self, scope)))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Introduces a unit variable inplace.
    fn unit_var(&mut self, scope: &mut BlockScope) -> LivingVar {
        generators::TupleConstruct { inputs: vec![], ty: unit_ty(self.ctx.db) }
            .add(&mut self.ctx, scope)
    }

    /// Lowers an expression of type [semantic::ExprEnumVariantCtor].
    fn lower_expr_enum_ctor(
        &mut self,
        expr: &semantic::ExprEnumVariantCtor,
        scope: &mut BlockScope,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        Ok(LoweredExpr::AtVariable(
            generators::EnumConstruct {
                input: self.lower_expr(scope, expr.value_expr)?.var(self, scope),
                variant: expr.variant.clone(),
            }
            .add(&mut self.ctx, scope),
        ))
    }

    /// Lowers an expression of type [semantic::ExprAssignment].
    fn lower_expr_assignment(
        &mut self,
        expr: &semantic::ExprAssignment,
        scope: &mut BlockScope,
    ) -> Result<LoweredExpr, LoweringFlowError> {
        scope.try_ensure_semantic_variable(&mut self.ctx, expr.var);
        let var = self.lower_expr(scope, expr.rhs)?.var(self, scope);
        scope.put_semantic_variable(expr.var, var);
        Ok(LoweredExpr::Unit)
    }

    /// Retrieves a LivingVar that corresponds to a semantic var in the current scope.
    /// Moves it if necessary. If it is already moved, fails and emits a diagnostic.
    fn use_semantic_var(
        &mut self,
        scope: &mut BlockScope,
        semantic_var: semantic::VarId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Result<LivingVar, LoweringFlowError> {
        scope.use_semantic_variable(&mut self.ctx, semantic_var).take_var().ok_or_else(|| {
            self.ctx.diagnostics.report(stable_ptr, VariableMoved);
            LoweringFlowError::Failed
        })
    }
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
                None => LoweredExpr::Unit,
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

/// Representation of the value of a computed expression.
#[derive(Debug)]
enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(LivingVar),
    /// The expression value is unit.
    Unit,
}
impl LoweredExpr {
    fn var(self, lowerer: &mut Lowerer<'_>, scope: &mut BlockScope) -> LivingVar {
        match self {
            LoweredExpr::AtVariable(var_id) => var_id,
            LoweredExpr::Unit => lowerer.unit_var(scope),
        }
    }
}

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed,
    /// The current computation is unreachable.
    Unreachable,
}
/// Cases where the flow of lowering a statement should halt.
pub enum StatementLoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed,
    /// The block should end after this statement.
    End(BlockScopeEnd),
}
impl From<LoweringFlowError> for StatementLoweringFlowError {
    fn from(err: LoweringFlowError) -> Self {
        match err {
            LoweringFlowError::Failed => StatementLoweringFlowError::Failed,
            LoweringFlowError::Unreachable => {
                StatementLoweringFlowError::End(BlockScopeEnd::Unreachable)
            }
        }
    }
}
