use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use itertools::zip_eq;
use scope::{BlockScope, BlockScopeEnd, OwnedVariable};
use semantic::corelib::unit_ty;
use semantic::db::SemanticGroup;
use semantic::TypeLongId;
use utils::extract_matches;
use utils::unordered_hash_map::UnorderedHashMap;

use self::context::LoweringContext;
use self::scope::{generators, BlockSealed};
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{Block, BlockId, Variable};

mod context;
mod scope;
mod semantic_map;

/// Context for lowering a function.
pub struct Lowerer<'db> {
    /// Semantic model for current function definition.
    function_def: &'db semantic::FreeFunctionDefinition,
    ctx: LoweringContext<'db>,
}

/// A lowered function code.
pub struct Lowered {
    /// Diagnostics produced while lowering.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    /// Block id for the start of the lwoered function.
    pub root: BlockId,
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
            diagnostics: LoweringDiagnostics::new(free_function_id.module(db.upcast())),
            variables: Arena::default(),
            blocks: Arena::default(),
            semantic_defs: UnorderedHashMap::default(),
        };
        let mut lowerer = Lowerer { function_def: &*function_def, ctx };

        let signature = db.free_function_declaration_signature(free_function_id)?;

        // Params.
        let input_semantic_vars: Vec<_> =
            signature.params.iter().map(|param| semantic::Variable::Param(param.clone())).collect();
        let input_semantic_var_ids: Vec<_> =
            input_semantic_vars.iter().map(|semantic_var| semantic_var.id()).collect();
        // TODO(spapini): Build semantic_defs in semantic model.
        for semantic_var in input_semantic_vars {
            lowerer.ctx.semantic_defs.insert(semantic_var.id(), semantic_var);
        }

        // Fetch body block expr.
        let semantic_block =
            extract_matches!(&function_def.exprs[function_def.body], semantic::Expr::Block);
        // Lower block to a BlockSealed.
        // TODO(spapini): The `?` here is incorrect. In case of failure we should still return an
        //   object with the correct diagnostics.
        let root_sealed_block = lowerer.lower_block(semantic_block)?;
        let root_block = root_sealed_block.finalize(&mut lowerer.ctx, &input_semantic_var_ids, &[]);
        let root = lowerer.ctx.blocks.alloc(root_block);
        let LoweringContext { diagnostics, variables, blocks, .. } = lowerer.ctx;
        Some(Lowered { diagnostics: diagnostics.build(), root, variables, blocks })
    }

    /// Lowers a semantic block.
    fn lower_block(&mut self, expr_block: &semantic::ExprBlock) -> Option<BlockSealed> {
        let mut scope = BlockScope::default();
        for (i, stmt_id) in expr_block.statements.iter().enumerate() {
            let stmt = &self.function_def.statements[*stmt_id];
            let lowered_stmt = self.lower_statement(&mut scope, stmt);

            // If flow is not reachable anymore, no need to continue emitting statements.
            match lowered_stmt {
                Ok(()) => {}
                Err(StatementLoweringFlowError::Failed) => return None,
                Err(StatementLoweringFlowError::End(end)) => {
                    if i + 1 < expr_block.statements.len() {
                        let start_stmt =
                            &self.function_def.statements[expr_block.statements[i + 1]];
                        let end_stmt =
                            &self.function_def.statements[*expr_block.statements.last().unwrap()];
                        // Emit diagnostic fo the rest of the statements with unreachable.
                        self.ctx.diagnostics.report(
                            start_stmt.stable_ptr().untyped(),
                            Unreachable { last_statement_ptr: end_stmt.stable_ptr().untyped() },
                        );
                        return Some(scope.seal(end));
                    }
                }
            };
        }

        // Determine correct block end.
        let end = match expr_block.tail {
            Some(tail_expr) => {
                let lowered_expr = self.lower_expr(&mut scope, tail_expr);
                match lowered_expr {
                    Ok(LoweredExpr::AtVariable(var)) => BlockScopeEnd::Callsite(Some(var)),
                    Ok(LoweredExpr::Unit) => BlockScopeEnd::Callsite(None),
                    Err(LoweringFlowError::Unreachable) => BlockScopeEnd::Unreachable,
                    Err(LoweringFlowError::Failed) => {
                        return None;
                    }
                }
            }
            None => BlockScopeEnd::Callsite(None),
        };
        Some(scope.seal(end))
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
                let lowered_expr = self.lower_expr(scope, *expr)?;
                match lowered_expr {
                    LoweredExpr::AtVariable(var) => {
                        // TODO(spapini): Implicits and mutables.
                        return Err(StatementLoweringFlowError::End(BlockScopeEnd::Return(vec![
                            var,
                        ])));
                    }
                    LoweredExpr::Unit => {
                        // TODO(spapini): Implicits and mutables.
                        return Err(StatementLoweringFlowError::End(BlockScopeEnd::Return(vec![])));
                    }
                }
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
        var: OwnedVariable,
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
        let expr = &self.function_def.exprs[expr_id];
        match expr {
            semantic::Expr::Tuple(expr) => {
                let inputs = self.lower_exprs_as_vars(&expr.items, scope)?;
                Ok(LoweredExpr::AtVariable(
                    generators::TupleConstruct { inputs, ty: expr.ty }.add(&mut self.ctx, scope),
                ))
            }
            semantic::Expr::Assignment(_) => todo!(),
            semantic::Expr::Block(_) => todo!(),
            semantic::Expr::FunctionCall(expr) => {
                let inputs = self.lower_exprs_as_vars(&expr.args, scope)?;
                Ok(LoweredExpr::AtVariable(
                    generators::Call { function: expr.function, inputs, ret_ty: expr.ty }
                        .add(&mut self.ctx, scope),
                ))
            }
            semantic::Expr::Match(_) => todo!(),
            semantic::Expr::If(_) => todo!(),
            // TODO(spapini): Convert to a diagnostic.
            semantic::Expr::Var(expr) => Ok(LoweredExpr::AtVariable(
                scope
                    .get_or_pull_semantic_variable(&mut self.ctx, expr.var)
                    .var()
                    .expect("Value already moved."),
            )),
            semantic::Expr::Literal(expr) => Ok(LoweredExpr::AtVariable(
                generators::Literal { value: expr.value, ty: expr.ty }.add(&mut self.ctx, scope),
            )),
            semantic::Expr::MemberAccess(_) => todo!(),
            semantic::Expr::StructCtor(_) => todo!(),
            semantic::Expr::EnumVariantCtor(_) => todo!(),
            semantic::Expr::Missing(_) => todo!(),
        }
    }

    /// Lowers a sequence of expressions and return them all. If the flow ended in the middle,
    /// propagates that flow error without returning any variable.
    fn lower_exprs_as_vars(
        &mut self,
        exprs: &[semantic::ExprId],
        scope: &mut BlockScope,
    ) -> Result<Vec<OwnedVariable>, LoweringFlowError> {
        exprs
            .iter()
            .map(|arg_expr_id| Ok(self.lower_expr(scope, *arg_expr_id)?.var(self, scope)))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Introduces a unit variable inplace.
    fn unit_var(&mut self, scope: &mut BlockScope) -> OwnedVariable {
        generators::TupleConstruct { inputs: vec![], ty: unit_ty(self.ctx.db) }
            .add(&mut self.ctx, scope)
    }
}

#[allow(dead_code)]
/// Representation of the value of a computed expression.
enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(OwnedVariable),
    /// The expression value is unit.
    Unit,
}
impl LoweredExpr {
    fn var(self, lowerer: &mut Lowerer<'_>, scope: &mut BlockScope) -> OwnedVariable {
        match self {
            LoweredExpr::AtVariable(var_id) => var_id,
            LoweredExpr::Unit => lowerer.unit_var(scope),
        }
    }
}

#[allow(dead_code)]
/// Cases where the flow of lowering an expression should halt.
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
