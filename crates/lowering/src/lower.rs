use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use itertools::zip_eq;
use scope::{BlockScope, BlockScopeEnd, OwnedVariable};
use semantic::db::SemanticGroup;
use semantic::TypeLongId;
use utils::extract_matches;
use utils::unordered_hash_map::UnorderedHashMap;

use self::scope::BlockSealed;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{
    Block, BlockId, Statement, StatementCall, StatementLiteral, StatementTupleConstruct,
    StatementTupleDestruct, Variable, VariableId,
};

mod scope;
mod semantic_map;

/// Context for lowering a function.
pub struct Lowerer<'db> {
    pub db: &'db dyn SemanticGroup,
    /// Semantic model for current function definition.
    function_def: &'db semantic::FreeFunctionDefinition,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
    /// Definitions encountered for semantic variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
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
    // TODO(spapini): Consider forbidding direct access in lowering.
    // TODO(spapini): Consider splitting this struct to Arenas/Allocator, and rest of the logic.
    /// Allocates a new variable with a specific semantic type, in the arena.
    pub fn new_variable(&mut self, ty: semantic::TypeId) -> VariableId {
        // TODO(spapini): Get the correct values here for the type.
        self.variables.alloc(Variable { duplicatable: true, droppable: true, ty })
    }

    /// Lowers a semantic free function.
    pub fn lower(db: &dyn SemanticGroup, free_function_id: FreeFunctionId) -> Option<Lowered> {
        let function_def = db.free_function_definition(free_function_id)?;
        let mut lowerer = Lowerer {
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
        let input_semantic_var_ids: Vec<_> =
            input_semantic_vars.iter().map(|semantic_var| semantic_var.id()).collect();
        // TODO(spapini): Build semantic_defs in semantic model.
        for semantic_var in input_semantic_vars {
            lowerer.semantic_defs.insert(semantic_var.id(), semantic_var);
        }

        let root_sealed_block = lowerer.lower_block(function_def.body);
        let root_block = root_sealed_block.finalize(&mut lowerer, &input_semantic_var_ids, &[]);
        let root = lowerer.blocks.alloc(root_block);
        let Lowerer { diagnostics, variables, blocks, .. } = lowerer;
        Some(Lowered { diagnostics: diagnostics.build(), root, variables, blocks })
    }

    /// Lowers a semantic block.
    fn lower_block(&mut self, block_expr_id: semantic::ExprId) -> BlockSealed {
        let expr = &self.function_def.exprs[block_expr_id];
        let expr_block = extract_matches!(expr, semantic::Expr::Block);
        let mut scope = BlockScope::default();
        for (i, stmt_id) in expr_block.statements.iter().enumerate() {
            let stmt = &self.function_def.statements[*stmt_id];
            if let semantic::Statement::Return(expr_id) = stmt {
                if i + 1 < expr_block.statements.len() {
                    let start_stmt = &self.function_def.statements[expr_block.statements[i + 1]];
                    let end_stmt =
                        &self.function_def.statements[*expr_block.statements.last().unwrap()];
                    // Emit diagnostic fo the rest of the statements with unreachable.
                    self.diagnostics.report(
                        start_stmt.stable_ptr().untyped(),
                        Unreachable { last_statement_ptr: end_stmt.stable_ptr().untyped() },
                    );
                }

                // TODO(spapini): Handle borrowing, muts, implicits ...
                let returns = self.lower_expr(&mut scope, expr_id.expr);
                return scope.seal(BlockScopeEnd::Return(vec![returns]));
            }

            self.lower_statement(&mut scope, stmt)
        }

        let maybe_output = expr_block.tail.map(|expr_id| self.lower_expr(&mut scope, expr_id));
        scope.seal(BlockScopeEnd::Callsite(maybe_output))
    }

    /// Lowers a semantic statement.
    pub fn lower_statement(&mut self, scope: &mut BlockScope, stmt: &semantic::Statement) {
        match stmt {
            semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
                self.lower_expr(scope, *expr);
            }
            semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
                let var_id = self.lower_expr(scope, *expr);
                self.lower_single_pattern(scope, pattern, var_id);
            }
            semantic::Statement::Return(_) => unreachable!(),
        }
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
                self.semantic_defs.insert(sem_var.id(), sem_var);
            }
            semantic::Pattern::Struct(_) => todo!(),
            semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty }) => {
                // TODO(spapini): This logic currently gets rid of OwnedVariable, and needs to
                //   ensure that the crated Statement only uses live variables. This could lead
                //   to panics in add_statement. After the refactor mentioned at
                //   [`BlockScope::add_statement()`], this should be better,
                let tys = extract_matches!(self.db.lookup_intern_type(*ty), TypeLongId::Tuple);
                assert_eq!(
                    tys.len(),
                    field_patterns.len(),
                    "Expected the same number of tuple args."
                );
                let outputs: Vec<_> = tys.iter().map(|ty| self.new_variable(*ty)).collect();
                let stmt = Statement::TupleDestruct(StatementTupleDestruct {
                    input: var.var_id(),
                    outputs,
                });
                let owned_outputs = scope.add_statement(self, stmt);
                for (var, pattern) in zip_eq(owned_outputs, field_patterns) {
                    self.lower_single_pattern(scope, pattern, var);
                }
            }
            semantic::Pattern::Enum(_) => unreachable!(),
            semantic::Pattern::Otherwise(_) => {}
        }
    }

    /// Lowers a semantic expression.
    fn lower_expr(&mut self, scope: &mut BlockScope, expr_id: semantic::ExprId) -> OwnedVariable {
        let expr = &self.function_def.exprs[expr_id];
        match expr {
            semantic::Expr::Tuple(expr) => {
                let inputs = expr
                    .items
                    .iter()
                    .map(|arg_expr_id| self.lower_expr(scope, *arg_expr_id).var_id())
                    .collect();

                let result_var = self.new_variable(expr.ty);
                let owned_outputs = scope.add_statement(
                    self,
                    Statement::TupleConstruct(StatementTupleConstruct {
                        inputs,
                        output: result_var,
                    }),
                );
                owned_outputs.into_iter().next().unwrap()
            }
            semantic::Expr::Assignment(_) => todo!(),
            semantic::Expr::Block(_) => todo!(),
            semantic::Expr::FunctionCall(expr) => {
                // TODO(spapini): This logic currently gets rid of OwnedVariable, and needs to
                //   ensure that the crated Statement only uses live variables. This could lead
                //   to panics in add_statement. After the refactor mentioned at
                //   [`BlockScope::add_statement()`], this should be better,
                let inputs = expr
                    .args
                    .iter()
                    .map(|arg_expr_id| self.lower_expr(scope, *arg_expr_id).var_id())
                    .collect();

                // Allocate a new variable for the result of the function.
                let result_var = self.new_variable(expr.ty);
                let outputs = vec![result_var];
                let owned_outputs = scope.add_statement(
                    self,
                    Statement::Call(StatementCall { function: expr.function, inputs, outputs }),
                );
                owned_outputs.into_iter().next().unwrap()
            }
            semantic::Expr::Match(_) => todo!(),
            semantic::Expr::If(_) => todo!(),
            // TODO(spapini): Convert to a diagnostic.
            semantic::Expr::Var(expr) => scope
                .get_or_pull_semantic_variable(self, expr.var)
                .var()
                .expect("Value already moved."),
            semantic::Expr::Literal(expr) => {
                // TODO(spapini): This logic currently gets rid of OwnedVariable, and needs to
                //   ensure that the crated Statement only uses live variables. This could lead
                //   to panics in add_statement. After the refactor mentioned at
                //   [`BlockScope::add_statement()`], this should be better,
                let output = self.new_variable(expr.ty);
                let owned_outputs = scope.add_statement(
                    self,
                    Statement::Literal(StatementLiteral { value: expr.value, output }),
                );
                owned_outputs.into_iter().next().unwrap()
            }
            semantic::Expr::MemberAccess(_) => todo!(),
            semantic::Expr::StructCtor(_) => todo!(),
            semantic::Expr::EnumVariantCtor(_) => todo!(),
            semantic::Expr::Missing(_) => todo!(),
        }
    }
}
