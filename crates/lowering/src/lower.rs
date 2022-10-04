use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use itertools::zip_eq;
use semantic::db::SemanticGroup;
use semantic::TypeLongId;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{
    Block, BlockEnd, BlockId, Statement, StatementCall, StatementLiteral, StatementTupleDestruct,
    Variable, VariableId,
};

// TODO(spapini): Remove.
#[allow(dead_code)]
/// Context for lowering a function.
pub struct Lowerer<'db> {
    db: &'db dyn SemanticGroup,
    /// Semantic model for current function definition.
    function_def: &'db semantic::FreeFunctionDefinition,
    /// Current emitted diagnostics.
    diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    blocks: Arena<Block>,
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

/// Scope of a block, describing its current state.
pub struct BlockScope {
    /// Variables given as input to the current block.
    pub inputs: Vec<VariableId>,
    /// Living variables owned by this scope.
    pub living_variables: OrderedHashSet<VariableId>,
    /// Mapping from a semantic variable to its lowered variable. Note that there might be
    /// lowered variables that did not originate from a semantic variable.
    pub semantic_variables: OrderedHashMap<semantic::VarId, VariableId>,
    /// Current sequence of lowered statements emitted.
    pub statements: Vec<Statement>,
}

impl<'db> Lowerer<'db> {
    /// Lowers a semantic free function.
    pub fn lower(db: &dyn SemanticGroup, free_function_id: FreeFunctionId) -> Option<Lowered> {
        let function_def = db.free_function_definition(free_function_id)?;
        let mut lowerer = Lowerer {
            db,
            function_def: &*function_def,
            diagnostics: LoweringDiagnostics::new(free_function_id.module(db.upcast())),
            variables: Arena::default(),
            blocks: Arena::default(),
        };

        let signature = db.free_function_declaration_signature(free_function_id)?;

        // Prepare params.
        let inputs: OrderedHashMap<_, _> = signature
            .params
            .iter()
            .map(|param| {
                (
                    semantic::VarId::Param(param.id),
                    // TODO(spapini): Obtain the correct duplicatable, droppable from the type.
                    lowerer.variables.alloc(Variable {
                        duplicatable: true,
                        droppable: true,
                        ty: param.ty,
                    }),
                )
            })
            .collect();
        let root = lowerer.lower_block(function_def.body, inputs);
        let Lowerer { diagnostics, variables, blocks, .. } = lowerer;
        Some(Lowered { diagnostics: diagnostics.build(), root, variables, blocks })
    }

    // Lowers a semantic block.
    fn lower_block(
        &mut self,
        block_expr_id: semantic::ExprId,
        inputs: OrderedHashMap<semantic::VarId, VariableId>,
    ) -> BlockId {
        let expr = &self.function_def.exprs[block_expr_id];
        let expr_block = if let semantic::Expr::Block(expr_block) = expr {
            expr_block
        } else {
            panic!("Expected a block")
        };
        let mut scope = BlockScope {
            inputs: inputs.values().copied().collect(),
            living_variables: inputs.values().copied().collect(),
            semantic_variables: inputs,
            statements: vec![],
        };
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

                return self.finalize_block_return(scope, expr_id.expr);
            }

            self.lower_statement(&mut scope, stmt)
        }

        self.finalize_block_callsite(scope, expr_block.tail)
    }

    /// Finalizes lowering of a block that ends with a `return` statement.
    fn finalize_block_return(
        &mut self,
        mut scope: BlockScope,
        expr_id: semantic::ExprId,
    ) -> BlockId {
        // First, prepare the expr.
        let var_id = self.lower_expr(&mut scope, expr_id);
        self.take(&mut scope, var_id);
        let BlockScope { inputs, living_variables, semantic_variables: _, statements } = scope;
        // TODO(spapini): Find mut function parameters, and return them as well.
        self.blocks.alloc(Block {
            inputs,
            statements,
            drops: living_variables.into_iter().collect(),
            end: BlockEnd::Return(vec![var_id]),
        })
    }

    /// Finalizes lowering of a block that ends regularly, returning to callsite.
    fn finalize_block_callsite(
        &mut self,
        mut scope: BlockScope,
        expr_id: Option<semantic::ExprId>,
    ) -> BlockId {
        // First, prepare the expr.
        let extra_vars = expr_id.map(|expr_id| {
            let var_id = self.lower_expr(&mut scope, expr_id);
            self.take(&mut scope, var_id)
        });
        let BlockScope { inputs, living_variables, semantic_variables: _, statements } = scope;
        // TODO(spapini): Find mut function parameters, and return them as well.
        self.blocks.alloc(Block {
            inputs,
            statements,
            drops: living_variables.into_iter().collect(),
            end: BlockEnd::Callsite(extra_vars.into_iter().collect()),
        })
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
        var_id: id_arena::Id<Variable>,
    ) {
        match pattern {
            semantic::Pattern::Literal(_) => unreachable!(),
            semantic::Pattern::Variable(semantic::PatternVariable { name: _, var: sem_var }) => {
                assert_eq!(self.variables[var_id].ty, sem_var.ty, "Wrong type.");
                scope.semantic_variables.insert(semantic::VarId::Local(sem_var.id), var_id);
            }
            semantic::Pattern::Struct(_) => todo!(),
            semantic::Pattern::Tuple(semantic::PatternTuple { field_patterns, ty }) => {
                let tys = if let TypeLongId::Tuple(tys) = self.db.lookup_intern_type(*ty) {
                    tys
                } else {
                    panic!("Expected a tuple type.")
                };
                assert_eq!(
                    tys.len(),
                    field_patterns.len(),
                    "Expected the same number of tuple args."
                );
                let outputs: Vec<_> = tys.iter().map(|ty| self.new_variable(scope, *ty)).collect();
                let stmt = Statement::TupleDestruct(StatementTupleDestruct {
                    tys,
                    input: self.take(scope, var_id),
                    outputs: outputs.clone(),
                });
                scope.statements.push(stmt);
                for (var_id, pattern) in zip_eq(outputs, field_patterns) {
                    self.lower_single_pattern(scope, pattern, var_id);
                }
            }
            semantic::Pattern::Enum(_) => unreachable!(),
            semantic::Pattern::Otherwise(_) => {}
        }
    }

    /// Lowers a semantic expression.
    fn lower_expr(&mut self, scope: &mut BlockScope, expr_id: semantic::ExprId) -> VariableId {
        let expr = &self.function_def.exprs[expr_id];
        match expr {
            semantic::Expr::Tuple(_) => self.new_variable(scope, expr.ty()),
            semantic::Expr::Assignment(_) => todo!(),
            semantic::Expr::Block(_) => todo!(),
            semantic::Expr::FunctionCall(expr) => {
                let inputs = expr
                    .args
                    .iter()
                    .map(|arg_expr_id| self.lower_expr(scope, *arg_expr_id))
                    .collect();

                // Allocate a new variable for the result of the function.
                let result_var = self.new_variable(scope, expr.ty);
                let outputs = vec![result_var];
                scope.statements.push(Statement::Call(StatementCall {
                    function: expr.function,
                    inputs,
                    outputs,
                }));
                result_var
            }
            semantic::Expr::Match(_) => todo!(),
            semantic::Expr::Var(expr) => self.take(scope, scope.semantic_variables[expr.var]),
            semantic::Expr::Literal(expr) => {
                let output = self.new_variable(scope, expr.ty);
                scope
                    .statements
                    .push(Statement::Literal(StatementLiteral { value: expr.value, output }));
                output
            }
            semantic::Expr::MemberAccess(_) => todo!(),
            semantic::Expr::StructCtor(_) => todo!(),
            semantic::Expr::EnumVariantCtor(_) => todo!(),
            semantic::Expr::Missing(_) => todo!(),
        }
    }

    /// Takes a variable from the current block scope (i.e. moving/consuming it).
    fn take(&self, scope: &mut BlockScope, var_id: VariableId) -> VariableId {
        let var = self.variables.get(var_id).unwrap();
        if !var.duplicatable {
            scope.living_variables.swap_remove(&var_id);
        }
        var_id
    }

    pub fn new_variable(
        &mut self,
        scope: &mut BlockScope,
        ty: semantic::TypeId,
    ) -> id_arena::Id<Variable> {
        let var_id = self.variables.alloc(Variable { duplicatable: true, droppable: true, ty });
        scope.living_variables.insert(var_id);
        var_id
    }
}
