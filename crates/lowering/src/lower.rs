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
    Block, BlockEnd, BlockId, Statement, StatementTupleDestruct, Variable, VariableId,
};

// TODO(spapini): Remove.
#[allow(dead_code)]
pub struct Lowerer<'db> {
    db: &'db dyn SemanticGroup,
    function_def: &'db semantic::FreeFunctionDefinition,
    diagnostics: LoweringDiagnostics,
    variables: Arena<Variable>,
    blocks: Arena<Block>,
}
pub struct Lowered {
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    pub root: BlockId,
    pub variables: Arena<Variable>,
    pub blocks: Arena<Block>,
}

pub struct BlockScope {
    /// Living variables owned by this scope.
    pub inputs: Vec<VariableId>,
    pub living_variables: OrderedHashSet<VariableId>,
    pub semantic_variables: OrderedHashMap<semantic::VarId, VariableId>,
    pub statements: Vec<Statement>,
}

impl<'db> Lowerer<'db> {
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

    fn lower_block(
        &mut self,
        block_expr_id: semantic::ExprId,
        inputs: OrderedHashMap<semantic::VarId, VariableId>,
    ) -> BlockId {
        let expr = &self.function_def.exprs[block_expr_id];
        let expr_block = if let semantic::Expr::ExprBlock(expr_block) = expr {
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
        // TODO(spapini): Handle regular exit from a block.
        todo!("Handle regular exit from a block.");
    }

    fn finalize_block_return(
        &mut self,
        mut scope: BlockScope,
        expr_id: semantic::ExprId,
    ) -> BlockId {
        // First, prepare the expr.
        let var_id = self.lower_expr(&mut scope, expr_id);
        self.take(&mut scope, var_id);
        let BlockScope { inputs, living_variables: _, semantic_variables: _, statements } = scope;
        // TODO(spapini): Find mut function parameters, and return them as well.
        self.blocks.alloc(Block {
            inputs,
            statements,
            drops: vec![],
            end: BlockEnd::Return(vec![var_id]),
        })
    }

    pub fn lower_statement(&mut self, scope: &mut BlockScope, stmt: &semantic::Statement) {
        match stmt {
            semantic::Statement::Expr(semantic::StatementExpr { expr, stable_ptr: _ }) => {
                self.lower_expr(scope, *expr);
            }
            semantic::Statement::Let(semantic::StatementLet { pattern, expr, stable_ptr: _ }) => {
                let var_id = self.lower_expr(scope, *expr);
                self.bind_single_pattern(scope, pattern, var_id);
            }
            semantic::Statement::Return(_) => unreachable!(),
        }
    }

    fn bind_single_pattern(
        &mut self,
        scope: &mut BlockScope,
        pattern: &semantic::Pattern,
        var_id: id_arena::Id<Variable>,
    ) {
        // TODO:(spapini): Separate match pattern from non-match (single) patterns in the semantic
        // model.
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
                    self.bind_single_pattern(scope, pattern, var_id);
                }
            }
            semantic::Pattern::Enum(_) => unreachable!(),
            semantic::Pattern::Otherwise(_) => {}
        }
    }

    fn lower_expr(&mut self, _scope: &mut BlockScope, expr_id: semantic::ExprId) -> VariableId {
        // TODO(spapini): Replace this with real logic.
        self.variables.alloc(Variable {
            duplicatable: true,
            droppable: true,
            ty: self.function_def.exprs[expr_id].ty(),
        })
    }

    fn take(&self, scope: &mut BlockScope, var_id: VariableId) -> VariableId {
        let var = self.variables.get(var_id).unwrap();
        if var.duplicatable {
            return var_id;
        }
        scope.living_variables.swap_remove(&var_id);
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
