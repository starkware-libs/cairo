use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use semantic::db::SemanticGroup;
use semantic::FreeFunctionDefinition;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{Block, BlockEnd, BlockId, Statement, Variable, VariableId};

// TODO(spapini): Remove.
#[allow(dead_code)]
pub struct Lowerer<'db> {
    db: &'db dyn SemanticGroup,
    function_def: &'db FreeFunctionDefinition,
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
        for stmt_id in &expr_block.statements {
            let stmt = &self.function_def.statements[*stmt_id];
            if let semantic::Statement::Return(expr_id) = stmt {
                // Emit diagnostic fo the rest of the statements with unreachable.
                // TODO(spapini): emit unreachable to the correct place.
                self.diagnostics
                    .report(self.function_def.exprs[*expr_id].stable_ptr().untyped(), Unreachable);

                return self.finalize_block_return(scope, *expr_id);
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

    pub(crate) fn lower_statement(&self, _scope: &mut BlockScope, _stmt: &semantic::Statement) {
        todo!()
    }

    fn lower_expr(&mut self, _scope: &mut BlockScope, expr_id: semantic::ExprId) -> VariableId {
        // TODO(spapini): Replace this with real logic.
        self.variables.alloc(Variable {
            duplicatable: true,
            droppable: true,
            ty: self.function_def.exprs[expr_id].ty(),
        })
    }

    fn take(&self, scope: &mut BlockScope, var_id: VariableId) {
        let var = self.variables.get(var_id).unwrap();
        if var.duplicatable {
            return;
        }
        scope.living_variables.swap_remove(&var_id);
    }
}
