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
/// Context for lowering a function.
pub struct Lowerer<'db> {
    db: &'db dyn SemanticGroup,
    /// Semantic model for current function definition.
    function_def: &'db FreeFunctionDefinition,
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
        // TODO(spapini): Handle regular exit from a block.
        todo!("Handle regular exit from a block.");
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
        let BlockScope { inputs, living_variables: _, semantic_variables: _, statements } = scope;
        // TODO(spapini): Find mut function parameters, and return them as well.
        self.blocks.alloc(Block {
            inputs,
            statements,
            drops: vec![],
            end: BlockEnd::Return(vec![var_id]),
        })
    }

    /// Lowers a semantic statement.
    pub fn lower_statement(&self, _scope: &mut BlockScope, _stmt: &semantic::Statement) {
        todo!()
    }

    /// Lowers a semantic expression.
    fn lower_expr(&mut self, _scope: &mut BlockScope, expr_id: semantic::ExprId) -> VariableId {
        // Dummy expression lowering.
        // TODO(spapini): Replace this with real logic.
        self.variables.alloc(Variable {
            duplicatable: true,
            droppable: true,
            ty: self.function_def.exprs[expr_id].ty(),
        })
    }

    /// Takes a variable from the current block scope (i.e. moving/consuming it).
    fn take(&self, scope: &mut BlockScope, var_id: VariableId) {
        let var = self.variables.get(var_id).unwrap();
        if var.duplicatable {
            return;
        }
        scope.living_variables.swap_remove(&var_id);
    }
}
