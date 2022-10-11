use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use super::semantic_map::SemanticVariablesMap;
use crate::lower::Lowerer;
use crate::{Block, BlockEnd, Statement, VariableId};

/// Wrapper around VariableId, guaranteeing that the variable is alive.
/// Thus, it does not implement copy nor clone.
pub struct OwnedVariable(VariableId);
impl OwnedVariable {
    // TODO(spapini): Remove once statements are refactored.
    pub fn var_id(&self) -> VariableId {
        self.0
    }

    /// Duplicates the variable if it is duplicatable.
    pub fn try_duplicate(&self, lowerer: &Lowerer<'_>) -> Option<Self> {
        if lowerer.variables[self.0].duplicatable { Some(OwnedVariable(self.0)) } else { None }
    }
}

/// Scope of a block, describing its current state.
/// Maintains the liveness state of lowered variables.
/// Also maintains bound semantic variables. See [SemanticVariablesMap].
#[derive(Default)]
pub struct BlockScope {
    /// Semantic variables to be used as inputs to the block.
    input_semantic_vars: OrderedHashMap<semantic::VarId, VariableId>,
    /// Living variables owned by this scope.
    living_variables: OrderedHashSet<VariableId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    pub semantic_variables: SemanticVariablesMap,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
}

/// Represents how a block ends.
pub enum BlockScopeEnd {
    Callsite(Option<OwnedVariable>),
    Return(Vec<OwnedVariable>),
    Unreachable,
}

impl BlockScope {
    /// Adds a lowered statement to the block. Correctly handle updating liveness of variables.
    // TODO(spapini): Don't allow adding lowering::Statement directly. Instead have specific adding
    // functions to each statement kind, to enforce passing OwnedVariable instances.
    pub fn add_statement(&mut self, lowerer: &Lowerer<'_>, stmt: Statement) -> Vec<OwnedVariable> {
        for input_var in stmt.inputs() {
            // The variables should all come from OwnedVariable instances. When the TODO above
            // is fixed, this will be fixed too.
            self.get_var(lowerer, OwnedVariable(input_var));
        }
        let owned_outputs =
            stmt.outputs().iter().map(|var_id| self.introduce_variable(*var_id)).collect();
        self.statements.push(stmt);
        owned_outputs
    }

    /// Puts a semantic variable to the semantic store, binds it to a new lowered variable, and
    /// marks it as input to the block.
    pub fn add_input_semantic_variable(
        &mut self,
        lowerer: &mut Lowerer<'_>,
        semantic_var_id: semantic::VarId,
    ) {
        let ty = lowerer.semantic_defs[semantic_var_id].ty();
        let var_id = lowerer.new_variable(ty);
        let var = self.introduce_variable(var_id);
        self.semantic_variables.put(semantic_var_id, var);

        assert!(
            self.input_semantic_vars.insert(semantic_var_id, var_id).is_none(),
            "Semantic variable introduced more than once as input to the block"
        );
    }

    /// Finalizes a BlockScope returning a lowered [Block].
    pub fn finalize(mut self, lowerer: &Lowerer<'_>, end: BlockScopeEnd) -> Block {
        let block_end = match end {
            BlockScopeEnd::Callsite(maybe_output) => BlockEnd::Callsite(
                maybe_output.into_iter().map(|var| self.get_var(lowerer, var)).collect(),
            ),
            BlockScopeEnd::Return(returns) => BlockEnd::Return(
                returns.into_iter().map(|var| self.get_var(lowerer, var)).collect(),
            ),
            BlockScopeEnd::Unreachable => BlockEnd::Unreachable,
        };
        Block {
            inputs: self.input_semantic_vars.values().copied().collect(),
            statements: self.statements,
            drops: self.living_variables.into_iter().collect(),
            end: block_end,
        }
    }

    /// Internal. Gets a variable, removing from `living_variables` if not duplicatable.
    fn get_var(&mut self, lowerer: &Lowerer<'_>, var: OwnedVariable) -> VariableId {
        let var_id = var.0;
        if !lowerer.variables[var_id].duplicatable {
            assert!(self.living_variables.swap_remove(&var_id), "Unexpected dead variable.");
        }
        var_id
    }

    /// Internal. Introduces a new variable into `living_variables`.
    fn introduce_variable(&mut self, var_id: VariableId) -> OwnedVariable {
        assert!(self.living_variables.insert(var_id), "Unexpected reintroduced variable.");
        OwnedVariable(var_id)
    }
}
