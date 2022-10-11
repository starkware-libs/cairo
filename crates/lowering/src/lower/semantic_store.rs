use utils::ordered_hash_map::OrderedHashMap;

use super::variable::OwnedVariable;
use crate::lower::Lowerer;

/// The liveness state of a semantic variable.
pub enum SemanticVariableState {
    Alive(OwnedVariable),
    Moved,
}
impl SemanticVariableState {
    /// Gets the variable, moving it if not duplicatable. See [`Self::take_var()`].
    pub fn get_var(&mut self, lowerer: &Lowerer<'_>) -> SemanticVariableState {
        match self {
            SemanticVariableState::Alive(var) => {
                if let Some(var) = var.maybe_duplicate(lowerer) {
                    SemanticVariableState::Alive(var)
                } else {
                    self.take_var()
                }
            }
            SemanticVariableState::Moved => SemanticVariableState::Moved,
        }
    }
    /// Takes the variable and moves it. The current state will become SemanticVariableState::Moved.
    pub fn take_var(&mut self) -> SemanticVariableState {
        std::mem::replace(self, SemanticVariableState::Moved)
    }
    /// Reveals the inner OwnedVariable if not Moved.
    pub fn var(self) -> Option<OwnedVariable> {
        match self {
            SemanticVariableState::Alive(var) => Some(var),
            SemanticVariableState::Moved => None,
        }
    }
}

/// Stores owned lowered variables for semantic variables.
#[derive(Default)]
pub struct SemanticVariablesStore {
    /// Mapping from a semantic variable to its current liveness state.
    semantic_variables: OrderedHashMap<semantic::VarId, SemanticVariableState>,
}
impl SemanticVariablesStore {
    /// Fetches a semantic variable state.
    /// If the semantic variable was never in the store, returns None.
    /// Otherwise, if it was moved, returns SemanticVariableState::Moved.
    /// Otherwise, returns SemanticVariableState::Alive() with its owned lowered variable.
    pub fn get(
        &mut self,
        lowerer: &Lowerer<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<SemanticVariableState> {
        Some(self.semantic_variables.get_mut(&semantic_var_id)?.get_var(lowerer))
    }
    /// Stores a semantic variable together with its owned lowered variable into the store.
    pub fn put(&mut self, semantic_var_id: semantic::VarId, var: OwnedVariable) {
        self.semantic_variables.insert(semantic_var_id, SemanticVariableState::Alive(var));
    }
}
