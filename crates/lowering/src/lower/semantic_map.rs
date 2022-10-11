use utils::ordered_hash_map::OrderedHashMap;

use super::scope::OwnedVariable;
use crate::lower::Lowerer;

/// The liveness state of a semantic variable.
pub enum SemanticVariableEntry {
    Alive(OwnedVariable),
    Moved,
}
impl SemanticVariableEntry {
    /// Gets the variable, moving it if not duplicatable. See [`Self::take_var()`].
    fn get_var(&mut self, lowerer: &Lowerer<'_>) -> SemanticVariableEntry {
        match self {
            SemanticVariableEntry::Alive(var) => {
                if let Some(var) = var.try_duplicate(lowerer) {
                    SemanticVariableEntry::Alive(var)
                } else {
                    self.take_var()
                }
            }
            SemanticVariableEntry::Moved => SemanticVariableEntry::Moved,
        }
    }

    /// Takes the variable and moves it. The current state will become SemanticVariableState::Moved.
    fn take_var(&mut self) -> SemanticVariableEntry {
        std::mem::replace(self, SemanticVariableEntry::Moved)
    }

    /// Reveals the inner OwnedVariable if not Moved.
    pub fn var(self) -> Option<OwnedVariable> {
        match self {
            SemanticVariableEntry::Alive(var) => Some(var),
            SemanticVariableEntry::Moved => None,
        }
    }
}

/// Holds on to the [OwnedVariable] instance for semantic variables.
#[derive(Default)]
pub struct SemanticVariablesMap {
    /// Mapping from a semantic variable to its current liveness state.
    semantic_variables: OrderedHashMap<semantic::VarId, SemanticVariableEntry>,
}
impl SemanticVariablesMap {
    /// Fetches a semantic variable state.
    /// If the semantic variable was never in the store, returns None.
    /// Otherwise, if it was moved, returns SemanticVariableState::Moved.
    /// Otherwise, returns SemanticVariableState::Alive() with its owned lowered variable.
    pub fn get(
        &mut self,
        lowerer: &Lowerer<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<SemanticVariableEntry> {
        Some(self.semantic_variables.get_mut(&semantic_var_id)?.get_var(lowerer))
    }

    /// Stores a semantic variable together with its owned lowered variable into the store.
    pub fn put(&mut self, semantic_var_id: semantic::VarId, var: OwnedVariable) {
        self.semantic_variables.insert(semantic_var_id, SemanticVariableEntry::Alive(var));
    }
}
