use utils::ordered_hash_map::OrderedHashMap;

use super::context::LoweringContext;
use super::variables::LivingVar;

/// The liveness state of a semantic variable.
#[derive(Debug)]
pub enum SemanticVariableEntry {
    Alive(LivingVar),
    Moved,
}
impl SemanticVariableEntry {
    /// Gets the variable, moving it if not duplicatable. See [`Self::take_var()`].
    pub fn get(&mut self, ctx: &LoweringContext<'_>) -> SemanticVariableEntry {
        match self {
            SemanticVariableEntry::Alive(var) => {
                if let Some(var) = var.try_duplicate(ctx) {
                    SemanticVariableEntry::Alive(var)
                } else {
                    self.take()
                }
            }
            SemanticVariableEntry::Moved => SemanticVariableEntry::Moved,
        }
    }

    /// Takes the variable and moves it. The current state will become SemanticVariableState::Moved.
    fn take(&mut self) -> SemanticVariableEntry {
        std::mem::replace(self, SemanticVariableEntry::Moved)
    }

    /// Reveals the inner LivingVar if not Moved.
    pub fn take_var(self) -> Option<LivingVar> {
        match self {
            SemanticVariableEntry::Alive(var) => Some(var),
            SemanticVariableEntry::Moved => None,
        }
    }
}

/// Holds on to the [LivingVar] instance for semantic variables.
#[derive(Default)]
pub struct SemanticVariablesMap {
    /// Mapping from a semantic variable to its current liveness state.
    pub var_mapping: OrderedHashMap<semantic::VarId, SemanticVariableEntry>,
}
impl SemanticVariablesMap {
    /// Fetches a semantic variable.
    /// If the semantic variable was never in the store, returns None.
    /// Otherwise, if it was moved, returns SemanticVariableState::Moved.
    /// Otherwise, gets the var from the map, duplicating if possible.
    pub fn get(
        &mut self,
        ctx: &LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<SemanticVariableEntry> {
        Some(self.var_mapping.get_mut(&semantic_var_id)?.get(ctx))
    }

    /// Returns true if the variable exists in the map.
    pub fn contains(&mut self, semantic_var_id: semantic::VarId) -> bool {
        self.var_mapping.contains_key(&semantic_var_id)
    }

    /// Takes a semantic variable.
    /// If the semantic variable was never in the store, returns None.
    /// Otherwise, if it was moved, returns SemanticVariableState::Moved.
    /// Otherwise, moves the var from the map.
    pub fn take(&mut self, semantic_var_id: semantic::VarId) -> Option<SemanticVariableEntry> {
        Some(self.var_mapping.get_mut(&semantic_var_id)?.take())
    }

    /// Stores a semantic variable together with its owned lowered variable into the store.
    pub fn put(
        &mut self,
        semantic_var_id: semantic::VarId,
        var: LivingVar,
    ) -> &mut SemanticVariableEntry {
        self.var_mapping.insert(semantic_var_id, SemanticVariableEntry::Alive(var));
        self.var_mapping.get_mut(&semantic_var_id).unwrap()
    }
}
