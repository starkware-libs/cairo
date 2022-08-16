use std::collections::{hash_map, HashMap};
use std::fmt::Display;

use defs::ids::LocalVarId;
use semantic::db::SemanticGroup;

use crate::id_allocator::IdAllocator;

/// Represents a variable in the compiled Sierra.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraVariable(u64);
impl From<u64> for SierraVariable {
    fn from(id: u64) -> Self {
        SierraVariable(id)
    }
}
impl Display for SierraVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.0)
    }
}

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SemanticGroup,
    id_allocator: IdAllocator,
    variables: HashMap<LocalVarId, SierraVariable>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(db: &'a dyn SemanticGroup) -> Self {
        ExprGeneratorContext { db, id_allocator: IdAllocator::default(), variables: HashMap::new() }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_variable(&mut self) -> SierraVariable {
        SierraVariable::from(self.id_allocator.allocate())
    }

    /// Returns the SemanticGroup salsa database.
    pub fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }

    /// Attaches a local variable with its Sierra variable.
    /// See [Self::get_variable].
    pub fn register_variable(&mut self, local_var_id: LocalVarId, sierra_var: SierraVariable) {
        match self.variables.entry(local_var_id) {
            hash_map::Entry::Occupied(_) => {
                // TODO(lior): Either panic!() if this is necessarily an internal compiler error
                //   or use diagnostics or Result.
                unimplemented!()
            }
            hash_map::Entry::Vacant(entry) => entry.insert(sierra_var),
        };
    }

    /// Returns the Sierra variable associated with the given local variable.
    /// See [Self::register_variable].
    #[allow(dead_code)]
    pub fn get_variable(&self, local_var: LocalVarId) -> SierraVariable {
        // TODO(lior): Consider throwing an error with a location.
        self.variables.get(&local_var).expect("Internal compiler error.").clone()
    }
}
