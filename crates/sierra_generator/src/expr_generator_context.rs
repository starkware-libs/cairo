use std::collections::{hash_map, HashMap};

use defs::ids::LocalVarId;
use semantic::db::SemanticGroup;

use crate::id_allocator::IdAllocator;
use crate::pre_sierra;

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SemanticGroup,
    var_id_allocator: IdAllocator,
    statement_id_allocator: IdAllocator,
    variables: HashMap<LocalVarId, sierra::ids::VarId>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(db: &'a dyn SemanticGroup) -> Self {
        ExprGeneratorContext {
            db,
            var_id_allocator: IdAllocator::default(),
            statement_id_allocator: IdAllocator::default(),
            variables: HashMap::new(),
        }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_sierra_variable(&mut self) -> sierra::ids::VarId {
        sierra::ids::VarId::from_usize(self.var_id_allocator.allocate())
    }

    /// Returns the SemanticGroup salsa database.
    pub fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }

    /// Attaches a local variable with its Sierra variable.
    /// See [Self::get_variable].
    pub fn register_variable(&mut self, local_var_id: LocalVarId, sierra_var: sierra::ids::VarId) {
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
    pub fn get_variable(&self, local_var: LocalVarId) -> sierra::ids::VarId {
        // TODO(lior): Consider throwing an error with a location.
        self.variables.get(&local_var).expect("Internal compiler error.").clone()
    }

    /// Generates a label id and a label statement.
    // TODO(lior): Consider using stabe ids, instead of allocating sequential ids.
    pub fn new_label(&mut self) -> (pre_sierra::Statement, pre_sierra::LabelId) {
        let id = pre_sierra::LabelId::new(self.statement_id_allocator.allocate());
        (pre_sierra::Statement::Label(pre_sierra::Label { id: id.clone() }), id)
    }
}
