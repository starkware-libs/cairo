use id_arena::Arena;
use semantic::db::SemanticGroup;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::diagnostic::LoweringDiagnostics;
use crate::objects::{Block, Variable};
use crate::VariableId;

/// Context for the lowering phase.
pub struct LoweringContext<'db> {
    pub db: &'db dyn SemanticGroup,
    /// Current emitted diagnostics.
    pub diagnostics: LoweringDiagnostics,
    /// Arena of allocated lowered variables.
    pub variables: Arena<Variable>,
    /// Arena of allocated lowered blocks.
    pub blocks: Arena<Block>,
    /// Definitions encountered for semantic variables.
    // TODO(spapini): consider moving to semantic model.
    pub semantic_defs: UnorderedHashMap<semantic::VarId, semantic::Variable>,
}
impl<'db> LoweringContext<'db> {
    // TODO(spapini): Consider forbidding direct access in lowering.
    /// Allocates a new variable with a specific semantic type, in the arena.
    pub fn new_variable(&mut self, ty: semantic::TypeId) -> VariableId {
        // TODO(spapini): Get the correct values here for the type.
        self.variables.alloc(Variable { duplicatable: true, droppable: true, ty })
    }
}
