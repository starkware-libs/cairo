use id_arena::Arena;
use semantic::db::SemanticGroup;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::diagnostic::LoweringDiagnostics;
use crate::objects::{Block, Variable};

/// Context for the lowering phase.
pub struct LoweringContext<'db> {
    pub db: &'db dyn SemanticGroup,
    /// Semantic model for current function definition.
    pub function_def: &'db semantic::FreeFunctionDefinition,
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
