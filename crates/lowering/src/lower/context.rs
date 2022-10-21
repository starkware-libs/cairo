use id_arena::Arena;
use semantic::db::SemanticGroup;
use utils::unordered_hash_map::UnorderedHashMap;

use super::scope::{generators, BlockScope, BlockScopeEnd};
use super::variables::LivingVar;
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
    // TODO(spapini): Document.
    pub ref_params: &'db [semantic::VarId],
}

/// Representation of the value of a computed expression.
#[derive(Debug)]
pub enum LoweredExpr {
    /// The expression value lies in a variable.
    AtVariable(LivingVar),
    /// The expression value is a tuple.
    Tuple(Vec<LoweredExpr>),
}
impl LoweredExpr {
    pub fn var(self, ctx: &mut LoweringContext<'_>, scope: &mut BlockScope) -> LivingVar {
        match self {
            LoweredExpr::AtVariable(var_id) => var_id,
            LoweredExpr::Tuple(exprs) => {
                let inputs: Vec<_> = exprs.into_iter().map(|expr| expr.var(ctx, scope)).collect();
                let tys = inputs.iter().map(|var| ctx.variables[var.var_id()].ty).collect();
                let ty = ctx.db.intern_type(semantic::TypeLongId::Tuple(tys));
                generators::TupleConstruct { inputs, ty }.add(ctx, scope)
            }
        }
    }
}

/// Cases where the flow of lowering an expression should halt.
#[derive(Debug)]
pub enum LoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed,
    /// The current computation is unreachable.
    Unreachable,
}
/// Cases where the flow of lowering a statement should halt.
pub enum StatementLoweringFlowError {
    /// Computation failure. A corresponding diagnostic should be emitted.
    Failed,
    /// The block should end after this statement.
    End(BlockScopeEnd),
}
impl From<LoweringFlowError> for StatementLoweringFlowError {
    fn from(err: LoweringFlowError) -> Self {
        match err {
            LoweringFlowError::Failed => StatementLoweringFlowError::Failed,
            LoweringFlowError::Unreachable => {
                StatementLoweringFlowError::End(BlockScopeEnd::Unreachable)
            }
        }
    }
}
