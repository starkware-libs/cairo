use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::define_short_id;

use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::implicits::lower_implicits;
use crate::inline::apply_inlining;
use crate::optimizations::branch_inversion::branch_inversion;
use crate::optimizations::cancel_ops::cancel_ops;
use crate::optimizations::const_folding::const_folding;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::optimizations::reorder_statements::reorder_statements;
use crate::optimizations::return_optimization::return_optimization;
use crate::optimizations::split_structs::split_structs;
use crate::reorganize_blocks::reorganize_blocks;
use crate::FlatLowered;

/// Enum of the optimization phases that can be used in a strategy.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum OptimizationPhase {
    ApplyInlining,
    BranchInversion,
    CancelOps,
    ConstFolding,
    OptimizeMatches,
    OptimizeRemappings,
    ReorderStatements,
    ReorganizeBlocks,
    ReturnOptimization,
    SplitStructs,
    /// The following is not really an optimization but we want to apply optimizations before and
    /// after it, so it is convenient to treat it as an optimization.
    LowerImplicits,
}

impl OptimizationPhase {
    /// Applies the optimization phase to the lowering.
    ///
    /// Assumes `lowered` is a a lowering of `function`.
    pub fn apply(
        self,
        db: &dyn LoweringGroup,
        function: ConcreteFunctionWithBodyId,
        lowered: &mut FlatLowered,
    ) -> Maybe<()> {
        match self {
            OptimizationPhase::ApplyInlining => apply_inlining(db, function, lowered)?,
            OptimizationPhase::BranchInversion => branch_inversion(db, lowered),
            OptimizationPhase::CancelOps => cancel_ops(lowered),
            OptimizationPhase::ConstFolding => const_folding(db, lowered),
            OptimizationPhase::OptimizeMatches => optimize_matches(lowered),
            OptimizationPhase::OptimizeRemappings => optimize_remappings(lowered),
            OptimizationPhase::ReorderStatements => reorder_statements(db, lowered),
            OptimizationPhase::ReorganizeBlocks => reorganize_blocks(lowered),
            OptimizationPhase::ReturnOptimization => return_optimization(db, lowered),
            OptimizationPhase::SplitStructs => split_structs(lowered),
            OptimizationPhase::LowerImplicits => lower_implicits(db, function, lowered),
        }
        Ok(())
    }
}

define_short_id!(
    OptimizationStrategyId,
    OptimizationStrategy,
    LoweringGroup,
    lookup_intern_strategy
);

/// A strategy is a sequence of optimization phases.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OptimizationStrategy(pub Vec<OptimizationPhase>);

impl OptimizationStrategyId {
    /// Applies the optimization strategy phase to the lowering.
    ///
    /// Assumes `lowered` is a a lowering of `function`.
    pub fn apply_strategy(
        self,
        db: &dyn LoweringGroup,
        function: ConcreteFunctionWithBodyId,
        lowered: &mut FlatLowered,
    ) -> Maybe<()> {
        for phase in db.lookup_intern_strategy(self).0 {
            phase.apply(db, function, lowered)?;
        }

        Ok(())
    }
}

/// Query implementation of [crate::db::LoweringGroup::baseline_optimization_strategy].
pub fn baseline_optimization_strategy(db: &dyn LoweringGroup) -> OptimizationStrategyId {
    db.intern_strategy(OptimizationStrategy(vec![
        OptimizationPhase::ApplyInlining,
        OptimizationPhase::ReturnOptimization,
        OptimizationPhase::ReorganizeBlocks,
        // The call to `reorder_statements` before and after `branch_inversion` is intentional.
        // See description of `branch_inversion` for more details.
        OptimizationPhase::ReorderStatements,
        OptimizationPhase::BranchInversion,
        OptimizationPhase::ReorderStatements,
        OptimizationPhase::CancelOps,
        OptimizationPhase::ConstFolding,
        OptimizationPhase::OptimizeMatches,
        OptimizationPhase::SplitStructs,
        OptimizationPhase::ReorganizeBlocks,
        OptimizationPhase::ReorderStatements,
        OptimizationPhase::OptimizeMatches,
        OptimizationPhase::ReorganizeBlocks,
        OptimizationPhase::CancelOps,
        OptimizationPhase::ReorderStatements,
        OptimizationPhase::ReorganizeBlocks,
    ]))
}

/// Query implementation of [crate::db::LoweringGroup::final_optimization_strategy].
pub fn final_optimization_strategy(db: &dyn LoweringGroup) -> OptimizationStrategyId {
    db.intern_strategy(OptimizationStrategy(vec![
        OptimizationPhase::LowerImplicits,
        OptimizationPhase::ReorganizeBlocks,
        OptimizationPhase::CancelOps,
        OptimizationPhase::ReorderStatements,
        OptimizationPhase::ReorganizeBlocks,
    ]))
}
