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
    // The following is not really an optimization but we want to apply optimizations before and
    // after it, so it is convenient to treat it as an optimization.
    LowerImplicits,
}

define_short_id!(
    OptimizationStrategyId,
    OptimizationStrategy,
    LoweringGroup,
    lookup_intern_strategy
);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OptimizationStrategy(pub Vec<OptimizationPhase>);

impl OptimizationStrategyId {
    pub fn default_strategy(db: &dyn LoweringGroup) -> Self {
        db.intern_strategy(OptimizationStrategy(vec![
            OptimizationPhase::ApplyInlining,
            OptimizationPhase::ReturnOptimization,
            OptimizationPhase::OptimizeRemappings,
            // The call to `reorder_statements` before and after `branch_inversion` is intentional.
            // See description of `branch_inversion` for more details.
            OptimizationPhase::ReorderStatements,
            OptimizationPhase::BranchInversion,
            OptimizationPhase::ReorderStatements,
            OptimizationPhase::ConstFolding,
            OptimizationPhase::OptimizeMatches,
            OptimizationPhase::SplitStructs,
            OptimizationPhase::OptimizeRemappings,
            OptimizationPhase::ReorderStatements,
            OptimizationPhase::OptimizeMatches,
            OptimizationPhase::LowerImplicits,
            OptimizationPhase::OptimizeRemappings,
            OptimizationPhase::CancelOps,
            OptimizationPhase::ReorderStatements,
            OptimizationPhase::ReorganizeBlocks,
        ]))
    }

    pub fn apply_strategy(
        self,
        db: &dyn LoweringGroup,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<FlatLowered> {
        let mut lowered = (*db.concrete_function_with_body_postpanic_lowered(function)?).clone();

        for phase in db.lookup_intern_strategy(self).0 {
            match phase {
                OptimizationPhase::ApplyInlining => apply_inlining(db, function, &mut lowered)?,
                OptimizationPhase::ReturnOptimization => return_optimization(db, &mut lowered),
                OptimizationPhase::OptimizeRemappings => optimize_remappings(&mut lowered),
                OptimizationPhase::ReorderStatements => reorder_statements(db, &mut lowered),
                OptimizationPhase::BranchInversion => branch_inversion(db, &mut lowered),
                OptimizationPhase::ConstFolding => const_folding(db, &mut lowered),
                OptimizationPhase::OptimizeMatches => optimize_matches(&mut lowered),
                OptimizationPhase::SplitStructs => split_structs(&mut lowered),
                OptimizationPhase::LowerImplicits => lower_implicits(db, function, &mut lowered),
                OptimizationPhase::CancelOps => cancel_ops(&mut lowered),
                OptimizationPhase::ReorganizeBlocks => reorganize_blocks(&mut lowered),
            }
        }

        Ok(lowered)
    }
}
