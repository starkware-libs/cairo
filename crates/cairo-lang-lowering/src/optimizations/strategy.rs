use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::{Intern, define_short_id};
use salsa::Database;

use super::cse::cse;
use super::dedup_blocks::dedup_blocks;
use super::early_unsafe_panic::early_unsafe_panic;
use super::gas_redeposit::gas_redeposit;
use super::trim_unreachable::trim_unreachable;
use super::validate::validate;
use crate::Lowered;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::implicits::lower_implicits;
use crate::inline::apply_inlining;
use crate::optimizations::branch_inversion::branch_inversion;
use crate::optimizations::cancel_ops::cancel_ops;
use crate::optimizations::config::Optimizations;
use crate::optimizations::const_folding::const_folding;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::optimizations::reorder_statements::reorder_statements;
use crate::optimizations::return_optimization::return_optimization;
use crate::optimizations::split_structs::split_structs;
use crate::reorganize_blocks::reorganize_blocks;

/// Enum of the optimization phases that can be used in a strategy.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum OptimizationPhase<'db> {
    ApplyInlining {
        enable_const_folding: bool,
    },
    BranchInversion,
    CancelOps,
    ConstFolding,
    Cse,
    DedupBlocks,
    EarlyUnsafePanic,
    OptimizeMatches,
    OptimizeRemappings,
    ReorderStatements,
    ReorganizeBlocks,
    ReturnOptimization,
    SplitStructs,
    TrimUnreachable,
    GasRedeposit,
    /// The following is not really an optimization but we want to apply optimizations before and
    /// after it, so it is convenient to treat it as an optimization.
    LowerImplicits,
    /// A validation phase that checks the lowering is valid. Used for debugging purposes.
    Validate,
    /// A phase that iteratively a set of optimizations to the lowering.
    /// Stops after a certain number of iterations, or when no more changes are made.
    SubStrategy {
        /// The id of the optimization strategy to apply.
        strategy: OptimizationStrategyId<'db>,
        /// The number of times to apply the strategy.
        iterations: usize,
    },
}

impl<'db> OptimizationPhase<'db> {
    /// Applies the optimization phase to the lowering.
    ///
    /// Assumes `lowered` is a lowering of `function`.
    pub fn apply(
        self,
        db: &'db dyn Database,
        function: ConcreteFunctionWithBodyId<'db>,
        lowered: &mut Lowered<'db>,
    ) -> Maybe<()> {
        match self {
            OptimizationPhase::ApplyInlining { enable_const_folding } => {
                apply_inlining(db, function, lowered, enable_const_folding)?
            }
            OptimizationPhase::BranchInversion => branch_inversion(db, lowered),
            OptimizationPhase::CancelOps => cancel_ops(lowered),
            OptimizationPhase::ConstFolding => const_folding(db, function, lowered),
            OptimizationPhase::Cse => cse(lowered),
            OptimizationPhase::EarlyUnsafePanic => early_unsafe_panic(db, lowered),
            OptimizationPhase::DedupBlocks => dedup_blocks(lowered),
            OptimizationPhase::OptimizeMatches => optimize_matches(lowered),
            OptimizationPhase::OptimizeRemappings => optimize_remappings(lowered),
            OptimizationPhase::ReorderStatements => reorder_statements(db, lowered),
            OptimizationPhase::ReorganizeBlocks => reorganize_blocks(lowered),
            OptimizationPhase::ReturnOptimization => return_optimization(db, lowered),
            OptimizationPhase::SplitStructs => split_structs(lowered),
            OptimizationPhase::TrimUnreachable => trim_unreachable(db, lowered),
            OptimizationPhase::LowerImplicits => lower_implicits(db, function, lowered),
            OptimizationPhase::GasRedeposit => gas_redeposit(db, function, lowered),
            OptimizationPhase::Validate => validate(lowered).unwrap_or_else(|err| {
                panic!(
                    "Failed validation for function {}: {:?}",
                    function.full_path(db),
                    err.to_message()
                )
            }),
            OptimizationPhase::SubStrategy { strategy, iterations } => {
                for _ in 1..iterations {
                    let before = lowered.clone();
                    strategy.apply_strategy(db, function, lowered)?;
                    if *lowered == before {
                        return Ok(());
                    }
                }
                strategy.apply_strategy(db, function, lowered)?
            }
        }
        Ok(())
    }
}

define_short_id!(OptimizationStrategyId, OptimizationStrategy<'db>);

/// A strategy is a sequence of optimization phases.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OptimizationStrategy<'db>(pub Vec<OptimizationPhase<'db>>);

impl<'db> OptimizationStrategyId<'db> {
    /// Applies the optimization strategy phase to the lowering.
    ///
    /// Assumes `lowered` is a lowering of `function`.
    pub fn apply_strategy(
        self,
        db: &'db dyn Database,
        function: ConcreteFunctionWithBodyId<'db>,
        lowered: &mut Lowered<'db>,
    ) -> Maybe<()> {
        for phase in self.long(db).0.clone() {
            phase.apply(db, function, lowered)?;
        }

        Ok(())
    }
}

/// Query implementation of [crate::db::LoweringGroup::baseline_optimization_strategy].
#[salsa::tracked]
pub fn baseline_optimization_strategy<'db>(db: &'db dyn Database) -> OptimizationStrategyId<'db> {
    match db.optimizations() {
        Optimizations::Enabled(_) => {
            OptimizationStrategy(vec![
                // Must be right before inlining.
                OptimizationPhase::ReorganizeBlocks,
                OptimizationPhase::ApplyInlining { enable_const_folding: true },
                OptimizationPhase::ReturnOptimization,
                OptimizationPhase::ReorganizeBlocks,
                OptimizationPhase::ReorderStatements,
                OptimizationPhase::BranchInversion,
                OptimizationPhase::CancelOps,
                // Must be right before const folding.
                OptimizationPhase::ReorganizeBlocks,
                OptimizationPhase::ConstFolding,
                OptimizationPhase::OptimizeMatches,
                OptimizationPhase::SplitStructs,
                OptimizationPhase::ReorganizeBlocks,
                OptimizationPhase::ReorderStatements,
                OptimizationPhase::OptimizeMatches,
                OptimizationPhase::ReorganizeBlocks,
                OptimizationPhase::CancelOps,
                OptimizationPhase::ReorganizeBlocks,
                // Performing CSE here after blocks are the most contiguous, to reach maximum
                // effect.
                OptimizationPhase::Cse,
                OptimizationPhase::DedupBlocks,
                // Re-run ReturnOptimization to eliminate harmful merges introduced by DedupBlocks.
                OptimizationPhase::ReturnOptimization,
                OptimizationPhase::ReorderStatements,
                OptimizationPhase::ReorganizeBlocks,
            ])
        }
        Optimizations::Disabled => OptimizationStrategy(vec![OptimizationPhase::ApplyInlining {
            enable_const_folding: false,
        }]),
    }
    .intern(db)
}

/// Query implementation of [crate::db::LoweringGroup::final_optimization_strategy].
#[salsa::tracked]
pub fn final_optimization_strategy<'db>(db: &'db dyn Database) -> OptimizationStrategyId<'db> {
    match db.optimizations() {
        Optimizations::Enabled(_) => {
            OptimizationStrategy(vec![
                OptimizationPhase::GasRedeposit,
                OptimizationPhase::EarlyUnsafePanic,
                // Apply `TrimUnreachable` here to remove unreachable `redeposit_gas` and
                // `unsafe_panic` calls.
                OptimizationPhase::TrimUnreachable,
                OptimizationPhase::LowerImplicits,
                OptimizationPhase::ReorganizeBlocks,
            ])
        }
        Optimizations::Disabled => OptimizationStrategy(vec![
            OptimizationPhase::LowerImplicits,
            OptimizationPhase::ReorganizeBlocks,
        ]),
    }
    .intern(db)
}
