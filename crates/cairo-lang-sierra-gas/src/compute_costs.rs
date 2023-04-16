use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{Invocation, Program, Statement, StatementIdx};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::gas_info::GasInfo;
use crate::objects::BranchCost;

/// A trait for the cost type (either `PreCost` for pre-cost computation, or `i32` for the post-cost
/// computation).
pub trait CostTypeTrait: std::fmt::Debug + Default + Clone + Eq {
    fn max(values: impl Iterator<Item = Self>) -> Self;
}

/// Computes the [GasInfo] for a given program.
///
/// The `specific_cost_context` argument controls whether the computation is pre-cost or post-cost.
pub fn compute_costs<
    CostType: CostTypeTrait,
    SpecificCostContext: SpecificCostContextTrait<CostType>,
>(
    program: &Program,
    get_cost_fn: &dyn Fn(&ConcreteLibfuncId) -> Vec<BranchCost>,
    specific_cost_context: &SpecificCostContext,
) -> GasInfo {
    let mut context = CostContext { program, costs: UnorderedHashMap::default(), get_cost_fn };

    for i in 0..program.statements.len() {
        context.compute_wallet_at(&StatementIdx(i), specific_cost_context);
    }

    unimplemented!("GasInfo computation is not yet implemented.");
}

pub trait SpecificCostContextTrait<CostType: CostTypeTrait> {
    /// Returns the required value for the wallet for each branch.
    fn get_branch_requirements(
        &self,
        wallet_at_fn: &mut dyn FnMut(&StatementIdx) -> CostType,
        idx: &StatementIdx,
        invocation: &Invocation,
        libfunc_cost: &[BranchCost],
    ) -> Vec<CostType>;
}

/// Represents the status of the computation of the wallet at a given statement.
#[derive(Eq, PartialEq)]
enum CostComputationStatus<CostType> {
    /// The computation is in progress.
    InProgress,
    /// The computation was completed.
    Done(CostType),
}

/// Helper struct for computing the wallet value at each statement.
struct CostContext<'a, CostType> {
    /// The Sierra program.
    program: &'a Program,
    /// A callback function returning the cost of a libfunc for every output branch.
    get_cost_fn: &'a dyn Fn(&ConcreteLibfuncId) -> Vec<BranchCost>,
    /// The cost before executing a Sierra statement.
    costs: UnorderedHashMap<StatementIdx, CostComputationStatus<CostType>>,
}
impl<'a, CostType: CostTypeTrait> CostContext<'a, CostType> {
    /// Returns the cost of a libfunc for every output branch.
    fn get_cost(&self, libfunc_id: &ConcreteLibfuncId) -> Vec<BranchCost> {
        (self.get_cost_fn)(libfunc_id)
    }

    /// Returns the required value in the wallet before executing statement `idx`.
    ///
    /// Assumes that [Self::compute_wallet_at] was called before.
    ///
    /// For `branch_align` the function returns the result as if the alignment is zero (since the
    /// alignment is not know at this point).
    // TODO(lior): Remove the following annotation once `wallet_at` is used.
    #[allow(dead_code)]
    fn wallet_at(&self, idx: &StatementIdx) -> CostType {
        match self.costs.get(idx) {
            Some(CostComputationStatus::Done(res)) => res.clone(),
            _ => {
                panic!("Wallet value for statement {idx} was not yet computed.")
            }
        }
    }

    /// Same as [Self::wallet_at], but computes the value if it was not yet computed.
    fn compute_wallet_at<SpecificCostContext: SpecificCostContextTrait<CostType>>(
        &mut self,
        idx: &StatementIdx,
        specific_cost_context: &SpecificCostContext,
    ) -> CostType {
        match self.costs.get_mut(idx) {
            Some(CostComputationStatus::InProgress) => {
                panic!("Found an unexpected cycle during cost computation.")
            }
            Some(CostComputationStatus::Done(res)) => {
                return res.clone();
            }
            None => {}
        }

        // Mark the statement's computation as in-progress.
        self.costs.insert(*idx, CostComputationStatus::InProgress);

        // Compute the value.
        let res = self.no_cache_compute_wallet_at(idx, specific_cost_context);

        // Update the cache with the result.
        assert!(
            self.costs.insert(*idx, CostComputationStatus::Done(res.clone()))
                == Some(CostComputationStatus::InProgress),
            "Unexpected cost computation status."
        );
        res
    }

    /// Same as [Self::compute_wallet_at], except that the cache is not used.
    ///
    /// Calls [Self::compute_wallet_at] to get the wallet value of the following instructions.
    fn no_cache_compute_wallet_at<SpecificCostContext: SpecificCostContextTrait<CostType>>(
        &mut self,
        idx: &StatementIdx,
        specific_cost_context: &SpecificCostContext,
    ) -> CostType {
        match &self.program.get_statement(idx).unwrap() {
            Statement::Return(_) => Default::default(),
            Statement::Invocation(invocation) => {
                let libfunc_cost: Vec<BranchCost> = self.get_cost(&invocation.libfunc_id);

                // For each branch, compute the required value for the wallet.
                let branch_requirements: Vec<CostType> = specific_cost_context
                    .get_branch_requirements(
                        &mut |statement_idx| {
                            self.compute_wallet_at(statement_idx, specific_cost_context)
                        },
                        idx,
                        invocation,
                        &libfunc_cost,
                    );

                // The wallet value at the beginning of the statement is the maximal value
                // required by all the branches.
                CostType::max(branch_requirements.into_iter())
            }
        }
    }
}
