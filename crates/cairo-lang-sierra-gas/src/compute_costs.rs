use std::ops::{Add, Sub};

use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{BranchInfo, Invocation, Program, Statement, StatementIdx};
use cairo_lang_utils::iterators::zip_eq3;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::zip_eq;

use crate::gas_info::GasInfo;
use crate::objects::{BranchCost, PreCost};

type VariableValues = OrderedHashMap<(StatementIdx, CostTokenType), i64>;

/// A trait for the cost type (either [PreCost] for pre-cost computation, or `i32` for the post-cost
/// computation).
pub trait CostTypeTrait:
    std::fmt::Debug + Default + Clone + Eq + Add<Output = Self> + Sub<Output = Self>
{
    fn max(values: impl Iterator<Item = Self>) -> Self;
}

impl CostTypeTrait for PreCost {
    fn max(values: impl Iterator<Item = Self>) -> Self {
        let mut res = Self::default();
        for value in values {
            for (token_type, val) in value.0 {
                res.0.insert(token_type, std::cmp::max(*res.0.get(&token_type).unwrap_or(&0), val));
            }
        }
        res
    }
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

    let mut variable_values = VariableValues::default();
    for i in 0..program.statements.len() {
        analyze_gas_statements(
            &context,
            specific_cost_context,
            &StatementIdx(i),
            &mut variable_values,
        );
    }

    let function_costs = program
        .funcs
        .iter()
        .map(|func| {
            let res = SpecificCostContext::to_cost_map(
                context.wallet_at(&func.entry_point).get_pure_value(),
            );
            (func.id.clone(), res)
        })
        .collect();

    GasInfo { variable_values, function_costs }
}

/// Returns the statements whose wallet value is needed by
/// [get_branch_requirements].
fn get_branch_requirements_dependencies(
    idx: &StatementIdx,
    invocation: &Invocation,
    libfunc_cost: &[BranchCost],
) -> OrderedHashSet<StatementIdx> {
    let mut res: OrderedHashSet<StatementIdx> = Default::default();
    for (branch_info, branch_cost) in zip_eq(&invocation.branches, libfunc_cost) {
        match branch_cost {
            BranchCost::FunctionCall { const_cost: _, function } => {
                res.insert(function.entry_point);
            }
            BranchCost::WithdrawGas { const_cost: _, success: true, with_builtin_costs: _ } => {
                // If withdraw_gas succeeds, we don't need to take future_wallet_value into account,
                // so we simply return.
                continue;
            }
            _ => {}
        }
        res.insert(idx.next(&branch_info.target));
    }

    res
}

/// Returns the required value for the wallet for each branch.
fn get_branch_requirements<
    CostType: CostTypeTrait,
    SpecificCostContext: SpecificCostContextTrait<CostType>,
>(
    specific_context: &SpecificCostContext,
    wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<CostType>,
    idx: &StatementIdx,
    invocation: &Invocation,
    libfunc_cost: &[BranchCost],
) -> Vec<WalletInfo<CostType>> {
    zip_eq(&invocation.branches, libfunc_cost)
        .map(|(branch_info, branch_cost)| {
            specific_context.get_branch_requirement(wallet_at_fn, idx, branch_info, branch_cost)
        })
        .collect()
}

/// For every `branch_align` and `withdraw_gas` statements, computes the required cost variables.
///
/// * For `branch_align` this is the amount of cost *reduced* from the wallet.
/// * For `withdraw_gas` this is the amount that should be withdrawn and added to the wallet.
fn analyze_gas_statements<
    CostType: CostTypeTrait,
    SpecificCostContext: SpecificCostContextTrait<CostType>,
>(
    context: &CostContext<'_, CostType>,
    specific_context: &SpecificCostContext,
    idx: &StatementIdx,
    variable_values: &mut VariableValues,
) {
    let Statement::Invocation(invocation) = &context.program.get_statement(idx).unwrap() else {
            return;
        };
    let libfunc_cost: Vec<BranchCost> = context.get_cost(&invocation.libfunc_id);
    let branch_requirements: Vec<WalletInfo<CostType>> = get_branch_requirements(
        specific_context,
        &|statement_idx| context.wallet_at(statement_idx),
        idx,
        invocation,
        &libfunc_cost,
    );

    let wallet_value = context.wallet_at(idx).get_value();

    if invocation.branches.len() > 1 {
        for (branch_info, branch_cost, branch_requirement) in
            zip_eq3(&invocation.branches, &libfunc_cost, &branch_requirements)
        {
            let future_wallet_value = context.wallet_at(&idx.next(&branch_info.target)).get_value();
            // TODO(lior): Consider checking that idx.next(&branch_info.target) is indeed branch
            //   align.
            if let BranchCost::WithdrawGas { success: true, .. } = branch_cost {
                for (token_type, amount) in specific_context.get_withdraw_gas_values(
                    branch_cost,
                    &wallet_value,
                    future_wallet_value,
                ) {
                    assert_eq!(
                        variable_values.insert((*idx, token_type), std::cmp::max(amount, 0)),
                        None
                    );

                    assert_eq!(
                        variable_values.insert(
                            (idx.next(&branch_info.target), token_type),
                            std::cmp::max(-amount, 0),
                        ),
                        None
                    );
                }
            } else {
                for (token_type, amount) in specific_context
                    .get_branch_align_values(&wallet_value, &branch_requirement.get_value())
                {
                    assert_eq!(
                        variable_values.insert((idx.next(&branch_info.target), token_type), amount),
                        None
                    );
                }
            }
        }
    }
}

pub trait SpecificCostContextTrait<CostType: CostTypeTrait> {
    /// Converts a `CostType` to a [OrderedHashMap] from [CostTokenType] to i64.
    fn to_cost_map(cost: CostType) -> OrderedHashMap<CostTokenType, i64>;

    /// Computes the value that should be withdrawn and added to the wallet for each token type.
    fn get_withdraw_gas_values(
        &self,
        branch_cost: &BranchCost,
        wallet_value: &CostType,
        future_wallet_value: CostType,
    ) -> OrderedHashMap<CostTokenType, i64>;

    /// Computes the value that should be reduced from the wallet for each token type.
    fn get_branch_align_values(
        &self,
        wallet_value: &CostType,
        branch_requirement: &CostType,
    ) -> OrderedHashMap<CostTokenType, i64>;

    /// Returns the required value for the wallet for a single branch.
    fn get_branch_requirement(
        &self,
        wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<CostType>,
        idx: &StatementIdx,
        branch_info: &BranchInfo,
        branch_cost: &BranchCost,
    ) -> WalletInfo<CostType>;
}

/// The information about the wallet value at a given statement.
#[derive(Clone, Debug, Default)]
pub struct WalletInfo<CostType: CostTypeTrait> {
    /// The minimum wallet value before executing the statement.
    value: CostType,
}

impl<CostType: CostTypeTrait> WalletInfo<CostType> {
    fn merge(branches: Vec<Self>) -> Self {
        let max_value = CostType::max(branches.iter().map(|wallet_info| wallet_info.value.clone()));
        WalletInfo { value: max_value }
    }

    /// Returns the value.
    fn get_value(&self) -> CostType {
        self.value.clone()
    }

    /// Returns the value, assuming there are no used cost variables (panics otherwise).
    // TODO(lior): Support cost variables - currently this function is identical to `get_value()`.
    fn get_pure_value(self) -> CostType {
        self.get_value()
    }
}

/// Implements a cast from CostType to WalletInfo.
impl<CostType: CostTypeTrait> From<CostType> for WalletInfo<CostType> {
    fn from(value: CostType) -> Self {
        WalletInfo { value }
    }
}

/// Implements addition of WalletInfo.
impl<CostType: CostTypeTrait> std::ops::Add for WalletInfo<CostType> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        WalletInfo { value: self.value + other.value }
    }
}

/// Represents the status of the computation of the wallet at a given statement.
enum CostComputationStatus<CostType: CostTypeTrait> {
    /// The computation is in progress.
    InProgress,
    /// The computation was completed.
    Done(WalletInfo<CostType>),
}

/// Helper struct for computing the wallet value at each statement.
struct CostContext<'a, CostType: CostTypeTrait> {
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
    fn wallet_at(&self, idx: &StatementIdx) -> WalletInfo<CostType> {
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
    ) -> WalletInfo<CostType> {
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
        if !matches!(
            self.costs.insert(*idx, CostComputationStatus::Done(res.clone())),
            Some(CostComputationStatus::InProgress)
        ) {
            panic!("Unexpected cost computation status.")
        }
        res
    }

    /// Same as [Self::compute_wallet_at], except that the cache is not used.
    ///
    /// Calls [Self::compute_wallet_at] to get the wallet value of the following instructions.
    fn no_cache_compute_wallet_at<SpecificCostContext: SpecificCostContextTrait<CostType>>(
        &mut self,
        idx: &StatementIdx,
        specific_cost_context: &SpecificCostContext,
    ) -> WalletInfo<CostType> {
        match &self.program.get_statement(idx).unwrap() {
            Statement::Return(_) => Default::default(),
            Statement::Invocation(invocation) => {
                let libfunc_cost: Vec<BranchCost> = self.get_cost(&invocation.libfunc_id);

                for dependency in
                    get_branch_requirements_dependencies(idx, invocation, &libfunc_cost)
                {
                    self.compute_wallet_at(&dependency, specific_cost_context);
                }

                // For each branch, compute the required value for the wallet.
                let branch_requirements: Vec<WalletInfo<CostType>> = get_branch_requirements(
                    specific_cost_context,
                    &|statement_idx| self.wallet_at(statement_idx),
                    idx,
                    invocation,
                    &libfunc_cost,
                );

                // The wallet value at the beginning of the statement is the maximal value
                // required by all the branches.
                WalletInfo::merge(branch_requirements)
            }
        }
    }
}

pub struct PreCostContext {}

impl SpecificCostContextTrait<PreCost> for PreCostContext {
    fn to_cost_map(cost: PreCost) -> OrderedHashMap<CostTokenType, i64> {
        let res = cost.0;
        res.into_iter().map(|(token_type, val)| (token_type, val as i64)).collect()
    }

    fn get_withdraw_gas_values(
        &self,
        _branch_cost: &BranchCost,
        wallet_value: &PreCost,
        future_wallet_value: PreCost,
    ) -> OrderedHashMap<CostTokenType, i64> {
        let res = (future_wallet_value - wallet_value.clone()).0;
        CostTokenType::iter_precost()
            .map(|token_type| (*token_type, *res.get(token_type).unwrap_or(&0) as i64))
            .collect()
    }

    fn get_branch_align_values(
        &self,
        wallet_value: &PreCost,
        branch_requirement: &PreCost,
    ) -> OrderedHashMap<CostTokenType, i64> {
        let res = (wallet_value.clone() - branch_requirement.clone()).0;
        CostTokenType::iter_precost()
            .map(|token_type| (*token_type, *res.get(token_type).unwrap_or(&0) as i64))
            .collect()
    }

    fn get_branch_requirement(
        &self,
        wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<PreCost>,
        idx: &StatementIdx,
        branch_info: &BranchInfo,
        branch_cost: &BranchCost,
    ) -> WalletInfo<PreCost> {
        let branch_cost = match branch_cost {
            BranchCost::Regular { const_cost: _, pre_cost } => pre_cost.clone(),
            BranchCost::BranchAlign => Default::default(),
            BranchCost::FunctionCall { const_cost: _, function } => {
                wallet_at_fn(&function.entry_point).get_pure_value()
            }
            BranchCost::WithdrawGas { const_cost: _, success, with_builtin_costs: _ } => {
                if *success {
                    // If withdraw_gas succeeds, we don't need to take
                    // future_wallet_value into account, so we simply return.
                    return Default::default();
                } else {
                    Default::default()
                }
            }
            BranchCost::RedepositGas => {
                // TODO(lior): Replace with actually redepositing the gas.
                Default::default()
            }
        };
        let future_wallet_value = wallet_at_fn(&idx.next(&branch_info.target));
        WalletInfo::from(branch_cost) + future_wallet_value
    }
}
