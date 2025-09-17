use cairo_lang_sierra::extensions::circuit::CircuitInfo;
use cairo_lang_sierra::extensions::gas::{BuiltinCostsType, CostTokenMap, CostTokenType};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::collection_arithmetics::{AddCollection, SubCollection};

use crate::core_libfunc_cost_base::FunctionCostInfo;

/// Represents constant cost.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ConstCost {
    pub steps: i32,
    pub holes: i32,
    pub range_checks: i32,
    pub range_checks96: i32,
}
impl ConstCost {
    pub const fn cost(&self) -> i32 {
        self.steps * 100 + self.holes * 10 + self.range_checks * 70 + self.range_checks96 * 56
    }
    pub const fn steps(value: i32) -> Self {
        Self { steps: value, holes: 0, range_checks: 0, range_checks96: 0 }
    }
    pub const fn holes(value: i32) -> Self {
        Self { holes: value, steps: 0, range_checks: 0, range_checks96: 0 }
    }
    pub const fn range_checks(value: i32) -> Self {
        Self { range_checks: value, steps: 0, holes: 0, range_checks96: 0 }
    }
}

/// Adds two [ConstCost] instances.
impl ConstCost {
    // Note: this is necessary because `impl Add` does not support `const fn`.
    pub const fn add(self, rhs: Self) -> Self {
        Self {
            steps: self.steps + rhs.steps,
            holes: self.holes + rhs.holes,
            range_checks: self.range_checks + rhs.range_checks,
            range_checks96: self.range_checks96 + rhs.range_checks96,
        }
    }
}

/// Adds two [ConstCost] instances.
impl std::ops::Add for ConstCost {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.add(rhs)
    }
}

/// Subtracts two [ConstCost] instances.
impl std::ops::Sub for ConstCost {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            steps: self.steps - rhs.steps,
            holes: self.holes - rhs.holes,
            range_checks: self.range_checks - rhs.range_checks,
            range_checks96: self.range_checks96 - rhs.range_checks96,
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PreCost(pub CostTokenMap<i32>);
impl PreCost {
    pub fn builtin(token_type: CostTokenType) -> Self {
        Self(CostTokenMap::from_iter([(token_type, 1)]))
    }

    pub fn n_builtins(token_type: CostTokenType, n: i32) -> Self {
        Self(CostTokenMap::from_iter([(token_type, n)]))
    }
}

/// Adds two [PreCost] instances.
impl std::ops::Add for PreCost {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        PreCost(self.0.add_collection(rhs.0))
    }
}

/// Subtracts two [PreCost] instances.
impl std::ops::Sub for PreCost {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        PreCost(self.0.sub_collection(rhs.0))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BranchCostSign {
    /// Adds the cost to the wallet (e.g., in `coupon_refund`).
    Add,
    /// Subtracts the cost from the wallet (e.g., in `function_call`).
    Subtract,
}

/// The cost of executing a libfunc for a specific output branch.
#[derive(Clone, Debug)]
pub enum BranchCost {
    /// The cost of the statement is independent on other statements.
    Regular { const_cost: ConstCost, pre_cost: PreCost },
    /// A cost of a function.
    /// `sign` controls whether the cost (the function cost as well as `const_cost`) is added
    /// to or reduced from the wallet.
    FunctionCost { const_cost: ConstCost, function: FunctionCostInfo, sign: BranchCostSign },
    /// The cost of the `branch_align` libfunc.
    BranchAlign,
    /// The cost of `withdraw_gas` and `withdraw_gas_all` libfuncs.
    WithdrawGas(WithdrawGasBranchInfo),
    /// The cost of the `redeposit_gas` libfunc.
    RedepositGas,
}

/// Information about a branch of a `withdraw_gas` libfunc.
#[derive(Clone, Debug)]
pub struct WithdrawGasBranchInfo {
    /// Is this the success branch.
    pub success: bool,
    /// Is the builtin cost table supplied.
    pub with_builtin_costs: bool,
}
impl WithdrawGasBranchInfo {
    /// Returns the actual cost of the branch, not including the retrieved tokens, given the
    /// expected retrieved tokens per type.
    pub fn const_cost<TokenUsages: Fn(CostTokenType) -> usize>(
        &self,
        token_usages: TokenUsages,
    ) -> ConstCost {
        let cost_computation: i32 =
            BuiltinCostsType::cost_computation_steps(self.with_builtin_costs, token_usages)
                .into_or_panic();
        let mut steps = 3 + cost_computation;
        // Failure branch has some additional costs.
        if !self.success {
            if self.with_builtin_costs || cost_computation > 0 {
                // The additional jump to failure branch, and an additional minus 1 for the
                // range checked gas counter result.
                steps += 2;
            } else {
                // The additional jump to failure branch.
                steps += 1;
            }
        };
        ConstCost { steps, range_checks: 1, holes: 0, range_checks96: 0 }
    }
}

/// Converts [ConstCost] into [BranchCost].
impl From<ConstCost> for BranchCost {
    fn from(value: ConstCost) -> Self {
        BranchCost::Regular { const_cost: value, pre_cost: PreCost::default() }
    }
}

/// Trait for providing extra information required for calculating costs for a specific libfunc
/// invocation.
pub trait CostInfoProvider {
    /// Provides the sizes of types.
    fn type_size(&self, ty: &ConcreteTypeId) -> usize;

    /// Provides the info for the circuit.
    fn circuit_info(&self, ty: &ConcreteTypeId) -> &CircuitInfo;
}
