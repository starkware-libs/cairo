use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::Function;
use cairo_lang_utils::collection_arithmetics::{add_maps, sub_maps};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

/// Represents constant cost.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ConstCost {
    pub steps: i32,
    pub holes: i32,
    pub range_checks: i32,
}
impl ConstCost {
    pub const fn cost(&self) -> i32 {
        self.steps * 100 + self.holes * 10 + self.range_checks * 70
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

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PreCost(pub OrderedHashMap<CostTokenType, i32>);

/// Adds two [ConstCost] instances.
impl std::ops::Add for PreCost {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        PreCost(add_maps(self.0, rhs.0))
    }
}

/// Subtracts two [ConstCost] instances.
impl std::ops::Sub for PreCost {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        PreCost(sub_maps(self.0, rhs.0))
    }
}

/// The cost of executing a libfunc for a specific output branch.
#[derive(Clone)]
pub enum BranchCost {
    /// The cost of the statement is independent on other statements.
    Regular { const_cost: ConstCost, pre_cost: PreCost },
    /// A cost of a function call.
    FunctionCall { const_cost: ConstCost, function: Function },
    /// The cost of the `branch_align` libfunc.
    BranchAlign,
    /// The cost of `withdraw_gas` and `withdraw_gas_all` libfuncs.
    WithdrawGas { const_cost: ConstCost, success: bool, with_builtins: bool },
    /// The cost of the `redeposit_gas` libfunc.
    RedepositGas,
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
}
