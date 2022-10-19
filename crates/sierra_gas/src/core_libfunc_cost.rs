use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::program::StatementIdx;

use crate::core_libfunc_cost_base::{core_libfunc_cost_base, CostOperations};
use crate::gas_info::GasInfo;

/// Cost operations for getting `Option<i64>` costs values.
struct Ops<'a> {
    gas_info: &'a GasInfo,
    idx: StatementIdx,
}
impl CostOperations for Ops<'_> {
    type CostType = Option<i64>;

    fn const_cost(&self, value: i32) -> Self::CostType {
        Some(value as i64)
    }

    fn function_cost(&mut self, function: &sierra::program::Function) -> Self::CostType {
        self.gas_info.function_costs.get(&function.id).cloned()
    }

    fn statement_var_cost(&self) -> Self::CostType {
        self.gas_info.variable_values.get(&self.idx).cloned()
    }

    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) { Some(lhs + rhs) } else { None }
    }

    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) { Some(lhs - rhs) } else { None }
    }
}

/// Returns the gas usage for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_cost(
    gas_info: &GasInfo,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<Option<i64>> {
    core_libfunc_cost_base(&mut Ops { gas_info, idx: *idx }, libfunc)
}
