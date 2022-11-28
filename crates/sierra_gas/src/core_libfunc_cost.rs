use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::program::StatementIdx;
use utils::collection_arith::{add_maps, sub_maps};
use utils::ordered_hash_map::OrderedHashMap;

use crate::core_libfunc_cost_base::{core_libfunc_cost_base, CostOperations, CostTokenType};
use crate::gas_info::GasInfo;

/// Cost operations for getting `Option<i64>` costs values.
struct Ops<'a> {
    gas_info: &'a GasInfo,
    idx: StatementIdx,
}
impl CostOperations for Ops<'_> {
    type CostType = Option<OrderedHashMap<CostTokenType, i64>>;

    fn const_cost(&self, value: i32) -> Self::CostType {
        Some(OrderedHashMap::from_iter([(CostTokenType::Step, value as i64)]))
    }

    fn function_cost(&mut self, function: &sierra::program::Function) -> Self::CostType {
        Some(OrderedHashMap::from_iter([(
            CostTokenType::Step,
            self.gas_info.function_costs.get(&function.id)?.clone(),
        )]))
    }

    fn statement_var_cost(&self) -> Self::CostType {
        Some(OrderedHashMap::from_iter([(
            CostTokenType::Step,
            self.gas_info.variable_values.get(&self.idx)?.clone(),
        )]))
    }

    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        Some(add_maps(lhs?, rhs?))
    }

    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        Some(sub_maps(lhs?, rhs?))
    }
}

/// Returns the gas usage for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_cost(
    gas_info: &GasInfo,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<Option<OrderedHashMap<CostTokenType, i64>>> {
    core_libfunc_cost_base(&mut Ops { gas_info, idx: *idx }, libfunc)
}
