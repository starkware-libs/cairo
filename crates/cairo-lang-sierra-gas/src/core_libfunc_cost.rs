use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::collection_arithmetics::{add_maps, sub_maps};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

pub use crate::core_libfunc_cost_base::InvocationCostInfoProvider;
use crate::core_libfunc_cost_base::{core_libfunc_cost_base, CostOperations};
use crate::gas_info::GasInfo;

/// Cost operations for getting `Option<i64>` costs values.
struct Ops<'a> {
    gas_info: &'a GasInfo,
    idx: StatementIdx,
}
impl CostOperations for Ops<'_> {
    type CostType = Option<OrderedHashMap<CostTokenType, i64>>;

    fn const_cost(&self, value: i32) -> Self::CostType {
        self.const_cost_token(value, CostTokenType::Step)
    }

    fn const_cost_token(&self, value: i32, token_type: CostTokenType) -> Self::CostType {
        Some(OrderedHashMap::from_iter([(token_type, value as i64)]))
    }

    fn function_cost(&mut self, function: &cairo_lang_sierra::program::Function) -> Self::CostType {
        self.gas_info.function_costs.get(&function.id).cloned()
    }

    fn statement_var_cost(&self, token_type: CostTokenType) -> Self::CostType {
        Some(OrderedHashMap::from_iter([(
            token_type,
            *self.gas_info.variable_values.get(&(self.idx, token_type))?,
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
pub fn core_libfunc_cost<InfoProvider: InvocationCostInfoProvider>(
    gas_info: &GasInfo,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<Option<OrderedHashMap<CostTokenType, i64>>> {
    core_libfunc_cost_base(&mut Ops { gas_info, idx: *idx }, libfunc, info_provider)
}
