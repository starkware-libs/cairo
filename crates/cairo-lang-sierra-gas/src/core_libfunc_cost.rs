use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::gas::{CostTokenMap, CostTokenType};
use cairo_lang_sierra::program::StatementIdx;
use itertools::zip_eq;

use crate::core_libfunc_cost_base::{
    CostOperations, FunctionCostInfo, core_libfunc_postcost, core_libfunc_precost,
};
pub use crate::core_libfunc_cost_base::{
    DICT_SQUASH_FIXED_COST, DICT_SQUASH_REPEATED_ACCESS_COST, DICT_SQUASH_UNIQUE_KEY_COST,
    InvocationCostInfoProvider, SEGMENT_ARENA_ALLOCATION_COST,
};
use crate::gas_info::GasInfo;
pub use crate::starknet_libfunc_cost_base::SYSTEM_CALL_COST;

/// Cost operations for getting `CostTokenMap<i64>` costs values.
struct Ops<'a> {
    gas_info: &'a GasInfo,
    idx: StatementIdx,
}
impl CostOperations for Ops<'_> {
    type CostValueType = i64;

    fn cost_token(&self, value: i32) -> Self::CostValueType {
        value as i64
    }

    fn function_token_cost(
        &mut self,
        function: &FunctionCostInfo,
        token_type: CostTokenType,
    ) -> Option<Self::CostValueType> {
        self.gas_info.function_costs.get(&function.id)?.get(&token_type).copied()
    }

    fn statement_var_cost(&self, token_type: CostTokenType) -> Option<Self::CostValueType> {
        self.gas_info.variable_values.get(&(self.idx, token_type)).copied()
    }
}

/// Returns the gas cost for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_cost<InfoProvider: InvocationCostInfoProvider>(
    gas_info: &GasInfo,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<CostTokenMap<i64>> {
    let precost = core_libfunc_precost(&mut Ops { gas_info, idx: *idx }, libfunc, info_provider);
    let postcost = core_libfunc_postcost(&mut Ops { gas_info, idx: *idx }, libfunc, info_provider);
    zip_eq(precost, postcost)
        .map(|(mut precost, postcost)| {
            if precost.is_empty() {
                postcost
            } else {
                precost.extend(postcost);
                precost
            }
        })
        .collect()
}
