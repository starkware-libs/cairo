use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::gas::{CostTokenMap, CostTokenType};
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::collection_arithmetics::{AddCollection, SubCollection};
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

/// Cost operations for getting `Option<i64>` costs values.
struct Ops<'a> {
    gas_info: &'a GasInfo,
    idx: StatementIdx,
}
impl CostOperations for Ops<'_> {
    type CostType = CostTokenMap<i64>;

    fn cost_token(&self, value: i32, token_type: CostTokenType) -> Self::CostType {
        CostTokenMap::from_iter([(token_type, value as i64)])
    }

    fn function_token_cost(
        &mut self,
        function: &FunctionCostInfo,
        token_type: CostTokenType,
    ) -> Self::CostType {
        if let Some(function_cost) = self.gas_info.function_costs.get(&function.id)
            && let Some(v) = function_cost.get(&token_type)
        {
            CostTokenMap::from_iter([(token_type, *v)])
        } else {
            CostTokenMap::default()
        }
    }

    fn statement_var_cost(&self, token_type: CostTokenType) -> Self::CostType {
        if let Some(v) = self.gas_info.variable_values.get(&(self.idx, token_type)) {
            CostTokenMap::from_iter([(token_type, *v)])
        } else {
            CostTokenMap::default()
        }
    }

    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        lhs.add_collection(rhs)
    }

    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        lhs.sub_collection(rhs)
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
        .map(|(precost, postcost)| {
            CostTokenType::iter_casm_tokens()
                .map(|token| {
                    (
                        *token,
                        precost.get(token).copied().unwrap_or_default()
                            + postcost.get(token).copied().unwrap_or_default(),
                    )
                })
                .collect()
        })
        .collect()
}
