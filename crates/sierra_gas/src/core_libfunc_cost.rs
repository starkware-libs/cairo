use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::program::StatementIdx;

use crate::core_libfunc_cost_base::core_libfunc_cost_base;
use crate::gas_info::GasInfo;

/// Returns the gas usage for a core libfunc.
pub fn core_libfunc_cost(
    gas_info: &GasInfo,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<i64> {
    core_libfunc_cost_base(
        |c| c as i64,
        |function| gas_info.function_costs[&function.id],
        || gas_info.variable_values[idx],
        libfunc,
    )
}
