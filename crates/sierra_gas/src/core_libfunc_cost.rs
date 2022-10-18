use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::program::StatementIdx;

use crate::core_libfunc_cost_base::core_libfunc_cost_base;
use crate::gas_info::GasInfo;

/// Returns the gas usage for a core libfunc.
/// Values with unknown values will return as None.
pub fn core_libfunc_cost(
    gas_info: &GasInfo,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<Option<i64>> {
    core_libfunc_cost_base(
        |c| Some(c as i64),
        |function| gas_info.function_costs.get(&function.id).cloned(),
        || gas_info.variable_values.get(idx).cloned(),
        |a, b| if let (Some(a), Some(b)) = (a, b) { Some(a + b) } else { None },
        |a, b| if let (Some(a), Some(b)) = (a, b) { Some(a - b) } else { None },
        libfunc,
    )
}
