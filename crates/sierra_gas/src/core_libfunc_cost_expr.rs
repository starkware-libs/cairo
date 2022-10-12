use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::program::StatementIdx;

use crate::core_libfunc_cost_base::core_libfunc_cost_base;
use crate::cost_expr::{CostExpr, Var};
use crate::generate_equations::StatementFutureCost;

/// Returns an expression for the gas cost for core libfuncs.
pub fn core_libfunc_cost_expr(
    statement_future_cost: &mut dyn StatementFutureCost,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<CostExpr> {
    core_libfunc_cost_base(
        CostExpr::from_const,
        |function| statement_future_cost.get_future_cost(&function.entry_point).clone(),
        &|| CostExpr::from_var(Var::LibFuncImplicitGasVariable(*idx)),
        libfunc,
    )
}
