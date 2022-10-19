use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::program::StatementIdx;

use crate::core_libfunc_cost_base::{core_libfunc_cost_base, CostOperations};
use crate::cost_expr::{CostExpr, Var};
use crate::generate_equations::StatementFutureCost;

/// Cost operations for getting `CostExpr` costs values.
struct Ops<'a> {
    statement_future_cost: &'a mut dyn StatementFutureCost,
    idx: StatementIdx,
}
impl CostOperations for Ops<'_> {
    type CostType = CostExpr;

    fn const_cost(&self, value: i32) -> Self::CostType {
        CostExpr::from_const(value)
    }

    fn function_cost(&mut self, function: &sierra::program::Function) -> Self::CostType {
        self.statement_future_cost.get_future_cost(&function.entry_point).clone()
    }

    fn statement_var_cost(&self) -> Self::CostType {
        CostExpr::from_var(Var::LibFuncImplicitGasVariable(self.idx))
    }

    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        lhs + rhs
    }

    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType {
        lhs - rhs
    }
}

/// Returns an expression for the gas cost for core libfuncs.
pub fn core_libfunc_cost_expr(
    statement_future_cost: &mut dyn StatementFutureCost,
    idx: &StatementIdx,
    libfunc: &CoreConcreteLibFunc,
) -> Vec<CostExpr> {
    core_libfunc_cost_base(&mut Ops { statement_future_cost, idx: *idx }, libfunc)
}
