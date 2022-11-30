use sierra::program::StatementIdx;
use test_log::test;

use super::{CostExpr, Var};
use crate::CostTokenType;

#[test]
fn operations() {
    let as_var = |idx| Var::LibFuncImplicitGasVariable(StatementIdx(idx), CostTokenType::Step);
    assert_eq!(CostExpr::from_const(1) + CostExpr::from_const(2), CostExpr::from_const(3));
    assert_eq!(CostExpr::from_const(1) - CostExpr::from_const(2), CostExpr::from_const(-1));
    assert_eq!(
        CostExpr::from_var(as_var(5)) + CostExpr::from_const(2),
        CostExpr { const_term: 2, var_to_coef: [(as_var(5), 1)].into_iter().collect() }
    );
    assert_eq!(
        CostExpr::from_var(as_var(5)) - CostExpr::from_const(2),
        CostExpr { const_term: -2, var_to_coef: [(as_var(5), 1)].into_iter().collect() }
    );
    assert_eq!(
        CostExpr::from_var(as_var(2)) - CostExpr::from_var(as_var(2)),
        CostExpr::from_const(0)
    );
}
