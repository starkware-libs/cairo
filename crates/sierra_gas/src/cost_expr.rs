use core::fmt;
use std::collections::HashMap;

use itertools::chain;
use sierra::program::StatementIdx;
use utils::collection_arith::HasZero;

#[cfg(test)]
#[path = "cost_expr_test.rs"]
mod test;

/// Variable parts of a cost expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Var {
    /// Variables supplied for a libfunc at statement id (e.g. get_gas, refund_gas).
    LibFuncImplicitGasVariable(StatementIdx),
    /// Variable marking on a statement's future cost (any route from it to a return).
    StatementFuture(StatementIdx),
}
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::LibFuncImplicitGasVariable(idx) => write!(f, "libfunc@{}", idx),
            Var::StatementFuture(idx) => write!(f, "future@{}", idx),
        }
    }
}

/// An expression of a full cost.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CostExpr {
    /// The constant term of the expression.
    pub const_term: i32,
    /// The coefficient for every variable in the expression.
    pub var_to_coef: HashMap<Var, i64>,
}
impl CostExpr {
    /// Creates a cost expression based on const value only.
    pub fn from_const(const_term: i32) -> Self {
        Self { const_term, var_to_coef: HashMap::default() }
    }

    /// Creates a cost expression based on variable only.
    pub fn from_var(var: Var) -> Self {
        Self { const_term: 0, var_to_coef: HashMap::from([(var, 1)]) }
    }
}

impl HasZero for CostExpr {
    fn zero() -> Self {
        Self::from_const(0)
    }
}

// CostExpr operators can be optimized if necessary.
impl std::ops::Add for CostExpr {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            const_term: self.const_term + other.const_term,
            var_to_coef: chain!(
                self.var_to_coef
                    .iter()
                    .map(|(k, v1)| (k.clone(), v1 + other.var_to_coef.get(k).unwrap_or(&0))),
                other
                    .var_to_coef
                    .iter()
                    .map(|(k, v2)| (k.clone(), self.var_to_coef.get(k).unwrap_or(&0) + v2))
            )
            .filter(|(_, v)| *v != 0)
            .collect(),
        }
    }
}

impl std::ops::Sub for CostExpr {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self {
            const_term: self.const_term - other.const_term,
            var_to_coef: chain!(
                self.var_to_coef
                    .iter()
                    .map(|(k, v1)| (k.clone(), v1 - other.var_to_coef.get(k).unwrap_or(&0))),
                other
                    .var_to_coef
                    .iter()
                    .map(|(k, v2)| (k.clone(), self.var_to_coef.get(k).unwrap_or(&0) - v2))
            )
            .filter(|(_, v)| *v != 0)
            .collect(),
        }
    }
}
