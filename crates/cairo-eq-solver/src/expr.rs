use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use itertools::chain;
use utils::collection_arithmetics::HasZero;

#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

/// An linear expression of varialbes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<Var: Clone + Debug + PartialEq + Eq + Hash> {
    /// The constant term of the expression.
    pub const_term: i32,
    /// The coefficient for every variable in the expression.
    pub var_to_coef: HashMap<Var, i64>,
}
impl<Var: Clone + Debug + PartialEq + Eq + Hash> Expr<Var> {
    /// Creates a cost expression based on const value only.
    pub fn from_const(const_term: i32) -> Self {
        Self { const_term, var_to_coef: HashMap::default() }
    }

    /// Creates a cost expression based on variable only.
    pub fn from_var(var: Var) -> Self {
        Self { const_term: 0, var_to_coef: HashMap::from([(var, 1)]) }
    }
}

impl<Var: Clone + Debug + PartialEq + Eq + Hash> HasZero for Expr<Var> {
    fn zero() -> Self {
        Self::from_const(0)
    }
}

// Expr operators can be optimized if necessary.
impl<Var: Clone + Debug + PartialEq + Eq + Hash> std::ops::Add for Expr<Var> {
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

impl<Var: Clone + Debug + PartialEq + Eq + Hash> std::ops::Sub for Expr<Var> {
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
