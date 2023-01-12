use std::fmt::Debug;
use std::hash::Hash;

use cairo_lang_utils::collection_arithmetics::{add_maps, sub_maps, HasZero};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

/// An linear expression of varialbes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<Var: Clone + Debug + PartialEq + Eq + Hash> {
    /// The constant term of the expression.
    pub const_term: i32,
    /// The coefficient for every variable in the expression.
    pub var_to_coef: OrderedHashMap<Var, i64>,
}
impl<Var: Clone + Debug + PartialEq + Eq + Hash> Expr<Var> {
    /// Creates a cost expression based on const value only.
    pub fn from_const(const_term: i32) -> Self {
        Self { const_term, var_to_coef: Default::default() }
    }

    /// Creates a cost expression based on variable only.
    pub fn from_var(var: Var) -> Self {
        Self { const_term: 0, var_to_coef: [(var, 1)].into_iter().collect() }
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
            var_to_coef: add_maps(self.var_to_coef, other.var_to_coef),
        }
    }
}

impl<Var: Clone + Debug + PartialEq + Eq + Hash> std::ops::Sub for Expr<Var> {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self {
            const_term: self.const_term - other.const_term,
            var_to_coef: sub_maps(self.var_to_coef, other.var_to_coef),
        }
    }
}
