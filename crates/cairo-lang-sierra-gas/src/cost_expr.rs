use core::fmt;

use cairo_lang_eq_solver::Expr;
use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::program::StatementIdx;

/// Variable parts of a cost expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Var {
    /// Variables supplied for a libfunc at statement id (e.g. get_gas, refund_gas).
    LibfuncImplicitGasVariable(StatementIdx, CostTokenType),
    /// Variable marking on a statement's future cost (any route from it to a return).
    StatementFuture(StatementIdx, CostTokenType),
}
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::LibfuncImplicitGasVariable(idx, token_type) => {
                write!(f, "libfunc@{token_type:?}{idx}")
            }
            Var::StatementFuture(idx, token_type) => write!(f, "future@{token_type:?}{idx}"),
        }
    }
}

/// An expression of a full cost.
pub type CostExpr = Expr<Var>;
