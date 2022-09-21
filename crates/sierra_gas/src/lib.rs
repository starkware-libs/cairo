use sierra::program::StatementIdx;
use thiserror::Error;

#[allow(dead_code)]
mod cost_expr;
#[allow(dead_code)]
mod generate_equations;

/// Error occurring while calculating the costing of a program's variables.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum CostError {
    #[error("found an illegal statement index during cost calculations")]
    StatementOutOfBounds(StatementIdx),
}
