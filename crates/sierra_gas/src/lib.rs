use std::collections::HashMap;

use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program::{Program, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

mod core_libfunc_cost;
mod cost_expr;
mod generate_equations;
mod solve_equations;

#[cfg(test)]
mod test;

/// Error occurring while calculating the costing of a program's variables.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum CostError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] ProgramRegistryError),
    #[error("found an illegal statement index during cost calculations")]
    StatementOutOfBounds(StatementIdx),
    #[error("failed solving the symbol tables")]
    SolvingGasEquationFailed,
}

/// Calculates values for gas symbols for a given program.
pub fn calc_gas_symbols(program: &Program) -> Result<HashMap<StatementIdx, i64>, CostError> {
    let registry = ProgramRegistry::<CoreType, CoreLibFunc>::new(program)?;
    let equations = generate_equations::generate_equations(
        program,
        |statement_future_cost, idx, libfunc_id| {
            let libfunc = registry
                .get_libfunc(libfunc_id)
                .expect("Program registery creation would have already failed.");
            core_libfunc_cost::core_libfunc_cost(statement_future_cost, idx, libfunc)
        },
    )?;
    solve_equations::solve_equations(equations)
}
