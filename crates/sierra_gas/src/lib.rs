pub use core_libfunc_cost_base::CostTokenType;
use cost_expr::Var;
use gas_info::GasInfo;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program::{Program, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;
use utils::try_extract_matches;

pub mod core_libfunc_cost;
mod core_libfunc_cost_base;
mod core_libfunc_cost_expr;
mod cost_expr;
pub mod gas_info;
mod generate_equations;
mod solve_equations;

#[cfg(test)]
mod test;

/// Error occurring while calculating the costing of a program's variables.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum CostError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error("found an illegal statement index during cost calculations")]
    StatementOutOfBounds(StatementIdx),
    #[error("failed solving the symbol tables")]
    SolvingGasEquationFailed,
}

/// Calculates gas information for a given program.
pub fn calc_gas_info(program: &Program) -> Result<GasInfo, CostError> {
    let registry = ProgramRegistry::<CoreType, CoreLibFunc>::new(program)?;
    let equations = generate_equations::generate_equations(
        program,
        |statement_future_cost, idx, libfunc_id| {
            let libfunc = registry
                .get_libfunc(libfunc_id)
                .expect("Program registery creation would have already failed.");
            // TODO(lior): Instead of taking only the steps, generate equations for all of the token
            //   types.
            core_libfunc_cost_expr::core_libfunc_cost_expr(statement_future_cost, idx, libfunc)
                .iter()
                .map(|x| x[CostTokenType::Step].clone())
                .collect()
        },
    )?;
    let solution = solve_equations::solve_equations(equations)?;
    let function_costs = program
        .funcs
        .iter()
        .map(|f| (f.id.clone(), solution[&Var::StatementFuture(f.entry_point)]))
        .collect();
    let variable_values = solution
        .into_iter()
        .filter_map(|(var, value)| {
            Some((try_extract_matches!(var, Var::LibFuncImplicitGasVariable)?, value))
        })
        .collect();
    Ok(GasInfo { variable_values, function_costs })
}
