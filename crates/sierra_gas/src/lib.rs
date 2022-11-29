pub use core_libfunc_cost_base::CostTokenType;
use cost_expr::Var;
use gas_info::GasInfo;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program::{Program, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

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
            core_libfunc_cost_expr::core_libfunc_cost_expr(statement_future_cost, idx, libfunc)
        },
    )?;
    // TODO(lior): Output the costs in other tokens as well.
    // TODO(lior): Remove the clone() below.
    let solution = solve_equations::solve_equations(equations[CostTokenType::Step].clone())?;
    let function_costs = program
        .funcs
        .iter()
        .map(|f| {
            (f.id.clone(), solution[&Var::StatementFuture(f.entry_point, CostTokenType::Step)])
        })
        .collect();
    let variable_values = solution
        .into_iter()
        .filter_map(|(var, value)| {
            Some((
                match var {
                    Var::LibFuncImplicitGasVariable(v, CostTokenType::Step) => Some(v),
                    _ => None,
                }?,
                value,
            ))
        })
        .collect();
    Ok(GasInfo { variable_values, function_costs })
}
