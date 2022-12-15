use std::collections::HashMap;

use cost_expr::Var;
use gas_info::GasInfo;
use sierra::extensions::builtin_cost::CostTokenType;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program::{Program, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;
use utils::ordered_hash_map::OrderedHashMap;

pub mod core_libfunc_cost;
mod core_libfunc_cost_base;
mod core_libfunc_cost_expr;
mod cost_expr;
pub mod gas_info;
mod generate_equations;
mod starknet_libfunc_cost_base;

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

    let mut variable_values = HashMap::<(StatementIdx, CostTokenType), i64>::default();
    let mut function_costs =
        HashMap::<sierra::ids::FunctionId, OrderedHashMap<CostTokenType, i64>>::default();
    for (token_type, token_equations) in equations {
        let solution = solver::try_solve_equations(token_equations)
            .ok_or(CostError::SolvingGasEquationFailed)?;
        for func in &program.funcs {
            let id = &func.id;
            if !function_costs.contains_key(id) {
                function_costs.insert(id.clone(), OrderedHashMap::default());
            }
            let value = solution[&Var::StatementFuture(func.entry_point, token_type)];
            if value != 0 {
                function_costs.get_mut(id).unwrap().insert(token_type, value);
            }
        }
        for (var, value) in solution {
            if let Var::LibFuncImplicitGasVariable(idx, var_token_type) = var {
                assert_eq!(
                    token_type, var_token_type,
                    "Unexpected variable of type {var_token_type:?} while handling {token_type:?}."
                );
                variable_values.insert((idx, var_token_type), value);
            }
        }
    }
    Ok(GasInfo { variable_values, function_costs })
}
