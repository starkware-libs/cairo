//! Sierra gas computation.
//!
//! This crate provides the gas computation for the Cairo programs.

use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::ConcreteType;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use core_libfunc_cost_base::InvocationCostInfoProvider;
use cost_expr::Var;
use gas_info::GasInfo;
use thiserror::Error;

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

impl InvocationCostInfoProvider for ProgramRegistry<CoreType, CoreLibfunc> {
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.get_type(ty).unwrap().info().size as usize
    }
}

/// Calculates gas information for a given program.
pub fn calc_gas_info(program: &Program) -> Result<GasInfo, CostError> {
    let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new(program)?;
    let equations = generate_equations::generate_equations(
        program,
        |statement_future_cost, idx, libfunc_id| {
            let libfunc = registry
                .get_libfunc(libfunc_id)
                .expect("Program registery creation would have already failed.");
            core_libfunc_cost_expr::core_libfunc_cost_expr(
                statement_future_cost,
                idx,
                libfunc,
                &registry,
            )
        },
    )?;

    let mut variable_values = OrderedHashMap::default();
    let mut function_costs = OrderedHashMap::default();
    for (token_type, token_equations) in equations {
        let solution = cairo_lang_eq_solver::try_solve_equations(token_equations)
            .ok_or(CostError::SolvingGasEquationFailed)?;
        for func in &program.funcs {
            let id = &func.id;
            if !function_costs.contains_key(id) {
                function_costs.insert(id.clone(), OrderedHashMap::default());
            }
            let value = solution[Var::StatementFuture(func.entry_point, token_type)];
            if value != 0 {
                function_costs.get_mut(id).unwrap().insert(token_type, value);
            }
        }
        for (var, value) in solution {
            if let Var::LibfuncImplicitGasVariable(idx, var_token_type) = var {
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
