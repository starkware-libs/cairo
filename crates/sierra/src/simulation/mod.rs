use std::collections::HashMap;

use itertools::izip;
use thiserror::Error;

use self::mem_cell::MemCell;
use crate::edit_state::{put_results, take_args, EditStateError};
use crate::extensions::core::function_call::FunctionCallConcrete;
use crate::extensions::CoreConcrete;
use crate::ids::{FunctionId, VarId};
use crate::program::{Program, Statement, StatementIdx};
use crate::program_registry::{ProgramRegistry, ProgramRegistryError};

pub mod core;
pub mod mem_cell;
#[cfg(test)]
mod test;

/// Error occurring while simulating a libfunc.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum LibFuncSimulationError {
    #[error("Expected different number of arguments")]
    WrongNumberOfArgs,
    #[error("Expected a different memory layout")]
    MemoryLayoutMismatch,
    #[error("Cannot simulate this sort of libfunc")]
    CannotBeSimulated,
}

/// Error occurring while simulating a program function.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SimulationError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] ProgramRegistryError),
    #[error("error from editing a variable state")]
    EditStateError(EditStateError, StatementIdx),
    #[error("error from simulating a libfunc")]
    LibFuncSimulationError(LibFuncSimulationError, StatementIdx),
    #[error("jumped out of bounds during simulation")]
    StatementOutOfBounds(StatementIdx),
    #[error("unexpected number of arguments to function")]
    FunctionArgumentCountMismatch { function_id: FunctionId, expected: usize, actual: usize },
    #[error("identifiers left at function return")]
    FunctionDidNotConsumeAllArgs(FunctionId, StatementIdx),
}

/// Runs a function from the program with the given inputs.
pub fn run(
    program: &Program,
    entry_point: &FunctionId,
    inputs: Vec<Vec<MemCell>>,
) -> Result<Vec<Vec<MemCell>>, SimulationError> {
    let registry = ProgramRegistry::new(program)?;
    run_helper(program, &registry, entry_point, inputs)
}

/// Helper for the run function enabling recursive function calls.
fn run_helper(
    program: &Program,
    registry: &ProgramRegistry,
    entry_point: &FunctionId,
    inputs: Vec<Vec<MemCell>>,
) -> Result<Vec<Vec<MemCell>>, SimulationError> {
    let func = registry.get_function(entry_point)?;
    let mut current_statement_id = func.entry;
    if func.params.len() != inputs.len() {
        return Err(SimulationError::FunctionArgumentCountMismatch {
            function_id: func.id.clone(),
            expected: func.params.len(),
            actual: inputs.len(),
        });
    }
    let mut state = HashMap::<VarId, Vec<MemCell>>::from_iter(
        izip!(func.params.iter(), inputs.into_iter())
            .map(|(param, input)| (param.id.clone(), input)),
    );
    loop {
        let statement = program
            .get_statement(&current_statement_id)
            .ok_or(SimulationError::StatementOutOfBounds(current_statement_id))?;
        match statement {
            Statement::Return(ids) => {
                let (remaining, outputs) = take_args(state, ids.iter()).map_err(|error| {
                    SimulationError::EditStateError(error, current_statement_id)
                })?;
                return if remaining.is_empty() {
                    Ok(outputs)
                } else {
                    Err(SimulationError::FunctionDidNotConsumeAllArgs(
                        func.id.clone(),
                        current_statement_id,
                    ))
                };
            }
            Statement::Invocation(invocation) => {
                let (remaining, inputs) =
                    take_args(state, invocation.args.iter()).map_err(|error| {
                        SimulationError::EditStateError(error, current_statement_id)
                    })?;
                let libfunc = registry.get_libfunc(&invocation.libfunc_id)?;
                let (outputs, chosen_branch) = if let CoreConcrete::FunctionCall(
                    FunctionCallConcrete { function },
                ) = libfunc
                {
                    Ok((run_helper(program, registry, &function.id, inputs)?, 0))
                } else {
                    core::simulate(libfunc, inputs).map_err(|error| {
                        SimulationError::LibFuncSimulationError(error, current_statement_id)
                    })
                }?;
                let branch_info = &invocation.branches[chosen_branch];
                state =
                    put_results(remaining, izip!(branch_info.results.iter(), outputs.into_iter()))
                        .map_err(|error| {
                            SimulationError::EditStateError(error, current_statement_id)
                        })?;
                current_statement_id = current_statement_id.next(&branch_info.target);
            }
        }
    }
}
