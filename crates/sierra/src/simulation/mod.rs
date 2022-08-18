use std::collections::HashMap;

use itertools::izip;
use thiserror::Error;

use self::mem_cell::MemCell;
use crate::edit_state::{put_results, take_args, EditError};
use crate::ids::{FunctionId, VarId};
use crate::program::{BranchTarget, Program, Statement, StatementId};
use crate::program_registry::{ProgramRegistry, ProgramRegistryError};

pub mod core;
pub mod mem_cell;
#[cfg(test)]
mod tests;

/// Error occurring while testing extension inputs.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionSimulationError {
    #[error("Expected different number of arguments")]
    WrongNumberOfArgs,
    #[error("Expected a different memory layout")]
    MemoryLayoutMismatch,
}

/// Error occurring while testing extension inputs.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SimulationError {
    #[error("got an error from the program registry")]
    ProgramRegistryError(ProgramRegistryError),
    #[error("got an error from the editing the variable state")]
    EditError(EditError),
    #[error("got an error from simulating an extension")]
    ExtensionSimulationError(ExtensionSimulationError, StatementId),
    #[error("could not find the function to call")]
    MissingFunction,
    #[error("jumped out of bounds during simulation")]
    StatementOutOfBounds,
    #[error("unexpected number of arguments to function")]
    FunctionArgumentsMismatch,
    #[error("identifiers left at function return")]
    FunctionDidNotConsumeAllArgs,
}

/// Runs a program starting from entry point matching function.
pub fn run(
    program: &Program,
    entry_point: &FunctionId,
    inputs: Vec<Vec<MemCell>>,
) -> Result<Vec<Vec<MemCell>>, SimulationError> {
    let registry = ProgramRegistry::new(program).map_err(SimulationError::ProgramRegistryError)?;
    // TODO(orizi): use registry to get the function info when it is in the registry.
    let func = program
        .funcs
        .iter()
        .find(|func| &func.id == entry_point)
        .ok_or(SimulationError::MissingFunction)?;
    let mut current_statement_id = func.entry;
    if func.params.len() != inputs.len() {
        return Err(SimulationError::FunctionArgumentsMismatch);
    }
    let mut state = HashMap::<VarId, Vec<MemCell>>::from_iter(
        izip!(func.params.iter(), inputs.into_iter())
            .map(|(param, input)| (param.id.clone(), input)),
    );
    loop {
        if current_statement_id.0 >= program.statements.len() {
            return Err(SimulationError::StatementOutOfBounds);
        }
        match &program.statements[current_statement_id.0] {
            Statement::Return(ids) => {
                let (remaining, outputs) =
                    take_args(state, ids.iter()).map_err(SimulationError::EditError)?;
                return if remaining.is_empty() {
                    Ok(outputs)
                } else {
                    Err(SimulationError::FunctionDidNotConsumeAllArgs)
                };
            }
            Statement::Invocation(invocation) => {
                let (remaining, inputs) =
                    take_args(state, invocation.args.iter()).map_err(SimulationError::EditError)?;
                let extension = registry
                    .get_extension(&invocation.extension_id)
                    .map_err(SimulationError::ProgramRegistryError)?;
                let (outputs, chosen_branch) =
                    core::simulate(extension, inputs).map_err(|error| {
                        SimulationError::ExtensionSimulationError(error, current_statement_id)
                    })?;
                let branch_info = &invocation.branches[chosen_branch];
                state =
                    put_results(remaining, izip!(branch_info.results.iter(), outputs.into_iter()))
                        .map_err(SimulationError::EditError)?;
                current_statement_id = match &branch_info.target {
                    BranchTarget::Fallthrough => StatementId(current_statement_id.0 + 1),
                    BranchTarget::Statement(next_statement_id) => *next_statement_id,
                };
            }
        }
    }
}
