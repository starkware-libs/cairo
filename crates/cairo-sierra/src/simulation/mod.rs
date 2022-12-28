use std::collections::HashMap;

use itertools::izip;
use thiserror::Error;

use self::value::CoreValue;
use crate::edit_state::{put_results, take_args, EditStateError};
use crate::extensions::core::{CoreConcreteLibFunc, CoreLibFunc, CoreType};
use crate::ids::{FunctionId, VarId};
use crate::program::{Program, Statement, StatementIdx};
use crate::program_registry::{ProgramRegistry, ProgramRegistryError};

pub mod core;
#[cfg(test)]
mod test;
pub mod value;

/// Error occurring while simulating a libfunc.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum LibFuncSimulationError {
    #[error("Expected different number of arguments")]
    WrongNumberOfArgs,
    #[error("Expected a different type of an argument")]
    WrongArgType,
    #[error("Expected a different memory layout")]
    MemoryLayoutMismatch,
    #[error("Could not resolve requested symbol value")]
    UnresolvedStatementGasInfo,
    #[error("Error occurred during user function call")]
    FunctionSimulationError(FunctionId, Box<SimulationError>),
}

/// Error occurring while simulating a program function.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SimulationError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
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
    statement_gas_info: &HashMap<StatementIdx, i64>,
    function_id: &FunctionId,
    inputs: Vec<CoreValue>,
) -> Result<Vec<CoreValue>, SimulationError> {
    let context = SimulationContext {
        program,
        statement_gas_info,
        registry: &ProgramRegistry::new(program)?,
    };
    context.simulate_function(function_id, inputs)
}

/// Helper class for runing the simulation.
struct SimulationContext<'a> {
    pub program: &'a Program,
    pub statement_gas_info: &'a HashMap<StatementIdx, i64>,
    pub registry: &'a ProgramRegistry<CoreType, CoreLibFunc>,
}
impl SimulationContext<'_> {
    /// Simulates the run of a function, even recursively.
    fn simulate_function(
        &self,
        function_id: &FunctionId,
        inputs: Vec<CoreValue>,
    ) -> Result<Vec<CoreValue>, SimulationError> {
        let func = self.registry.get_function(function_id)?;
        let mut current_statement_id = func.entry_point;
        if func.params.len() != inputs.len() {
            return Err(SimulationError::FunctionArgumentCountMismatch {
                function_id: func.id.clone(),
                expected: func.params.len(),
                actual: inputs.len(),
            });
        }
        let mut state = HashMap::<VarId, CoreValue>::from_iter(
            izip!(func.params.iter(), inputs.into_iter())
                .map(|(param, input)| (param.id.clone(), input)),
        );
        loop {
            let statement = self
                .program
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
                    let libfunc = self.registry.get_libfunc(&invocation.libfunc_id)?;
                    let (outputs, chosen_branch) = self.simulate_libfunc(
                        &current_statement_id,
                        libfunc,
                        inputs,
                        current_statement_id,
                    )?;
                    let branch_info = &invocation.branches[chosen_branch];
                    state = put_results(
                        remaining,
                        izip!(branch_info.results.iter(), outputs.into_iter()),
                    )
                    .map_err(|error| {
                        SimulationError::EditStateError(error, current_statement_id)
                    })?;
                    current_statement_id = current_statement_id.next(&branch_info.target);
                }
            }
        }
    }
    /// Simulates the run of libfuncs. Returns the memory reperesentations of the outputs given the
    /// inputs.
    fn simulate_libfunc(
        &self,
        idx: &StatementIdx,
        libfunc: &CoreConcreteLibFunc,
        inputs: Vec<CoreValue>,
        current_statement_id: StatementIdx,
    ) -> Result<(Vec<CoreValue>, usize), SimulationError> {
        core::simulate(
            libfunc,
            inputs,
            || self.statement_gas_info.get(idx).copied(),
            |function_id, inputs| {
                self.simulate_function(function_id, inputs).map_err(|error| {
                    LibFuncSimulationError::FunctionSimulationError(
                        function_id.clone(),
                        Box::new(error),
                    )
                })
            },
        )
        .map_err(|error| SimulationError::LibFuncSimulationError(error, current_statement_id))
    }
}
