use std::collections::HashMap;

// use thiserror::Error;
use crate::edit_state::{put_results, take_args, Error as EditError};
use crate::extensions::{Error as ExtError, Extensions};
use crate::program::{Identifier, Program, Statement, StatementId};

#[derive(Debug, PartialEq)]
pub enum SimError {
    Extension(ExtError, String),
    EditState(StatementId, EditError),
    FunctionNonTupleInput(String),
    FunctionArgsSizeMismatch(String, usize),
    MissingFunctionCall(Identifier),
}

// Simulates a Sierra program run of the function with the given id.
pub fn run(
    program: &Program,
    entry_function: &Identifier,
    inputs: Vec<Vec<i64>>,
) -> Result<Vec<Vec<i64>>, SimError> {
    let exts = Extensions::new();
    let f = program
        .funcs
        .iter()
        .find(|f| f.id == *entry_function)
        .ok_or_else(|| SimError::MissingFunctionCall(entry_function.clone()))?;
    let mut vars: HashMap<Identifier, Vec<i64>> =
        izip!(f.args.iter().map(|v| v.id.clone()), inputs.into_iter()).collect();
    let mut current_statement_id = f.entry;
    loop {
        match program.get_statement(current_statement_id) {
            Statement::Return(ref_ids) => {
                let (_remaining_vars, used_vars) = take_args(vars, ref_ids.iter())
                    .map_err(|e| SimError::EditState(current_statement_id, e))?;
                return Ok(used_vars);
            }
            Statement::Invocation(invocation) => {
                let (nvars, args_info) = take_args(vars, invocation.args.iter())
                    .map_err(|e| SimError::EditState(current_statement_id, e))?;
                let (results_info, chosen_branch_index) = exts
                    .simulate(&invocation.ext, args_info)
                    .map_err(|e| SimError::Extension(e, invocation.ext.to_string()))?;
                let chosen_branch = &invocation.branches[chosen_branch_index];
                vars = put_results(
                    nvars,
                    izip!(chosen_branch.results.iter(), results_info.into_iter()),
                )
                .map_err(|e| SimError::EditState(current_statement_id, e))?;
                current_statement_id = chosen_branch.target.get(current_statement_id);
            }
        }
    }
}
