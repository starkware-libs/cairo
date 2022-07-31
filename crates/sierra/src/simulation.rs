use std::collections::HashMap;

use thiserror::Error;

use crate::edit_state::{put_results, take_args, EditError};
use crate::extensions::{ExtensionError, Extensions};
use crate::program::{Identifier, Program, Statement, StatementId};

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("Extension handling error")]
    Extension(ExtensionError, String),
    #[error("State edit error")]
    EditState(StatementId, EditError),
    #[error("Requested function identifier missing")]
    MissingFunctionCall(Identifier),
}

// Simulates a Sierra program run of the function with the given id.
pub fn run(
    program: &Program,
    entry_function: &Identifier,
    inputs: Vec<Vec<i64>>,
) -> Result<Vec<Vec<i64>>, Error> {
    let exts = Extensions::new();
    let f = program
        .funcs
        .iter()
        .find(|f| f.id == *entry_function)
        .ok_or_else(|| Error::MissingFunctionCall(entry_function.clone()))?;
    let mut vars: HashMap<Identifier, Vec<i64>> =
        izip!(f.args.iter().map(|v| v.id.clone()), inputs.into_iter()).collect();
    let mut current_statement_id = f.entry;
    loop {
        match program.get_statement(current_statement_id) {
            Statement::Return(ref_ids) => {
                let (_remaining_vars, used_vars) = take_args(vars, ref_ids.iter())
                    .map_err(|e| Error::EditState(current_statement_id, e))?;
                return Ok(used_vars);
            }
            Statement::Invocation(invocation) => {
                let (nvars, args_info) = take_args(vars, invocation.args.iter())
                    .map_err(|e| Error::EditState(current_statement_id, e))?;
                let (results_info, chosen_branch_index) = exts
                    .simulate(&invocation.ext, args_info)
                    .map_err(|e| Error::Extension(e, invocation.ext.to_string()))?;
                let chosen_branch = &invocation.branches[chosen_branch_index];
                vars = put_results(
                    nvars,
                    izip!(chosen_branch.results.iter(), results_info.into_iter()),
                )
                .map_err(|e| Error::EditState(current_statement_id, e))?;
                current_statement_id = chosen_branch.target.get(current_statement_id);
            }
        }
    }
}
