use std::collections::HashMap;

use crate::edit_state::{put_results, take_args, Error as EditError};
use crate::extensions::{Error as ExtError, Extensions};
use crate::program::{BranchTarget, Identifier, Program, Statement, StatementId};

#[derive(Debug, PartialEq)]
pub enum Error {
    Extension(ExtError, String),
    EditState(StatementId, EditError),
    FunctionNonTupleInput(String),
    FunctionArgsSizeMismatch(String, usize),
    MissingFunctionCall(Identifier),
}

// Simulates a Sierra program run of the function with the given id.
pub fn run(prog: &Program, id: &Identifier, inputs: Vec<Vec<i64>>) -> Result<Vec<Vec<i64>>, Error> {
    let exts = Extensions::new();
    let f = prog
        .funcs
        .iter()
        .find(|f| f.id == *id)
        .ok_or_else(|| Error::MissingFunctionCall(id.clone()))?;
    let mut vars = HashMap::<Identifier, Vec<i64>>::from_iter(
        izip!(f.args.iter(), inputs.into_iter()).map(|(v, input)| (v.id.clone(), input)),
    );
    let mut s_id = f.entry;
    loop {
        match &prog.statements[s_id.0] {
            Statement::Return(ref_ids) => {
                let (_, used_vars) =
                    take_args(vars, ref_ids.iter()).map_err(|e| Error::EditState(s_id, e))?;
                return Ok(used_vars);
            }
            Statement::Invocation(invc) => {
                let (nvars, args_info) =
                    take_args(vars, invc.args.iter()).map_err(|e| Error::EditState(s_id, e))?;
                let (results_info, chosen_branch) = exts
                    .simulate(&invc.ext, args_info)
                    .map_err(|e| Error::Extension(e, invc.ext.to_string()))?;
                let chosen_branch = &invc.branches[chosen_branch];
                vars = put_results(
                    nvars,
                    izip!(chosen_branch.results.iter(), results_info.into_iter()),
                )
                .map_err(|e| Error::EditState(s_id, e))?;
                s_id = match chosen_branch.target {
                    BranchTarget::Statement(next) => next,
                    BranchTarget::Fallthrough => StatementId(s_id.0 + 1),
                };
            }
        }
    }
}
