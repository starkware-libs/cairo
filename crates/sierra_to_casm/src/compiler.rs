use std::collections::HashMap;
use std::fmt::Display;

use casm::ap_change::{ApChangeError, ApplyApChange};
use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use itertools::zip_eq;
use sierra::edit_state::{put_results, take_args, EditStateError};
use sierra::extensions::ExtensionError;
use sierra::ids::VarId;
use sierra::program::{Program, Statement, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

use crate::invocations::compile_invocation;
use crate::references::{init_reference, ReferenceValue, ReferencesError};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("Error from program registry")]
    ProgramRegistryError(ProgramRegistryError),
    #[error(transparent)]
    ExtensionError(#[from] ExtensionError),
    #[error("MissingReferencesForStatement")]
    MissingReferencesForStatement,
    #[error(transparent)]
    ReferencesError(#[from] ReferencesError),
    #[error(transparent)]
    EditStateError(#[from] EditStateError),
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
}

#[derive(Error, Debug, Eq, PartialEq)]
pub struct CairoProgram {
    instructions: Vec<Instruction>,
}
impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in &self.instructions {
            writeln!(f, "{};", instruction)?
        }
        Ok(())
    }
}

pub fn compile(program: &Program) -> Result<CairoProgram, CompilationError> {
    let mut instructions = Vec::new();

    let registry = ProgramRegistry::new(program).map_err(CompilationError::ProgramRegistryError)?;
    let mut program_refs = init_reference(program.statements.len(), &program.funcs)?;

    for (statement_id, statement) in program.statements.iter().enumerate() {
        let statement_refs = program_refs.per_statement_refs[statement_id]
            .as_ref()
            .ok_or(CompilationError::MissingReferencesForStatement)?;

        match statement {
            Statement::Return(ref_ids) => {
                if let Some(ref_id) = ref_ids.iter().next() {
                    return Err(CompilationError::MissingReference(ref_id.clone()));
                }

                instructions.push(Instruction {
                    body: InstructionBody::Ret(RetInstruction {}),
                    inc_ap: false,
                });
            }
            Statement::Invocation(invocation) => {
                let extension = registry
                    .get_extension(&invocation.extension_id)
                    .map_err(CompilationError::ProgramRegistryError)?;

                let (statement_refs, invoke_refs) =
                    take_args(statement_refs.clone(), invocation.args.iter())?;
                let compiled_invocation = compile_invocation(extension, &invoke_refs)?;
                instructions.extend(compiled_invocation.instruction);

                // Update program_refs for all the target branches.
                for (branch_info, branch_result) in
                    zip_eq(&invocation.branches, compiled_invocation.results)
                {
                    let mut new_refs =
                        HashMap::with_capacity(statement_refs.len() + branch_result.refs.len());
                    for (var_id, ref_value) in &statement_refs {
                        new_refs.insert(
                            var_id.clone(),
                            ReferenceValue {
                                expression: ref_value
                                    .expression
                                    .clone()
                                    .apply_ap_change(branch_result.ap_change)?,
                            },
                        );
                    }

                    program_refs.set_or_assert(
                        StatementIdx(statement_id).next(&branch_info.target),
                        put_results(new_refs, zip_eq(&branch_info.results, branch_result.refs))?,
                    )?;
                }
            }
        }
    }
    Ok(CairoProgram { instructions })
}
