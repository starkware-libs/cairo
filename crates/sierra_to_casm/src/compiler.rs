use std::fmt::Display;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use sierra::extensions::ExtensionError;
use sierra::program::{Program, Statement, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

use crate::invocations::compile_invocation;
use crate::references::{init_reference, ReferencesError};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Error from program registry")]
    ProgramRegistryError(ProgramRegistryError),
    #[error(transparent)]
    ExtensionError(#[from] ExtensionError),
    #[error(transparent)]
    ReferencesError(#[from] ReferencesError),
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
    let program_refs = init_reference(program.statements.len(), &program.funcs)?;

    for (statement_id, statement) in program.statements.iter().enumerate() {
        let statement_idx = StatementIdx(statement_id);
        match statement {
            Statement::Return(ref_ids) => {
                program_refs.take_references(statement_idx, ref_ids.iter())?;

                instructions.push(Instruction {
                    body: InstructionBody::Ret(RetInstruction {}),
                    inc_ap: false,
                });
            }
            Statement::Invocation(invocation) => {
                let libfunc = registry
                    .get_libfunc(&invocation.libfunc_id)
                    .map_err(CompilationError::ProgramRegistryError)?;
                compile_invocation(libfunc)?;
            }
        }
    }
    Ok(CairoProgram { instructions })
}
