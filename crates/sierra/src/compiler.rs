use std::fmt::Display;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use thiserror::Error;

use crate::extensions::ExtensionError;
use crate::ids::VarId;
use crate::program::{Program, Statement};
use crate::program_registry::{ProgramRegistry, ProgramRegistryError};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod compiler_test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("Error from program registry")]
    ProgramRegistryError(ProgramRegistryError),
    #[error(transparent)]
    ExtensionError(#[from] ExtensionError),
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

    for statement in program.statements.iter() {
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
                extension.gen_code()?;
            }
        }
    }
    Ok(CairoProgram { instructions })
}
