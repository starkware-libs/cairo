use std::fmt::Display;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use sierra::extensions::{CoreConcrete, ExtensionError};
use sierra::ids::VarId;
use sierra::program::{Program, Statement};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

use crate::references::{init_reference, Reference, ReferencesError};

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
    #[error("MissingReferencesForStatement")]
    MissingReferencesForStatement,
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
        let _statement_refs = program_refs.per_statement_refs[statement_id]
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
                compile_invocation(extension)?;
            }
        }
    }
    Ok(CairoProgram { instructions })
}

/// Describes the changes to the set of reference at the branch target.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchRefChanges {
    // New references defined at a given branch.
    // should correspond to BranchInfo.results.
    refs: Vec<Reference>,
    ap_change: i16,
}

#[derive(Debug, Eq, PartialEq)]
struct CompiledInvocation {
    // A vector instructions that implement the Invocation.
    instruction: Vec<Instruction>,
    // A vector of BranchRefChanges, should correspond to Invocation.branches.
    results: Vec<BranchRefChanges>,
}

fn compile_invocation(_ext: &CoreConcrete) -> Result<CompiledInvocation, ExtensionError> {
    Err(ExtensionError::NotImplemented)
}
