use std::fmt::Display;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use sierra::extensions::{CoreLibFunc, CoreType};
use sierra::program::{Program, Statement, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

use crate::invocations::{check_references_on_stack, compile_invocation, InvocationError};
use crate::references::{init_reference, ReferencesError};
use crate::type_sizes::get_type_size_map;

#[cfg(test)]
#[path = "compiler_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Failed building type information")]
    FailedBuildingTypeInformation,
    #[error("Error from program registry")]
    ProgramRegistryError(ProgramRegistryError),
    #[error(transparent)]
    InvocationError(#[from] InvocationError),
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

    // Maps statement_idx to program_offset.
    let mut statement_offsets = Vec::with_capacity(program.statements.len());

    let registry = ProgramRegistry::<CoreType, CoreLibFunc>::new(program)
        .map_err(CompilationError::ProgramRegistryError)?;
    let type_sizes = get_type_size_map(program, &registry)
        .ok_or(CompilationError::FailedBuildingTypeInformation)?;
    let mut program_refs = init_reference(program.statements.len(), &program.funcs)?;

    let mut program_offset: usize = 0;

    for (statement_id, statement) in program.statements.iter().enumerate() {
        let statement_idx = StatementIdx(statement_id);

        statement_offsets.push(program_offset);

        match statement {
            Statement::Return(ref_ids) => {
                let (_statement_refs, return_refs) =
                    program_refs.take_references(statement_idx, ref_ids.iter())?;
                check_references_on_stack(&type_sizes, &return_refs)?;
                // TODO(orizi): Also test types of the results using 'check_types_match' when the
                // returning function is available.

                instructions.push(Instruction {
                    body: InstructionBody::Ret(RetInstruction {}),
                    inc_ap: false,
                });
            }
            Statement::Invocation(invocation) => {
                let (statement_refs, invoke_refs) =
                    program_refs.take_references(statement_idx, invocation.args.iter())?;

                let libfunc = registry
                    .get_libfunc(&invocation.libfunc_id)
                    .map_err(CompilationError::ProgramRegistryError)?;
                let compiled_invocation = compile_invocation(&type_sizes, libfunc, &invoke_refs)?;

                for instruction in &instructions {
                    program_offset += instruction.body.op_size();
                }
                instructions.extend(compiled_invocation.instructions.into_iter());

                program_refs.update_references(
                    statement_idx,
                    statement_refs,
                    &invocation.branches,
                    compiled_invocation.results.into_iter(),
                )?;
            }
        }
    }
    Ok(CairoProgram { instructions })
}
