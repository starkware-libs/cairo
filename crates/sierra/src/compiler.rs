use std::fmt::Display;

use casm::instructions::{Instruction, RetInstruction};
use thiserror::Error;

use crate::program::{Program, Statement, VarId};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod compiler_test;

#[derive(Error, Debug, PartialEq)]
pub enum CompilationError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("UnsupportedStatement")]
    UnsupportedStatement(Statement),
}

#[derive(Error, Debug, PartialEq)]
pub struct CairoProgram {
    instructions: Vec<Instruction>,
}
impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in self.instructions.iter() {
            writeln!(f, "{};", instruction)?
        }
        Ok(())
    }
}

pub fn compile(program: &Program) -> Result<CairoProgram, CompilationError> {
    let mut instructions = Vec::new();

    for statement in program.statements.iter() {
        match statement {
            Statement::Return(ref_ids) => {
                if let Some(ref_id) = ref_ids.iter().next() {
                    return Err(CompilationError::MissingReference(ref_id.clone()));
                }

                instructions.push(Instruction::Ret(RetInstruction {}));
            }
            Statement::Invocation(_invocation) => {
                return Err(CompilationError::UnsupportedStatement(statement.clone()));
            }
        }
    }
    Ok(CairoProgram { instructions })
}
