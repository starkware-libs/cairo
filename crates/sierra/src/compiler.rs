use std::collections::HashMap;
use std::fmt::Display;

use casm::instructions::{Instruction, RetInstruction};
use thiserror::Error;

use crate::extensions::{ConcreteExtensionBox, ExtensionError, Extensions, SpecializationError};
use crate::ids::{ConcreteExtensionId, VarId};
use crate::program::{ExtensionDeclaration, Program, Statement};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod compiler_test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("UnsupportedStatement")]
    UnsupportedStatement(Statement),
    #[error(transparent)]
    SpecializationError(#[from] SpecializationError),
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

pub fn collect_extensions(
    extension_declarations: &Vec<ExtensionDeclaration>,
) -> Result<HashMap<ConcreteExtensionId, ConcreteExtensionBox>, CompilationError> {
    let mut extensions: HashMap<ConcreteExtensionId, ConcreteExtensionBox> = HashMap::new();

    for extentsion in extension_declarations {
        let concreate_ext = Extensions::default()
            .specialize(&extentsion.generic_id, &extentsion.args)
            .map_err(|error| match error {
                ExtensionError::Specialization { extension_id: _, error } => error,
            })?;
        extensions.insert(extentsion.id.clone(), concreate_ext);
    }
    Ok(extensions)
}

pub fn compile(program: &Program) -> Result<CairoProgram, CompilationError> {
    let mut instructions = Vec::new();

    let extensions = collect_extensions(&program.extension_declarations)?;

    for statement in program.statements.iter() {
        match statement {
            Statement::Return(ref_ids) => {
                if let Some(ref_id) = ref_ids.iter().next() {
                    return Err(CompilationError::MissingReference(ref_id.clone()));
                }

                instructions.push(Instruction::Ret(RetInstruction {}));
            }
            Statement::Invocation(invocation) => {
                let ext = extensions
                    .get(&invocation.extension_id)
                    .ok_or(SpecializationError::UnsupportedLibCallName)?;
                ext.gen_code();
                return Err(CompilationError::UnsupportedStatement(statement.clone()));
            }
        }
    }
    Ok(CairoProgram { instructions })
}
