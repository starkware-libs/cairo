use std::collections::HashMap;
use std::fmt::Display;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use thiserror::Error;

use crate::extensions::{ConcreteExtensionBox, ExtensionError, Extensions};
use crate::ids::{ConcreteExtensionId, VarId};
use crate::program::{ExtensionDeclaration, Program, Statement};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod compiler_test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("ConcreateExtensionAlreadyDecleared")]
    ConcreateExtensionAlreadyDecleared(ConcreteExtensionId),
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

pub fn collect_extensions(
    extension_declarations: &Vec<ExtensionDeclaration>,
) -> Result<HashMap<ConcreteExtensionId, ConcreteExtensionBox>, CompilationError> {
    let mut extensions: HashMap<ConcreteExtensionId, ConcreteExtensionBox> = HashMap::new();

    for extentsion in extension_declarations {
        let concreate_ext =
            Extensions::default().specialize(&extentsion.generic_id, &extentsion.args)?;
        if extensions.insert(extentsion.id.clone(), concreate_ext).is_some() {
            return Err(CompilationError::ConcreateExtensionAlreadyDecleared(
                extentsion.id.clone(),
            ));
        };
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

                instructions.push(Instruction {
                    body: InstructionBody::Ret(RetInstruction {}),
                    inc_ap: false,
                });
            }
            Statement::Invocation(invocation) => {
                let ext = extensions.get(&invocation.extension_id).ok_or_else(|| {
                    ExtensionError::UndeclaredExtension {
                        extension_id: invocation.extension_id.clone(),
                    }
                })?;
                ext.gen_code()?;
            }
        }
    }
    Ok(CairoProgram { instructions })
}
