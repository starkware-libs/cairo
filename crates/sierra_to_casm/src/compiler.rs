use std::collections::HashMap;
use std::fmt::Display;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use sierra::extensions::{CoreConcrete, CoreExtension, ExtensionError, GenericExtensionEx};
use sierra::ids::{ConcreteExtensionId, GenericExtensionId, VarId};
use sierra::program::{ExtensionDeclaration, Program, Statement};
use thiserror::Error;

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
    #[error("ExtensionNotFound")]
    ExtensionNotFound(GenericExtensionId),
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
) -> Result<HashMap<ConcreteExtensionId, CoreConcrete>, CompilationError> {
    let mut extensions: HashMap<ConcreteExtensionId, CoreConcrete> = HashMap::new();

    for extension in extension_declarations {
        let concreate_ext =
            CoreExtension::specialize_by_id(&extension.generic_id, &extension.args)?;
        if extensions.insert(extension.id.clone(), concreate_ext).is_some() {
            return Err(CompilationError::ConcreateExtensionAlreadyDecleared(extension.id.clone()));
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
                gen_code(ext)?;
            }
        }
    }
    Ok(CairoProgram { instructions })
}

fn gen_code(_ext: &CoreConcrete) -> Result<Vec<Instruction>, ExtensionError> {
    Err(ExtensionError::NotImplemented)
}
