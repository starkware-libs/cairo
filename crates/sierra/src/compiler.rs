use std::collections::HashMap;
use std::fmt::Display;
use std::iter;

use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use casm::operand::{DerefOperand, Register, ResOperand};
use itertools::izip;
use thiserror::Error;

use crate::extensions::{ConcreteExtensionBox, ExtensionError, Extensions};
use crate::ids::{ConcreteExtensionId, VarId};
use crate::program::{ExtensionDeclaration, Function, Param, Program, Statement};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod compiler_test;

type SierraReference = ResOperand;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("ConcreateExtensionAlreadyDecleared")]
    ConcreateExtensionAlreadyDecleared(ConcreteExtensionId),

    #[error("Inconsistent References.")]
    InconsistentReferences,

    #[error("MissingReferecesForStatement")]
    MissingReferecesForStatement,

    #[error("InvalidEntryPoint")]
    InvalidEntryPoint,

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
    extension_declarations: &[ExtensionDeclaration],
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

/// Build the HashMap of references for the paramaters of a function.
pub fn build_functions_paramater_refs(
    params: &[Param],
) -> Result<HashMap<VarId, SierraReference>, CompilationError> {
    let mut refs = HashMap::with_capacity(params.len());

    let mut offset = -2;
    for param in params.iter().rev() {
        if refs
            .insert(
                param.id.clone(),
                SierraReference::Deref(DerefOperand { register: Register::FP, offset }),
            )
            .is_some()
        {
            return Err(CompilationError::InconsistentReferences);
        }
        // TODO(10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }

    Ok(refs)
}

pub fn init_reference(
    n_statements: usize,
    funcs: &[Function],
) -> Result<Vec<Option<HashMap<VarId, SierraReference>>>, CompilationError> {
    let mut refs: Vec<Option<HashMap<VarId, SierraReference>>> =
        iter::repeat_with(|| None).take(n_statements).collect();

    for func in funcs {
        if func.entry.0 >= n_statements {
            return Err(CompilationError::InvalidEntryPoint);
        }

        if refs[func.entry.0].is_some() {
            // References for that entry point where  already defined by a different function.
            return Err(CompilationError::InconsistentReferences);
        }

        refs[func.entry.0] = Some(build_functions_paramater_refs(&func.params)?);
    }

    Ok(refs)
}

pub fn compile(program: &Program) -> Result<CairoProgram, CompilationError> {
    let mut instructions = Vec::new();

    let extensions = collect_extensions(&program.extension_declarations)?;
    let all_refs = init_reference(program.statements.len(), &program.funcs)?;

    for (statement, refs) in izip!(&program.statements, &all_refs) {
        if refs.is_none() {
            return Err(CompilationError::MissingReferecesForStatement);
        }

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
