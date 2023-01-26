use std::fmt::Display;

use cairo_lang_casm::instructions::{Instruction, InstructionBody, RetInstruction};
use cairo_lang_sierra::extensions::core::{CoreConcreteLibfunc, CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::ids::VarId;
use cairo_lang_sierra::program::{BranchTarget, Invocation, Program, Statement, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use itertools::zip_eq;
use thiserror::Error;

use crate::annotations::{AnnotationError, ProgramAnnotations, StatementAnnotations};
use crate::invocations::{
    check_references_on_stack, compile_invocation, InvocationError, ProgramInfo,
};
use crate::metadata::Metadata;
use crate::references::{check_types_match, ReferencesError};
use crate::relocations::{relocate_instructions, RelocationEntry};
use crate::type_sizes::get_type_size_map;

#[cfg(test)]
#[path = "compiler_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Failed building type information")]
    FailedBuildingTypeInformation,
    #[error("Error from program registry")]
    ProgramRegistryError(Box<ProgramRegistryError>),
    #[error(transparent)]
    AnnotationError(#[from] AnnotationError),
    #[error("#{statement_idx}: {error}")]
    InvocationError { statement_idx: StatementIdx, error: InvocationError },
    #[error("#{statement_idx}: Return arguments are not on the stack.")]
    ReturnArgumentsNotOnStack { statement_idx: StatementIdx },
    #[error("#{statement_idx}: {error}")]
    ReferencesError { statement_idx: StatementIdx, error: ReferencesError },
    #[error("#{statement_idx}: Invocation mismatched to libfunc")]
    LibfuncInvocationMismatch { statement_idx: StatementIdx },
    #[error("{var_id} is dangling at #{statement_idx}.")]
    DanglingReferences { statement_idx: StatementIdx, var_id: VarId },

    #[error("#{source_statement_idx}->#{destination_statement_idx}: Expected branch align")]
    ExpectedBranchAlign {
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
    },
}

/// The casm program representation.
#[derive(Debug, Eq, PartialEq)]
pub struct CairoProgram {
    pub instructions: Vec<Instruction>,
    pub debug_info: CairoProgramDebugInfo,
}
impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in &self.instructions {
            writeln!(f, "{instruction};")?
        }
        Ok(())
    }
}

/// The debug information of a compilation from Sierra to casm.
#[derive(Debug, Eq, PartialEq)]
pub struct SierraStatementDebugInfo {
    /// The offset of the sierra statement within the bytecode.
    pub code_offset: usize,
}

/// The debug information of a compilation from Sierra to casm.
#[derive(Debug, Eq, PartialEq)]
pub struct CairoProgramDebugInfo {
    /// The debug information per Sierra statement.
    pub sierra_statement_info: Vec<SierraStatementDebugInfo>,
}

/// Ensure the basic structure of the invocation is the same as the library function.
pub fn check_basic_structure(
    statement_idx: StatementIdx,
    invocation: &Invocation,
    libfunc: &CoreConcreteLibfunc,
) -> Result<(), CompilationError> {
    if invocation.args.len() != libfunc.param_signatures().len()
        || !itertools::equal(
            invocation.branches.iter().map(|branch| branch.results.len()),
            libfunc.output_types().iter().map(|types| types.len()),
        )
        || match libfunc.fallthrough() {
            Some(expected_fallthrough) => {
                invocation.branches[expected_fallthrough].target != BranchTarget::Fallthrough
            }
            None => false,
        }
    {
        Err(CompilationError::LibfuncInvocationMismatch { statement_idx })
    } else {
        Ok(())
    }
}

pub fn compile(
    program: &Program,
    metadata: &Metadata,
    gas_usage_check: bool,
) -> Result<CairoProgram, Box<CompilationError>> {
    let mut instructions = Vec::new();
    let mut relocations: Vec<RelocationEntry> = Vec::new();

    // Maps statement_idx to program_offset. The last value (for statement_idx=number-of-statements)
    // contains the final offset (the size of the program code segment).
    let mut statement_offsets = Vec::with_capacity(program.statements.len());

    let registry = ProgramRegistry::<CoreType, CoreLibfunc>::with_ap_change(
        program,
        metadata.ap_change_info.function_ap_change.clone(),
    )
    .map_err(CompilationError::ProgramRegistryError)?;
    let type_sizes = get_type_size_map(program, &registry)
        .ok_or(CompilationError::FailedBuildingTypeInformation)?;
    let mut program_annotations = ProgramAnnotations::create(
        program.statements.len(),
        &program.funcs,
        metadata,
        gas_usage_check,
        &type_sizes,
    )
    .map_err(|err| Box::new(err.into()))?;

    let mut program_offset: usize = 0;

    for (statement_id, statement) in program.statements.iter().enumerate() {
        let statement_idx = StatementIdx(statement_id);
        statement_offsets.push(program_offset);
        match statement {
            Statement::Return(ref_ids) => {
                let (annotations, return_refs) = program_annotations
                    .get_annotations_after_take_args(statement_idx, ref_ids.iter())
                    .map_err(|err| Box::new(err.into()))?;

                if let Some(var_id) = annotations.refs.keys().next() {
                    return Err(Box::new(CompilationError::DanglingReferences {
                        statement_idx,
                        var_id: var_id.clone(),
                    }));
                };

                program_annotations
                    .validate_final_annotations(
                        statement_idx,
                        &annotations,
                        &program.funcs,
                        metadata,
                        &return_refs,
                    )
                    .map_err(|err| Box::new(err.into()))?;
                check_references_on_stack(&return_refs).map_err(|error| match error {
                    InvocationError::InvalidReferenceExpressionForArgument => {
                        CompilationError::ReturnArgumentsNotOnStack { statement_idx }
                    }
                    _ => CompilationError::InvocationError { statement_idx, error },
                })?;

                let ret_instruction = RetInstruction {};
                program_offset += ret_instruction.op_size();
                instructions.push(Instruction::new(InstructionBody::Ret(ret_instruction), false));
            }
            Statement::Invocation(invocation) => {
                let (annotations, invoke_refs) = program_annotations
                    .get_annotations_after_take_args(statement_idx, invocation.args.iter())
                    .map_err(|err| Box::new(err.into()))?;

                let libfunc = registry
                    .get_libfunc(&invocation.libfunc_id)
                    .map_err(CompilationError::ProgramRegistryError)?;
                check_basic_structure(statement_idx, invocation, libfunc)?;

                let param_types: Vec<_> = libfunc
                    .param_signatures()
                    .iter()
                    .map(|param_signature| param_signature.ty.clone())
                    .collect();
                check_types_match(&invoke_refs, &param_types).map_err(|error| {
                    Box::new(AnnotationError::ReferencesError { statement_idx, error }.into())
                })?;
                let compiled_invocation = compile_invocation(
                    ProgramInfo { metadata, type_sizes: &type_sizes },
                    invocation,
                    libfunc,
                    statement_idx,
                    &invoke_refs,
                    annotations.environment,
                )
                .map_err(|error| CompilationError::InvocationError { statement_idx, error })?;

                for instruction in &compiled_invocation.instructions {
                    program_offset += instruction.body.op_size();
                }

                for entry in compiled_invocation.relocations {
                    relocations.push(RelocationEntry {
                        instruction_idx: instructions.len() + entry.instruction_idx,
                        relocation: entry.relocation,
                    });
                }
                instructions.extend(compiled_invocation.instructions);

                let updated_annotations = StatementAnnotations {
                    environment: compiled_invocation.environment,
                    ..annotations
                };

                let branching_libfunc = compiled_invocation.results.len() > 1;

                for (branch_info, branch_changes) in
                    zip_eq(&invocation.branches, compiled_invocation.results)
                {
                    let destination_statement_idx = statement_idx.next(&branch_info.target);
                    if branching_libfunc
                        && !is_branch_align(
                            &registry,
                            &program.statements[destination_statement_idx.0],
                        )?
                    {
                        return Err(Box::new(CompilationError::ExpectedBranchAlign {
                            source_statement_idx: statement_idx,
                            destination_statement_idx,
                        }));
                    }

                    program_annotations
                        .propagate_annotations(
                            statement_idx,
                            destination_statement_idx,
                            &updated_annotations,
                            branch_info,
                            branch_changes,
                            branching_libfunc,
                        )
                        .map_err(|err| Box::new(err.into()))?;
                }
            }
        }
    }

    // Push the final offset at the end of `statement_offsets`.
    statement_offsets.push(program_offset);

    relocate_instructions(&relocations, &statement_offsets, &mut instructions);

    Ok(CairoProgram {
        instructions,
        debug_info: CairoProgramDebugInfo {
            sierra_statement_info: statement_offsets
                .into_iter()
                .map(|code_offset| SierraStatementDebugInfo { code_offset })
                .collect(),
        },
    })
}

/// Returns true if `statement` is an invocation of the branch_align libfunc.
fn is_branch_align(
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    statement: &Statement,
) -> Result<bool, CompilationError> {
    if let Statement::Invocation(invocation) = statement {
        let libfunc = registry
            .get_libfunc(&invocation.libfunc_id)
            .map_err(CompilationError::ProgramRegistryError)?;
        if let [branch_signature] = libfunc.branch_signatures() {
            if branch_signature.ap_change == SierraApChange::BranchAlign {
                return Ok(true);
            }
        }
    }

    Ok(false)
}
