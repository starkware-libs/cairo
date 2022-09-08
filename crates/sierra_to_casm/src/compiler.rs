use std::collections::HashMap;
use std::fmt::Display;

use casm::ap_change::ApplyApChange;
use casm::instructions::{Instruction, InstructionBody, RetInstruction};
use itertools::zip_eq;
use sierra::edit_state::{put_results, take_args, EditStateError};
use sierra::extensions::core::{CoreConcreteLibFunc, CoreLibFunc, CoreType};
use sierra::extensions::ConcreteLibFunc;
use sierra::program::{BranchInfo, BranchTarget, Invocation, Program, Statement, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

use crate::annotations::{
    AnnotationError, ProgramAnnotations, ReturnTypesAnnotation, StatementAnnotations,
};
use crate::invocations::{
    check_references_on_stack, compile_invocation, BranchRefChanges, InvocationError,
};
use crate::references::{check_types_match, ReferenceValue, ReferencesError, StatementRefs};
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
    ProgramRegistryError(ProgramRegistryError),
    #[error(transparent)]
    AnnotationError(#[from] AnnotationError),
    #[error(transparent)]
    InvocationError(#[from] InvocationError),
    #[error(transparent)]
    ReferencesError(#[from] ReferencesError),
    #[error("Invocation mismatched to libfunc")]
    LibFuncInvocationMismatch,
    #[error(transparent)]
    EditStateError(#[from] EditStateError),
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

/// Ensure the basic structure of the invocation is the same as the library function.
fn check_basic_structure(
    invocation: &Invocation,
    libfunc: &CoreConcreteLibFunc,
) -> Result<(), CompilationError> {
    if invocation.args.len() != libfunc.input_types().len()
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
        Err(CompilationError::LibFuncInvocationMismatch)
    } else {
        Ok(())
    }
}

// Propagate the annotations from the statement at 'statement_idx' to all the branches
// from said statement.
// 'annotations' is the result of calling get_annotations_after_take_args at
// 'statement_idx' and 'per_branch_ref_changes' are the reference changes at each branch.
pub fn propagate_annotations(
    statement_idx: StatementIdx,
    live_reference: StatementRefs,
    return_types: ReturnTypesAnnotation,
    branches: &[BranchInfo],
    per_branch_ref_changes: impl Iterator<Item = BranchRefChanges>,
    program_annotations: &mut ProgramAnnotations,
) -> Result<(), CompilationError> {
    for (branch_info, branch_result) in zip_eq(branches, per_branch_ref_changes) {
        let mut new_refs: StatementRefs =
            HashMap::with_capacity(live_reference.len() + branch_result.refs.len());
        for (var_id, ref_value) in &live_reference {
            new_refs.insert(
                var_id.clone(),
                ReferenceValue {
                    expression: ref_value
                        .expression
                        .clone()
                        .apply_ap_change(branch_result.ap_change)
                        .map_err(ReferencesError::ApChangeError)?,
                    ty: ref_value.ty.clone(),
                },
            );
        }

        program_annotations.set_or_assert(
            statement_idx.next(&branch_info.target),
            StatementAnnotations {
                refs: put_results(new_refs, zip_eq(&branch_info.results, branch_result.refs))?,
                return_types: return_types.clone(),
            },
        )?;
    }
    Ok(())
}

pub fn compile(program: &Program) -> Result<CairoProgram, CompilationError> {
    let mut instructions = Vec::new();
    let mut relocations: Vec<RelocationEntry> = Vec::new();

    // Maps statement_idx to program_offset.
    let mut statement_offsets = Vec::with_capacity(program.statements.len());

    let registry = ProgramRegistry::<CoreType, CoreLibFunc>::new(program)
        .map_err(CompilationError::ProgramRegistryError)?;
    let type_sizes = get_type_size_map(program, &registry)
        .ok_or(CompilationError::FailedBuildingTypeInformation)?;
    let mut program_annotations =
        ProgramAnnotations::create(program.statements.len(), &program.funcs)?;

    let mut program_offset: usize = 0;

    for (statement_id, statement) in program.statements.iter().enumerate() {
        let statement_idx = StatementIdx(statement_id);

        statement_offsets.push(program_offset);

        match statement {
            Statement::Return(ref_ids) => {
                let annotations = program_annotations.get_annotations(statement_idx)?;
                let (live_references, return_refs) =
                    take_args(annotations.refs.clone(), ref_ids.iter())?;

                if !live_references.is_empty() {
                    return Err(ReferencesError::DanglingReferences(statement_idx).into());
                }
                program_annotations
                    .validate_return_type(&return_refs, annotations.return_types.clone())?;
                check_references_on_stack(&type_sizes, &return_refs)?;

                let ret_instruction = RetInstruction {};
                program_offset += ret_instruction.op_size();
                instructions.push(Instruction {
                    body: InstructionBody::Ret(ret_instruction),
                    inc_ap: false,
                });
            }
            Statement::Invocation(invocation) => {
                let annotations = program_annotations.get_annotations(statement_idx)?;
                let (live_references, invoke_refs) =
                    take_args(annotations.refs.clone(), invocation.args.iter())?;

                let libfunc = registry
                    .get_libfunc(&invocation.libfunc_id)
                    .map_err(CompilationError::ProgramRegistryError)?;
                check_basic_structure(invocation, libfunc)?;

                check_types_match(&invoke_refs, libfunc.input_types())?;
                let compiled_invocation =
                    compile_invocation(invocation, libfunc, &invoke_refs, &type_sizes)?;

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

                propagate_annotations(
                    statement_idx,
                    live_references,
                    annotations.return_types.clone(),
                    &invocation.branches,
                    compiled_invocation.results.into_iter(),
                    &mut program_annotations,
                )?;
            }
        }
    }

    relocate_instructions(&relocations, statement_offsets, &mut instructions);

    Ok(CairoProgram { instructions })
}
