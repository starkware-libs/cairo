use std::fmt::Display;

use cairo_lang_casm::instructions::{Instruction, InstructionBody, RetInstruction};
use cairo_lang_sierra::extensions::const_type::ConstConcreteType;
use cairo_lang_sierra::extensions::core::{CoreConcreteLibfunc, CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::ids::{ConcreteTypeId, VarId};
use cairo_lang_sierra::program::{
    BranchTarget, GenericArg, Invocation, Program, Statement, StatementIdx,
};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_type_size::get_type_size_map;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::zip_eq;
use num_bigint::BigInt;
use thiserror::Error;

use crate::annotations::{AnnotationError, ProgramAnnotations, StatementAnnotations};
use crate::invocations::{
    check_references_on_stack, compile_invocation, InvocationError, ProgramInfo,
};
use crate::metadata::Metadata;
use crate::references::{check_types_match, ReferencesError};
use crate::relocations::{relocate_instructions, CodeOffset, RelocationEntry};

#[cfg(test)]
#[path = "compiler_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum CompilationError {
    #[error("Failed building type information")]
    FailedBuildingTypeInformation,
    #[error("Error from program registry: {0}")]
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
    #[error("Const data does not match the declared const type.")]
    ConstDataMismatch,
    #[error("Unsupported const type.")]
    UnsupportedConstType,
}

/// The casm program representation.
#[derive(Debug, Eq, PartialEq)]
pub struct CairoProgram {
    pub instructions: Vec<Instruction>,
    pub debug_info: CairoProgramDebugInfo,
    pub const_segment_info: ConstSegmentInfo,
}
impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in &self.instructions {
            writeln!(f, "{instruction};")?
        }
        for const_type in self.const_segment_info.const_allocations.keys() {
            writeln!(f, "ret;")?;
            let value = self.const_segment_info.const_types_values.get(const_type).unwrap();
            for value in &value.const_values {
                writeln!(f, "dw {};", value)?;
            }
        }
        Ok(())
    }
}

/// The debug information of a compilation from Sierra to casm.
#[derive(Debug, Eq, PartialEq)]
pub struct SierraStatementDebugInfo {
    /// The offset of the sierra statement within the bytecode.
    pub code_offset: usize,
    /// The index of the sierra statement in the instructions vector.
    pub instruction_idx: usize,
}

/// The debug information of a compilation from Sierra to casm.
#[derive(Debug, Eq, PartialEq)]
pub struct CairoProgramDebugInfo {
    /// The debug information per Sierra statement.
    pub sierra_statement_info: Vec<SierraStatementDebugInfo>,
}

/// The information about the constants used in the program.
#[derive(Debug, Eq, PartialEq, Default)]
pub struct ConstSegmentInfo {
    /// A map between the const type and the offset of the const within the constants segment.
    pub const_allocations: OrderedHashMap<ConcreteTypeId, CodeOffset>,
    /// The size of the constants segment.
    pub const_segment_size: usize,
    /// The type info of the all the declared constants.
    pub const_types_values: OrderedHashMap<ConcreteTypeId, ConstValue>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ConstValue {
    pub const_values: Vec<BigInt>,
}
impl ConstSegmentInfo {
    pub fn new(
        program: &Program,
        registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    ) -> Result<Self, CompilationError> {
        let mut const_types_values: OrderedHashMap<ConcreteTypeId, ConstValue> = Default::default();
        for type_declaration in program.type_declarations.iter() {
            // As the registry is built from the program, we can unwrap here.
            let const_core_type = registry.get_type(&type_declaration.id).unwrap();
            if let cairo_lang_sierra::extensions::core::CoreTypeConcrete::Const(ty) =
                const_core_type
            {
                let const_values = extract_const_value(registry, ty)?;
                const_types_values.insert(type_declaration.id.clone(), ConstValue { const_values });
            }
        }
        Ok(Self {
            const_allocations: Default::default(),
            const_segment_size: 0,
            const_types_values,
        })
    }
    /// Insert a new const type to the const segment.
    pub fn insert(&mut self, const_type: ConcreteTypeId) {
        if self.const_allocations.contains_key(&const_type) {
            return;
        }
        self.const_allocations.insert(const_type.clone(), self.const_segment_size);
        // The size is the number of values, +1 for the prepending ret instruction.
        self.const_segment_size +=
            1 + self.const_types_values.get(&const_type).unwrap().const_values.len();
    }
    /// Returns the offset of the const within the const segment.
    pub fn get(&self, const_type: &ConcreteTypeId) -> Option<CodeOffset> {
        self.const_allocations.get(const_type).copied()
    }
}

/// Gets a const type, and returns a vector of the values to be stored in the const segment.
pub fn extract_const_value(
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    const_type: &ConstConcreteType,
) -> Result<Vec<BigInt>, CompilationError> {
    let const_core_type = registry.get_type(&const_type.inner_ty).unwrap();
    match const_core_type {
        cairo_lang_sierra::extensions::core::CoreTypeConcrete::Felt252(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Uint8(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Uint16(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Uint32(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Uint64(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Uint128(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Sint8(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Sint16(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Sint32(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Sint64(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Sint128(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Bytes31(_)
        | cairo_lang_sierra::extensions::core::CoreTypeConcrete::Felt252Bounded(_) => {
            if let [GenericArg::Value(value)] = &const_type.inner_data[..] {
                Ok(vec![value.clone()])
            } else {
                Err(CompilationError::ConstDataMismatch)
            }
        }
        _ => Err(CompilationError::UnsupportedConstType),
    }
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
    let mut statement_indices = Vec::with_capacity(program.statements.len());

    let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new_with_ap_change(
        program,
        metadata.ap_change_info.function_ap_change.clone(),
    )
    .map_err(CompilationError::ProgramRegistryError)?;
    let mut const_segment_info = ConstSegmentInfo::new(program, &registry)?;
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
        statement_indices.push(instructions.len());
        statement_offsets.push(program_offset);
        match statement {
            Statement::Return(ref_ids) => {
                let (annotations, return_refs) = program_annotations
                    .get_annotations_after_take_args(statement_idx, ref_ids.iter())
                    .map_err(|err| Box::new(err.into()))?;
                return_refs.iter().for_each(|r| r.validate(&type_sizes));

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
                invoke_refs.iter().for_each(|r| r.validate(&type_sizes));
                let compiled_invocation = compile_invocation(
                    ProgramInfo { metadata, type_sizes: &type_sizes },
                    invocation,
                    libfunc,
                    statement_idx,
                    &invoke_refs,
                    annotations.environment,
                    &mut const_segment_info,
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
    // Push the final offset and index at the end of the vectors.
    statement_indices.push(instructions.len());
    statement_offsets.push(program_offset);

    relocate_instructions(&relocations, &statement_offsets, &const_segment_info, &mut instructions);

    Ok(CairoProgram {
        instructions,
        const_segment_info,
        debug_info: CairoProgramDebugInfo {
            sierra_statement_info: zip_eq(statement_offsets, statement_indices)
                .map(|(code_offset, instruction_idx)| SierraStatementDebugInfo {
                    code_offset,
                    instruction_idx,
                })
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
