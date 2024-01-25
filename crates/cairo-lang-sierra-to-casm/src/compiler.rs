use std::fmt::Display;

use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::instructions::{Instruction, InstructionBody, RetInstruction};
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
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{chain, zip_eq};
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CairoProgram {
    pub instructions: Vec<Instruction>,
    pub debug_info: CairoProgramDebugInfo,
    pub const_segment_info: ConstSegmentInfo,
}
impl Display for CairoProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if std::env::var("PRINT_CASM_BYTECODE_OFFSETS").is_ok() {
            let mut bytecode_offset = 0;
            for instruction in &self.instructions {
                writeln!(f, "{instruction}; // {bytecode_offset}")?;
                bytecode_offset += instruction.body.op_size();
            }
            for const_allocation in self.const_segment_info.const_allocations.values() {
                writeln!(f, "ret; // {bytecode_offset}")?;
                bytecode_offset += 1;
                for value in &const_allocation.values {
                    writeln!(f, "dw {value}; // {bytecode_offset}")?;
                }
                bytecode_offset += 1;
            }
        } else {
            for instruction in &self.instructions {
                writeln!(f, "{instruction};")?;
            }
            for const_allocation in self.const_segment_info.const_allocations.values() {
                writeln!(f, "ret;")?;
                for value in &const_allocation.values {
                    writeln!(f, "dw {value};")?;
                }
            }
        }
        Ok(())
    }
}

impl CairoProgram {
    /// Creates an assembled representation of the program.
    pub fn assemble(&self) -> AssembledCairoProgram {
        self.assemble_ex(&[], &[])
    }

    /// Creates an assembled representation of the program preceded by `header` and followed by
    /// `footer`.
    pub fn assemble_ex(
        &self,
        header: &[Instruction],
        footer: &[Instruction],
    ) -> AssembledCairoProgram {
        let mut bytecode = vec![];
        let mut hints = vec![];
        for instruction in chain!(header, &self.instructions) {
            if !instruction.hints.is_empty() {
                hints.push((bytecode.len(), instruction.hints.clone()))
            }
            bytecode.extend(instruction.assemble().encode().into_iter())
        }
        let [ref ret_bytecode] = Instruction::new(InstructionBody::Ret(RetInstruction {}), false)
            .assemble()
            .encode()[..]
        else {
            panic!("Ret instruction is a single word")
        };
        for const_allocation in self.const_segment_info.const_allocations.values() {
            bytecode.push(ret_bytecode.clone());
            bytecode.extend(const_allocation.values.clone());
        }
        for instruction in footer {
            assert!(
                instruction.hints.is_empty(),
                "All footer instructions must have no hints since these cannot be added to the \
                 hints dict."
            );
            bytecode.extend(instruction.assemble().encode().into_iter())
        }
        AssembledCairoProgram { bytecode, hints }
    }
}

/// The debug information of a compilation from Sierra to casm.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct SierraStatementDebugInfo {
    /// The offset of the sierra statement within the bytecode.
    pub code_offset: usize,
    /// The index of the sierra statement in the instructions vector.
    pub instruction_idx: usize,
}

/// The debug information of a compilation from Sierra to casm.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CairoProgramDebugInfo {
    /// The debug information per Sierra statement.
    pub sierra_statement_info: Vec<SierraStatementDebugInfo>,
}

/// A builder for the const segment information.
#[derive(Debug, Default)]
pub struct ConstSegmentInfoBuilder {
    used_const_types: OrderedHashSet<ConcreteTypeId>,
}

impl ConstSegmentInfoBuilder {
    /// Inserts a const type into the builder, to be added to the const segment.
    pub fn insert(&mut self, const_type: &ConcreteTypeId) {
        self.used_const_types.insert(const_type.clone());
    }
    /// Builds the const segment information.
    pub fn build(
        self,
        registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    ) -> Result<ConstSegmentInfo, CompilationError> {
        let mut const_allocations = OrderedHashMap::default();
        let mut const_segment_size = 0;
        for const_type in self.used_const_types {
            let const_allocation = ConstAllocation {
                offset: const_segment_size,
                values: extract_const_value(registry, &const_type).unwrap(),
            };
            const_segment_size += const_allocation.values.len() + 1;
            const_allocations.insert(const_type.clone(), const_allocation);
        }
        Ok(ConstSegmentInfo { const_allocations, const_segment_size })
    }
}

/// The data of a single const in the const segment.
#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct ConstAllocation {
    /// The offset of the const within the constants segment.
    pub offset: CodeOffset,
    /// The values to be stored in the const segment.
    pub values: Vec<BigInt>,
}

/// The information about the constants used in the program.
#[derive(Debug, Eq, PartialEq, Default, Clone)]
pub struct ConstSegmentInfo {
    /// A map between the const type and the data of the const.
    pub const_allocations: OrderedHashMap<ConcreteTypeId, ConstAllocation>,
    /// The size of the constants segment.
    pub const_segment_size: usize,
}

/// Gets a concrete type, if it is a const type returns a vector of the values to be stored in the
/// const segment.
fn extract_const_value(
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
    ty: &ConcreteTypeId,
) -> Result<Vec<BigInt>, CompilationError> {
    let mut values = Vec::new();
    let mut types_stack = vec![ty.clone()];
    while let Some(ty) = types_stack.pop() {
        let const_type =
            if let cairo_lang_sierra::extensions::core::CoreTypeConcrete::Const(const_type) =
                registry.get_type(&ty).unwrap()
            {
                const_type
            } else {
                return Err(CompilationError::UnsupportedConstType);
            };
        let inner_type = registry.get_type(&const_type.inner_ty).unwrap();
        match inner_type {
            cairo_lang_sierra::extensions::core::CoreTypeConcrete::Struct(_) => {
                // Add the struct members types to the stack.
                for arg in const_type.inner_data.iter().rev() {
                    match arg {
                        GenericArg::Type(arg_ty) => types_stack.push(arg_ty.clone()),
                        _ => return Err(CompilationError::ConstDataMismatch),
                    }
                }
            }
            cairo_lang_sierra::extensions::core::CoreTypeConcrete::Enum(_) => {
                // The first argument is the variant selector, the second is the variant data.
                match const_type.inner_data.as_slice() {
                    [GenericArg::Value(selector), GenericArg::Type(ty)] => {
                        values.push(selector.clone());
                        types_stack.push(ty.clone());
                    }
                    _ => return Err(CompilationError::ConstDataMismatch),
                }
            }
            _ => values.extend(
                const_type
                    .inner_data
                    .iter()
                    .map(|arg| match arg {
                        GenericArg::Value(value) => Ok(value.clone()),
                        _ => Err(CompilationError::ConstDataMismatch),
                    })
                    .collect::<Result<Vec<BigInt>, CompilationError>>()?,
            ),
        };
    }
    Ok(values)
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
    let mut const_segment_info_builder = ConstSegmentInfoBuilder::default();
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
                    &mut const_segment_info_builder,
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
    let const_segment_info = const_segment_info_builder.build(&registry)?;
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
