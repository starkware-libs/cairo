use std::collections::VecDeque;

use casm::ap_change::ApChange;
use casm::instructions::{AssertEqInstruction, CallInstruction, Instruction, InstructionBody};
use casm::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, ImmediateOperand, Operation, Register, ResOperand,
};
use itertools::zip_eq;
use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::extensions::felt::{FeltBinaryOperationConcreteLibFunc, FeltConcrete};
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::integer::Operator;
use sierra::extensions::lib_func::SignatureOnlyConcreteLibFunc;
use sierra::extensions::mem::{MemConcreteLibFunc, StoreTempConcreteLibFunc};
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use thiserror::Error;

use crate::references::ReferenceValue;
use crate::relocations::{Relocation, RelocationEntry};
use crate::type_sizes::TypeSizeMap;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum InvocationError {
    #[error("One of the arguments does not satisfy the requirements of the libfunc.")]
    InvalidReferenceExpressionForArgument,
    #[error("One of the arguments does not match the expected input of the libfunc.")]
    InvalidReferenceTypeForArgument,
    #[error("Unexpected error - an unregistered type id used.")]
    UnknownTypeId(ConcreteTypeId),
    #[error("Expected a different number of arguments")]
    WrongNumberOfArguments,
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
}

/// Describes the changes to the set of references at the branch target.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchRefChanges {
    // New references defined at a given branch.
    // should correspond to BranchInfo.results.
    pub refs: Vec<ReferenceValue>,
    pub ap_change: ApChange,
}
impl BranchRefChanges {
    fn new(
        ap_change: ApChange,
        expressions: impl Iterator<Item = ResOperand>,
        types: impl Iterator<Item = ConcreteTypeId>,
    ) -> Self {
        Self {
            refs: zip_eq(expressions, types)
                .map(|(expression, ty)| ReferenceValue { expression, ty })
                .collect(),
            ap_change,
        }
    }
}

/// The result from a compilation of a single invocation statement.
#[derive(Debug, Eq, PartialEq)]
pub struct CompiledInvocation {
    // A vector instructions that implement the Invocation.
    pub instructions: Vec<Instruction>,
    // A vector of static relocation.
    pub relocations: Vec<RelocationEntry>,
    // A vector of BranchRefChanges, should correspond to Invocation.branches.
    pub results: Vec<BranchRefChanges>,
}
impl CompiledInvocation {
    fn new(
        instructions: Vec<Instruction>,
        relocations: Vec<RelocationEntry>,
        ap_changes: impl Iterator<Item = ApChange>,
        output_expressions: impl Iterator<Item = impl Iterator<Item = ResOperand>>,
        output_types: &[Vec<ConcreteTypeId>],
    ) -> Self {
        Self {
            instructions,
            relocations,
            results: zip_eq(ap_changes, zip_eq(output_expressions, output_types))
                .map(|(ap_change, (expressions, types))| {
                    BranchRefChanges::new(ap_change, expressions, types.iter().cloned())
                })
                .collect(),
        }
    }

    // A constructor for the trivial case of fallthrough without any instructions.
    fn only_reference_changes(
        output_expressions: impl Iterator<Item = ResOperand>,
        output_types: &[Vec<ConcreteTypeId>],
    ) -> Self {
        Self::new(
            vec![],
            vec![],
            [ApChange::Known(0)].into_iter(),
            [output_expressions].into_iter(),
            output_types,
        )
    }
}

fn handle_felt_op(
    felt_op: &FeltBinaryOperationConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    let op = match felt_op.operator {
        Operator::Add => Operation::Add,
        Operator::Mul => Operation::Mul,

        // TODO(ilya, 12/12/2022): Support div and sub.
        _ => return Err(InvocationError::NotImplemented),
    };

    let (expr_a, expr_b) = match refs {
        [ReferenceValue { expression: expr_a, .. }, ReferenceValue { expression: expr_b, .. }] => {
            (expr_a, expr_b)
        }
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    let ref_expression = match (expr_a, expr_b) {
        (ResOperand::Deref(a), ResOperand::Deref(b)) => {
            BinOpOperand { op, a: *a, b: DerefOrImmediate::Deref(*b) }
        }
        (ResOperand::Deref(a), ResOperand::Immediate(b)) => {
            BinOpOperand { op, a: *a, b: DerefOrImmediate::Immediate(*b) }
        }
        _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
    };
    Ok(CompiledInvocation::only_reference_changes(
        [ResOperand::BinOp(ref_expression)].into_iter(),
        felt_op.output_types(),
    ))
}

// Checks that the list of reference is contiguous on the stack and ends at ap - 1.
// This is the requirement for function call and return statements.
pub fn check_references_on_stack(
    type_sizes: &TypeSizeMap,
    refs: &[ReferenceValue],
) -> Result<(), InvocationError> {
    let mut expected_offset: i16 = -1;
    for return_ref in refs.iter().rev() {
        match return_ref.expression {
            ResOperand::Deref(DerefOperand { register: Register::AP, offset })
                if offset == expected_offset =>
            {
                expected_offset -= type_sizes
                    .get(&return_ref.ty)
                    .ok_or_else(|| InvocationError::UnknownTypeId(return_ref.ty.clone()))?;
            }
            _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
        }
    }
    Ok(())
}

/// Checks that the list of reference contains types matching the given types.
pub fn check_types_match(
    refs: &[ReferenceValue],
    types: &[ConcreteTypeId],
) -> Result<(), InvocationError> {
    if itertools::equal(types.iter(), refs.iter().map(|r| &r.ty)) {
        Ok(())
    } else {
        Err(InvocationError::InvalidReferenceTypeForArgument)
    }
}

fn handle_felt_dup(
    felt_dup: &SignatureOnlyConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    let expression = match refs {
        [ReferenceValue { expression, .. }] => expression,
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    Ok(CompiledInvocation::only_reference_changes(
        [expression.clone(), expression.clone()].into_iter(),
        felt_dup.output_types(),
    ))
}

fn handle_function_call(
    type_sizes: &TypeSizeMap,
    func_call: &FunctionCallConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    check_references_on_stack(type_sizes, refs)?;

    let output_types = func_call.output_types();
    let fallthrough_outputs = &output_types[0];

    let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

    let mut offset = -1;
    for output_type in fallthrough_outputs.iter().rev() {
        refs.push_front(ResOperand::Deref(DerefOperand { register: Register::AP, offset }));

        offset -= type_sizes
            .get(output_type)
            .ok_or_else(|| InvocationError::UnknownTypeId(output_type.clone()))?;
    }

    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::Call(CallInstruction {
                target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                relative: true,
            }),
            inc_ap: false,
        }],
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementID(func_call.function.entry),
        }],
        [ApChange::Known(0)].into_iter(),
        [refs.into_iter()].into_iter(),
        func_call.output_types(),
    ))
}

fn handle_store_temp(
    store_temp: &StoreTempConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::AssertEq(AssertEqInstruction {
                a: DerefOperand { register: Register::AP, offset: 0 },
                b: refs[0].expression.clone(),
            }),
            inc_ap: true,
        }],
        vec![],
        [ApChange::Known(1)].into_iter(),
        [[ResOperand::Deref(DerefOperand { register: Register::AP, offset: -1 })].into_iter()]
            .into_iter(),
        store_temp.output_types(),
    ))
}

pub fn compile_invocation(
    type_sizes: &TypeSizeMap,
    libfunc: &CoreConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    check_types_match(refs, libfunc.input_types())?;
    match libfunc {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcreteLibFunc::Felt(FeltConcrete::Operation(felt_op)) => {
            handle_felt_op(felt_op, refs)
        }
        CoreConcreteLibFunc::Felt(FeltConcrete::Duplicate(felt_dup)) => {
            handle_felt_dup(felt_dup, refs)
        }
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::StoreTemp(store_temp)) => {
            handle_store_temp(store_temp, refs)
        }
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Rename(libfunc)) => {
            Ok(CompiledInvocation::only_reference_changes(
                refs.iter().map(|r| r.expression.clone()),
                libfunc.output_types(),
            ))
        }
        CoreConcreteLibFunc::FunctionCall(func_call) => {
            handle_function_call(type_sizes, func_call, refs)
        }
        _ => Err(InvocationError::NotImplemented),
    }
}
