use std::collections::VecDeque;

use casm::ap_change::ApChange;
use casm::instructions::{AssertEqInstruction, CallInstruction, Instruction, InstructionBody};
use casm::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, ImmediateOperand, Operation, Register, ResOperand,
};
use itertools::{equal, zip_eq};
use sierra::extensions::core::felt::{
    FeltBinaryOperationConcreteLibFunc, FeltConcrete, FeltDuplicateConcreteLibFunc,
};
use sierra::extensions::core::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::core::integer::Operator;
use sierra::extensions::core::mem::{MemConcreteLibFunc, StoreTempConcreteLibFunc};
use sierra::extensions::{ConcreteLibFunc, CoreConcreteLibFunc};
use sierra::ids::ConcreteTypeId;
use thiserror::Error;

use crate::references::ReferenceValue;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum InvocationError {
    #[error("One of the arguments does not satisfy the requirements of the libfunc.")]
    InvalidReferenceExpressionForArgument,
    #[error("One of the arguments does not match the expected input of the libfunc.")]
    InvalidReferenceTypeForArgument,
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

/// The result from a compilation of a single invocation statement.
#[derive(Debug, Eq, PartialEq)]
pub struct CompiledInvocation {
    // A vector instructions that implement the Invocation.
    pub instruction: Vec<Instruction>,
    // A vector of BranchRefChanges, should correspond to Invocation.branches.
    pub results: Vec<BranchRefChanges>,
}

/// Module internal CompiledInvocation without the type information.
struct TypeLessCompiledInvocation {
    pub instruction: Vec<Instruction>,
    pub results: Vec<(Vec<ResOperand>, ApChange)>,
}

fn handle_felt_op(
    felt_op: &FeltBinaryOperationConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<TypeLessCompiledInvocation, InvocationError> {
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
    Ok(TypeLessCompiledInvocation {
        instruction: vec![],
        results: vec![(vec![ResOperand::BinOp(ref_expression)], ApChange::Known(0))],
    })
}

// Checks that the list of reference is contiguous on the stack and ends at ap - 1.
// This is the requirement for function call and return statements.
pub fn check_references_on_stack(refs: &[ReferenceValue]) -> Result<(), InvocationError> {
    let mut expected_offset: i16 = -1;
    for return_ref in refs.iter().rev() {
        match return_ref.expression {
            ResOperand::Deref(DerefOperand { register: Register::AP, offset })
                if offset == expected_offset =>
            {
                // TODO(ilya, 10/10/2022): Get size from type.
                expected_offset -= 1
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
    if equal(types.iter(), refs.iter().map(|r| &r.ty)) {
        Ok(())
    } else {
        Err(InvocationError::InvalidReferenceTypeForArgument)
    }
}

fn handle_felt_dup(
    _felt_dup: &FeltDuplicateConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<TypeLessCompiledInvocation, InvocationError> {
    let expression = match refs {
        [ReferenceValue { expression, .. }] => expression,
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    Ok(TypeLessCompiledInvocation {
        instruction: vec![],
        results: vec![(vec![expression.clone(), expression.clone()], ApChange::Known(0))],
    })
}

fn handle_function_call(
    func_call: &FunctionCallConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<TypeLessCompiledInvocation, InvocationError> {
    check_references_on_stack(refs)?;

    let output_types = func_call.output_types();
    let fallthrough_outputs = &output_types[0];

    let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

    let mut offset = -1;
    for _output_type in fallthrough_outputs.iter().rev() {
        refs.push_front(ResOperand::Deref(DerefOperand { register: Register::AP, offset }));

        // TODO(ilya, 10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }
    Ok(TypeLessCompiledInvocation {
        instruction: vec![Instruction {
            body: InstructionBody::Call(CallInstruction {
                target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                relative: true,
            }),
            inc_ap: false,
        }],
        results: vec![(refs.into(), ApChange::Known(0))],
    })
}

fn handle_store_temp(
    _store_temp: &StoreTempConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<TypeLessCompiledInvocation, InvocationError> {
    Ok(TypeLessCompiledInvocation {
        instruction: vec![Instruction {
            body: InstructionBody::AssertEq(AssertEqInstruction {
                a: DerefOperand { register: Register::AP, offset: 0 },
                b: refs[0].expression.clone(),
            }),
            inc_ap: true,
        }],
        results: vec![(
            vec![ResOperand::Deref(DerefOperand { register: Register::AP, offset: -1 })],
            ApChange::Known(1),
        )],
    })
}

pub fn compile_invocation(
    libfunc: &CoreConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    check_types_match(refs, &libfunc.input_types())?;
    let typeless = match libfunc {
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
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Rename(_)) => Ok(TypeLessCompiledInvocation {
            instruction: vec![],
            results: vec![(
                refs.iter().map(|r| r.expression.clone()).collect(),
                ApChange::Known(0),
            )],
        }),
        CoreConcreteLibFunc::FunctionCall(func_call) => handle_function_call(func_call, refs),
        _ => Err(InvocationError::NotImplemented),
    }?;
    Ok(CompiledInvocation {
        instruction: typeless.instruction,
        results: zip_eq(typeless.results.into_iter(), libfunc.output_types())
            .map(|((expressions, ap_change), branch_output_types)| BranchRefChanges {
                refs: zip_eq(expressions, branch_output_types)
                    .map(|(expression, ty)| ReferenceValue { expression, ty })
                    .collect(),
                ap_change,
            })
            .collect(),
    })
}
