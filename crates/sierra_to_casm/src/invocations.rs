use std::collections::VecDeque;

use casm::ap_change::ApChange;
use casm::instructions::{AssertEqInstruction, CallInstruction, Instruction, InstructionBody};
use casm::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, ImmediateOperand, Operation, Register, ResOperand,
};
use sierra::extensions::core::felt::{
    FeltBinaryOperationConcreteLibFunc, FeltConcrete, FeltDuplicateConcreteLibFunc,
};
use sierra::extensions::core::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::core::integer::Operator;
use sierra::extensions::core::mem::{MemConcreteLibFunc, StoreTempConcreteLibFunc};
use sierra::extensions::{ConcreteLibFunc, CoreConcreteLibFunc};
use thiserror::Error;

use crate::references::ReferenceValue;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum InvocationError {
    #[error("One of the arguments does not satisfy the requirements of the libfunc.")]
    InvalidReferenceExpressionForArgument,
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

pub fn handle_felt_op(
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
        [ReferenceValue { expression: expr_a }, ReferenceValue { expression: expr_b }] => {
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

    Ok(CompiledInvocation {
        instruction: vec![],
        results: vec![BranchRefChanges {
            refs: vec![ReferenceValue { expression: ResOperand::BinOp(ref_expression) }],
            ap_change: ApChange::Known(0),
        }],
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

pub fn handle_felt_dup(
    _felt_dup: &FeltDuplicateConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    let ref_value = match refs {
        [ref_value] => ref_value,
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    Ok(CompiledInvocation {
        instruction: vec![],
        results: vec![BranchRefChanges {
            refs: vec![ref_value.clone(), ref_value.clone()],
            ap_change: ApChange::Known(0),
        }],
    })
}

pub fn handle_function_call(
    func_call: &FunctionCallConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    check_references_on_stack(refs)?;

    let output_types = func_call.output_types();
    let fallthrough_outputs = &output_types[0];

    let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

    let mut offset = -1;
    for _output_type in fallthrough_outputs.iter().rev() {
        refs.push_front(ReferenceValue {
            expression: ResOperand::Deref(DerefOperand { register: Register::AP, offset }),
        });

        // TODO(ilya, 10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }

    // TODO(ilya, 10/10/2022): Fix call target.
    Ok(CompiledInvocation {
        instruction: vec![Instruction {
            body: InstructionBody::Call(CallInstruction {
                target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                relative: true,
            }),
            inc_ap: false,
        }],
        results: vec![BranchRefChanges { refs: refs.into(), ap_change: ApChange::Known(0) }],
    })
}

pub fn handle_store_temp(
    _store_temp: &StoreTempConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    Ok(CompiledInvocation {
        instruction: vec![Instruction {
            body: InstructionBody::AssertEq(AssertEqInstruction {
                a: DerefOperand { register: Register::AP, offset: 0 },
                b: refs[0].expression.clone(),
            }),
            inc_ap: true,
        }],
        results: vec![BranchRefChanges {
            refs: vec![ReferenceValue {
                expression: ResOperand::Deref(DerefOperand { register: Register::AP, offset: -1 }),
            }],
            ap_change: ApChange::Known(1),
        }],
    })
}

pub fn compile_invocation(
    ext: &CoreConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    match ext {
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
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Rename(_)) => Ok(CompiledInvocation {
            instruction: vec![],
            results: vec![BranchRefChanges { refs: refs.to_vec(), ap_change: ApChange::Known(0) }],
        }),
        CoreConcreteLibFunc::FunctionCall(func_call) => handle_function_call(func_call, refs),
        _ => Err(InvocationError::NotImplemented),
    }
}
