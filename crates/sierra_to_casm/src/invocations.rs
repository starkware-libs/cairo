use casm::ap_change::ApChange;
use casm::instructions::{AssertEqInstruction, Instruction, InstructionBody};
use casm::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, Operation, Register, ResOperand,
};
use sierra::extensions::core::felt::{FeltBinaryOperationConcreteLibFunc, FeltConcrete};
use sierra::extensions::core::integer::Operator;
use sierra::extensions::core::mem::MemConcreteLibFunc;
use sierra::extensions::CoreConcreteLibFunc;
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

pub fn compile_invocation(
    ext: &CoreConcreteLibFunc,
    refs: &[ReferenceValue],
) -> Result<CompiledInvocation, InvocationError> {
    match ext {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcreteLibFunc::Felt(FeltConcrete::Operation(felt_op)) => {
            handle_felt_op(felt_op, refs)
        }

        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::StoreTemp(_)) => Ok(CompiledInvocation {
            instruction: vec![Instruction {
                body: InstructionBody::AssertEq(AssertEqInstruction {
                    a: DerefOperand { register: Register::AP, offset: 0 },
                    b: refs[0].expression.clone(),
                }),
                inc_ap: true,
            }],
            results: vec![BranchRefChanges {
                refs: vec![ReferenceValue {
                    expression: ResOperand::Deref(DerefOperand {
                        register: Register::AP,
                        offset: -1,
                    }),
                }],
                ap_change: ApChange::Known(1),
            }],
        }),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Move(_))
        | CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Rename(_)) => Ok(CompiledInvocation {
            instruction: vec![],
            results: vec![BranchRefChanges { refs: refs.to_vec(), ap_change: ApChange::Known(0) }],
        }),
        _ => Err(InvocationError::NotImplemented),
    }
}
