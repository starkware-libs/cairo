use casm::casm;
use casm::hints::Hint;
use casm::operand::{BinOpOperand, DerefOrImmediate, ResOperand};
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use sierra::extensions::felt::FeltBinaryOperator;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    try_unpack_deref, try_unpack_deref_with_offset, BinOpExpression, CellExpression,
    ReferenceExpression, ReferenceValue,
};

/// Builds instructions for StarkNet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let selector = BigInt::from_bytes_le(num_bigint::Sign::Plus, "storage_read".as_bytes());

    let ((syscall_base, syscall_offset), storage_address) = match builder.refs {
        [
            ReferenceValue { expression: expr_syscall_ptr, .. },
            ReferenceValue { expression: expr_address, .. },
        ] => (try_unpack_deref_with_offset(expr_syscall_ptr)?, try_unpack_deref(expr_address)?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    if syscall_offset > i16::MAX - 2 {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let mut instructions = casm! {
        [ap] = selector, ap++;
        [ap] = [[syscall_base] + syscall_offset];
        storage_address = [[syscall_base] + (syscall_offset + 1)];
    }
    .instructions;

    instructions.last_mut().unwrap().hints = vec![Hint::SystemCall {
        syscall_ptr: ResOperand::BinOp(BinOpOperand {
            op: casm::operand::Operation::Add,
            a: syscall_base,
            b: DerefOrImmediate::Immediate(BigInt::from_i16(syscall_offset).unwrap()),
        }),
    }];

    let output_expressions = [vec![
        ReferenceExpression {
            cells: vec![CellExpression::BinOp(BinOpExpression {
                op: FeltBinaryOperator::Add,
                a: syscall_base,
                b: DerefOrImmediate::Immediate(BigInt::from_i16(syscall_offset).unwrap() + 3),
            })],
        },
        ReferenceExpression {
            cells: vec![CellExpression::DoubleDeref(syscall_base, syscall_offset + 2)],
        },
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
