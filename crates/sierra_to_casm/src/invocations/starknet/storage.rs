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

    let ((system_base, system_offset), storage_address) = match builder.refs {
        [
            ReferenceValue { expression: expr_system, .. },
            ReferenceValue { expression: expr_address, .. },
        ] => (try_unpack_deref_with_offset(expr_system)?, try_unpack_deref(expr_address)?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    if system_offset > i16::MAX - 2 {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let instructions = casm! {
        [ap] = selector, ap++;
        [ap] = [[system_base] + system_offset];
        storage_address = [[system_base] + (system_offset + 1)];
        %{ syscall_handler.syscall(segments=segments, syscall_ptr=[system_base] + system_offset) %}
        // TODO(yg): replace with???
        ap += 0;
    }
    .instructions;

    let output_expressions = [vec![
        // System
        ReferenceExpression {
            cells: vec![CellExpression::BinOp(BinOpExpression {
                op: FeltBinaryOperator::Add,
                a: system_base,
                b: DerefOrImmediate::Immediate(BigInt::from_i16(system_offset).unwrap() + 3),
            })],
        },
        // Read value
        ReferenceExpression {
            cells: vec![CellExpression::DoubleDeref(system_base, system_offset + 2)],
        },
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
