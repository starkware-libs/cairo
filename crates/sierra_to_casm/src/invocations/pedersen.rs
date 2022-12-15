#[cfg(test)]
#[path = "pedersen_test.rs"]
mod test;

use casm::casm;
use casm::operand::DerefOrImmediate;
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::extensions::pedersen::PedersenConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    try_unpack_deref, try_unpack_deref_with_offset, BinOpExpression, CellExpression,
    ReferenceExpression, ReferenceValue,
};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &PedersenConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        PedersenConcreteLibFunc::Hash(_) => build_pedersen_hash(builder),
    }
}

/// Handles instruction for appending an element to an array.
fn build_pedersen_hash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let ((pedersen_base, pedersen_offset), x, y) = match builder.refs {
        [
            ReferenceValue { expression: expr_pedersen, .. },
            ReferenceValue { expression: expr_x, .. },
            ReferenceValue { expression: expr_y, .. },
        ] => (
            try_unpack_deref_with_offset(expr_pedersen)?,
            try_unpack_deref(expr_x)?,
            try_unpack_deref(expr_y)?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };

    if pedersen_offset > i16::MAX - 2 {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let instructions = casm! {
        x = [[&pedersen_base] + pedersen_offset];
        y = [[&pedersen_base] + (pedersen_offset + 1)];
    }
    .instructions;
    let output_expressions = [vec![
        ReferenceExpression {
            cells: vec![CellExpression::BinOp(BinOpExpression {
                op: FeltBinaryOperator::Add,
                a: pedersen_base,
                b: DerefOrImmediate::Immediate(BigInt::from_i16(pedersen_offset).unwrap() + 3),
            })],
        },
        ReferenceExpression {
            cells: vec![CellExpression::DoubleDeref(pedersen_base, pedersen_offset + 2)],
        },
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
