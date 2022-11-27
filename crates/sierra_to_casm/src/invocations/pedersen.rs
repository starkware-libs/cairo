use casm::casm;
use casm::operand::DerefOrImmediate;
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::pedersen::PedersenConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    try_unpack_deref, BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue,
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
    let (pedersen, x, y) = match builder.refs {
        [
            ReferenceValue { expression: expr_pedersen, .. },
            ReferenceValue { expression: expr_x, .. },
            ReferenceValue { expression: expr_y, .. },
        ] => {
            (try_unpack_deref(expr_pedersen)?, try_unpack_deref(expr_x)?, try_unpack_deref(expr_y)?)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };

    let instructions = casm! {
        x = [[pedersen]];
        y = [[pedersen] + 1];
    }
    .instructions;
    let output_expressions = [vec![
        ReferenceExpression {
            cells: vec![CellExpression::BinOp(BinOpExpression {
                op: FeltOperator::Add,
                a: pedersen,
                b: DerefOrImmediate::Immediate(3.into()),
            })],
        },
        ReferenceExpression {
            cells: vec![
                CellExpression::DoubleDeref(pedersen), // TODO: add +2.
            ],
        },
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
