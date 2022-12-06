use casm::operand::DerefOrImmediate;
use sierra::extensions::boolean::BoolConcreteLibFunc;
use sierra::extensions::felt::FeltBinaryOperator;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    try_unpack_deref, BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue,
};

/// Builds instructions for Sierra bool operations.
pub fn build(
    libfunc: &BoolConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoolConcreteLibFunc::And(_) => build_bool_and(builder),
    }
}

/// Handles instructions for boolean AND.
fn build_bool_and(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (a, b) = match builder.refs {
        [ReferenceValue { expression: expr_a, .. }, ReferenceValue { expression: expr_b, .. }] => {
            (try_unpack_deref(expr_a)?, try_unpack_deref(expr_b)?)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    Ok(builder.build(
        vec![],
        vec![],
        [[ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
            op: FeltBinaryOperator::Mul,
            a,
            b: DerefOrImmediate::from(b),
        }))]
        .into_iter()]
        .into_iter(),
    ))
}
