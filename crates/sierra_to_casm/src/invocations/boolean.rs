use casm::casm;
use casm::operand::{ap_cell_ref, DerefOrImmediate};
use sierra::extensions::boolean::BoolConcreteLibFunc;
use sierra::extensions::felt::FeltBinaryOperator;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};

/// Builds instructions for Sierra bool operations.
pub fn build(
    libfunc: &BoolConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoolConcreteLibFunc::And(_) => build_bool_and(builder),
        BoolConcreteLibFunc::Not(_) => build_bool_not(builder),
    }
}

/// Handles instructions for boolean AND.
fn build_bool_and(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (a, b) = match builder.refs {
        [ReferenceValue { expression: expr_a, .. }, ReferenceValue { expression: expr_b, .. }] => {
            (expr_a.try_unpack_single()?.to_deref()?, expr_b.try_unpack_single()?.to_deref()?)
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

/// Handles instructions for boolean NOT.
fn build_bool_not(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let a = match builder.refs {
        [ReferenceValue { expression, .. }] => expression.try_unpack_single()?.to_deref()?,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };

    // We want to output `1 - a`, but a SUB expression cannot have an immediate value on the LHS.
    // Store 1 in AP first, advance AP and return `[ap - 1] - a`.
    Ok(builder.build(
        casm! { [ap + 0] = 1, ap++; }.instructions,
        vec![],
        [[ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
            op: FeltBinaryOperator::Sub,
            a: ap_cell_ref(-1),
            b: DerefOrImmediate::from(a),
        }))]
        .into_iter()]
        .into_iter(),
    ))
}
