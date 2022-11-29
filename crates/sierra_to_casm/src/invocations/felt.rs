use casm::operand::DerefOrImmediate;
use num_bigint::BigInt;
use sierra::extensions::felt::{
    FeltBinaryOpConcreteLibFunc, FeltBinaryOperationConcreteLibFunc, FeltBinaryOperator,
    FeltConcrete, FeltOperationWithConstConcreteLibFunc, FeltUnaryOpConcreteLibFunc,
    FeltUnaryOperationConcreteLibFunc, FeltUnaryOperator,
};

use super::misc::build_jump_nz;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue, UnaryOpExpression,
};

#[cfg(test)]
#[path = "felt_test.rs"]
mod test;

/// Builds instructions for Sierra felt operations.
pub fn build(
    libfunc: &FeltConcrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        FeltConcrete::UnaryOperation(FeltUnaryOperationConcreteLibFunc::Unary(
            FeltUnaryOpConcreteLibFunc { operator, .. },
        )) => build_felt_unary_op(builder, *operator),
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibFunc::Binary(
            FeltBinaryOpConcreteLibFunc { operator, .. },
        )) => build_felt_op(builder, *operator),
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibFunc::Const(
            FeltOperationWithConstConcreteLibFunc { operator, c, .. },
        )) => build_felt_op_with_const(builder, *operator, c.clone()),
        FeltConcrete::JumpNotZero(_) => build_jump_nz(builder),
        FeltConcrete::Const(libfunc) => Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))]
                .into_iter(),
        )),
    }
}

/// Handles a felt operation with the given unary op.
fn build_felt_unary_op(
    builder: CompiledInvocationBuilder<'_>,
    op: FeltUnaryOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let expr = match builder.refs {
        [ReferenceValue { expression: expr, .. }] => expr,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    let cell = expr
        .try_unpack_single()
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
    let expression = UnaryOpExpression {
        op,
        a: match cell {
            CellExpression::Deref(a) => DerefOrImmediate::Deref(a),
            CellExpression::Immediate(a) => DerefOrImmediate::Immediate(a),
            _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
        },
    };
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::UnaryOp(expression))].into_iter(),
    ))
}

/// Handles a felt operation with the given op.
fn build_felt_op(
    builder: CompiledInvocationBuilder<'_>,
    op: FeltBinaryOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let (expr_a, expr_b) = match builder.refs {
        [ReferenceValue { expression: expr_a, .. }, ReferenceValue { expression: expr_b, .. }] => {
            (expr_a, expr_b)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let cell_a = expr_a
        .try_unpack_single()
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
    let cell_b = expr_b
        .try_unpack_single()
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
    let bin_expression = match (cell_a, cell_b) {
        (CellExpression::Deref(a), CellExpression::Deref(b)) => {
            BinOpExpression { op, a, b: DerefOrImmediate::Deref(b) }
        }
        (CellExpression::Deref(a), CellExpression::Immediate(b)) => {
            BinOpExpression { op, a, b: DerefOrImmediate::Immediate(b) }
        }
        _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
    };
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::BinOp(bin_expression))].into_iter(),
    ))
}

/// Handles a felt operation with a const.
fn build_felt_op_with_const(
    builder: CompiledInvocationBuilder<'_>,
    op: FeltBinaryOperator,
    c: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let expr = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    let cell_expr = expr
        .try_unpack_single()
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
    let ref_expression = if let CellExpression::Deref(a) = cell_expr {
        BinOpExpression { op, a, b: DerefOrImmediate::Immediate(c) }
    } else {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    };
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::BinOp(ref_expression))].into_iter(),
    ))
}
