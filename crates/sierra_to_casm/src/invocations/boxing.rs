use sierra::extensions::boxing::BoxConcreteLibFunc;
use sierra::extensions::ConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibFunc::Into(_) => build_into_box(builder),
        BoxConcreteLibFunc::Unbox(_) => build_unbox(builder),
    }
}

/// Handles instruction for creating a box.
fn build_into_box(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    if builder.program_info.type_sizes.get(&builder.libfunc.param_signatures()[0].ty) != Some(&1) {
        unimplemented!("Box<T> is only supported for types of size 1.");
    }
    let expression = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    if let CellExpression::Deref(operand) = expression
        .try_unpack_single()
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
    {
        Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::IntoSingleCellRef(operand))]
                .into_iter(),
        ))
    } else {
        Err(InvocationError::InvalidReferenceExpressionForArgument)
    }
}

/// Handles instruction for unboxing a box.
fn build_unbox(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let expression = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    if let CellExpression::Deref(operand) = expression
        .try_unpack_single()
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
    {
        Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::DoubleDeref(operand, 0))].into_iter(),
        ))
    } else {
        Err(InvocationError::InvalidReferenceExpressionForArgument)
    }
}
