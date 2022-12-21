use sierra::extensions::nullable::NullableConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression};

/// Builds Casm instructions for Nullable operations.
pub fn build(
    libfunc: &NullableConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        NullableConcreteLibFunc::Null(_) => build_nullable_null(builder),
    }
}

/// Builds Casm instructions for the `null()` libfunc.
fn build_nullable_null(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    if !builder.refs.is_empty() {
        return Err(InvocationError::WrongNumberOfArguments {
            expected: 0,
            actual: builder.refs.len(),
        });
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression { cells: vec![CellExpression::Immediate(0.into())] }].into_iter(),
    ))
}
