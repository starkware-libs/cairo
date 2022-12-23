use sierra::extensions::nullable::NullableConcreteLibFunc;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression};

/// Builds Casm instructions for Nullable operations.
pub fn build(
    libfunc: &NullableConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        NullableConcreteLibFunc::Null(_) => build_nullable_null(builder),
        NullableConcreteLibFunc::IntoNullable(_) => build_identity(builder),
    }
}

/// Builds Casm instructions for the `null()` libfunc.
fn build_nullable_null(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    builder.try_get_refs::<0>()?;
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression { cells: vec![CellExpression::Immediate(0.into())] }].into_iter(),
    ))
}
