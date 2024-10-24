use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc::build_is_zero;
use crate::references::ReferenceExpression;

/// Builds Casm instructions for Nullable operations.
pub fn build(
    libfunc: &NullableConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        NullableConcreteLibfunc::Null(_) => build_nullable_null(builder),
        NullableConcreteLibfunc::NullableFromBox(_) => {
            // Note that the pointer of the Box is never zero:
            // 1. If the size of the inner type is nonnegative, then values are written to the
            //    memory address pointed by the pointer. It follows that this address cannot be
            //    zero, since the Cairo-AIR guarantees that all memory accesses have address >= 1.
            // 2. If the size of the inner type is zero, then the pointer is set to 1. see
            //    `build_into_box`.
            build_identity(builder)
        }
        NullableConcreteLibfunc::MatchNullable(_) => build_nullable_match_nullable(builder),
        NullableConcreteLibfunc::ForwardSnapshot(_) => build_identity(builder),
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

/// Builds Casm instructions for the `null()` libfunc.
fn build_nullable_match_nullable(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    builder.refs[0]
        .expression
        .try_unpack_single()?
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    build_is_zero(builder)
}
