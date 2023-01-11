use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::lib_func::SignatureAndTypeConcreteLibfunc;
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc::build_jump_nz;
use crate::references::ReferenceExpression;

/// Builds Casm instructions for Nullable operations.
pub fn build(
    libfunc: &NullableConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        NullableConcreteLibfunc::Null(_) => build_nullable_null(builder),
        NullableConcreteLibfunc::IntoNullable(_) => build_identity(builder),
        NullableConcreteLibfunc::FromNullable(libfunc) => {
            build_nullable_from_nullable(builder, libfunc)
        }
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
fn build_nullable_from_nullable(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndTypeConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    // Check that the size of the inner type is nonzero and the argument is a simple deref
    // expression.
    //
    // This guarantees that values are written to the memory address pointed by the `Nullable<>`
    // instance in the case it is not `null`.
    // It follows that this address cannot be zero, since the Cairo-AIR guarantees that all
    // memory accesses have address >= 1.
    //
    // Therefore, we can be sure that the address is nonzero if and only if the instance is not
    // `null`.
    assert!(
        builder.program_info.type_sizes[&libfunc.ty] > 0,
        "Nullable<> cannot be used for types of size 0."
    );

    builder.refs[0]
        .expression
        .try_unpack_single()?
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    build_jump_nz(builder)
}
