use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::uint::{Uint8Concrete, UintConstConcreteLibfunc, UintTraits};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

/// Builds invocations for uint const values.
pub fn build_const<TUintTraits: UintTraits>(
    libfunc: &UintConstConcreteLibfunc<TUintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.into()))].into_iter(),
    ))
}

/// Builds instructions for Sierra u8 operations.
pub fn build_u8(
    libfunc: &Uint8Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint8Concrete::Const(libfunc) => build_const(libfunc, builder),
    }
}
