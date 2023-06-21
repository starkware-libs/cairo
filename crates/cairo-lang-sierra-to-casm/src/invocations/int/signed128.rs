use cairo_lang_sierra::extensions::int::signed128::Sint128Concrete;

use super::signed::{build_sint_from_felt252, build_sint_overflowing_operation};
use super::{build_const, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc;

/// Builds instructions for Sierra s128 operations.
pub fn build(
    libfunc: &Sint128Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Sint128Concrete::IsZero(_) => misc::build_is_zero(builder),
        Sint128Concrete::Const(libfunc) => build_const(libfunc, builder),
        Sint128Concrete::FromFelt252(_) => build_sint_from_felt252(builder, i128::MIN, i128::MAX),
        Sint128Concrete::ToFelt252(_) => misc::build_identity(builder),
        Sint128Concrete::Equal(_) => misc::build_cell_eq(builder),
        Sint128Concrete::Operation(libfunc) => {
            build_sint_overflowing_operation(builder, i128::MIN, i128::MAX, libfunc.operator)
        }
    }
}
