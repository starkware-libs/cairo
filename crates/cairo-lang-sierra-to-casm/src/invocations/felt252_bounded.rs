use cairo_lang_sierra::extensions::range_reduction::Felt252BoundedConcreteLibfunc;

use super::range_reduction::build_try_range_reduction;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra felt252_bounded operations.
pub fn build(
    libfunc: &Felt252BoundedConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Felt252BoundedConcreteLibfunc::ConstrainRange(libfunc) => {
            build_try_range_reduction(builder, &libfunc.in_range, &libfunc.out_range)
        }
    }
}
