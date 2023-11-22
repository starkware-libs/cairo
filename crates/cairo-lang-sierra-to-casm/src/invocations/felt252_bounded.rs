use cairo_lang_sierra::extensions::felt252_bounded::Felt252BoundedConcreteLibfunc;

use super::misc::build_identity;
use super::range_reduction::build_try_range_reduction;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra felt252_bounded operations.
pub fn build(
    libfunc: &Felt252BoundedConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Felt252BoundedConcreteLibfunc::FromFelt(_) => build_identity(builder),
        Felt252BoundedConcreteLibfunc::ConstrainRange(libfunc) => build_try_range_reduction(
            builder,
            &libfunc.lower_bound_in,
            &libfunc.upper_bound_in,
            &libfunc.lower_bound_out,
            &libfunc.upper_bound_out,
        ),
    }
}
