use cairo_lang_sierra::extensions::casts::CastConcreteLibfunc;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra cast operations.
pub fn build(
    libfunc: &CastConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CastConcreteLibfunc::Upcast(_) => build_identity(builder),
    }
}
