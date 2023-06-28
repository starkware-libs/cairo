use cairo_lang_sierra::extensions::bytes31::Bytes31ConcreteLibfunc;

use super::misc::{build_identity, build_unsigned_try_from_felt252};
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra bytes31 operations.
pub fn build(
    libfunc: &Bytes31ConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Bytes31ConcreteLibfunc::TryFromFelt252(_) => build_unsigned_try_from_felt252(builder, 248),
        Bytes31ConcreteLibfunc::ToFelt252(_) => build_identity(builder),
    }
}
