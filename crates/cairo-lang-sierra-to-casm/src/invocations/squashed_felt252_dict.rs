use cairo_lang_sierra::extensions::squashed_felt252_dict::SquashedFelt252DictConcreteLibfunc;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra squashed dict operations.
pub fn build(
    libfunc: &SquashedFelt252DictConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        SquashedFelt252DictConcreteLibfunc::IntoEntries(_) => build_identity(builder),
    }
}
