use cairo_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use self::roll::build_roll;

use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod roll;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CheatcodesConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CheatcodesConcreteLibFunc::Roll(_) => build_roll(builder),
    }
}