use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use self::{roll::build_roll, declare::build_declare};

use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod roll;
mod declare;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CheatcodesConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CheatcodesConcreteLibFunc::Roll(_) => build_roll(builder),
        CheatcodesConcreteLibFunc::Declare(_) => build_declare(builder),
    }
}