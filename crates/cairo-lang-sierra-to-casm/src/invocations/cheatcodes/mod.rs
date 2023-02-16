use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use self::{roll::build_roll, declare::build_declare, start_prank::build_start_prank};

use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod roll;
mod declare;
mod start_prank;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CheatcodesConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CheatcodesConcreteLibFunc::Roll(_) => build_roll(builder),
        CheatcodesConcreteLibFunc::Declare(_) => build_declare(builder),
        CheatcodesConcreteLibFunc::StartPrank(_) => build_start_prank(builder),
    }
}