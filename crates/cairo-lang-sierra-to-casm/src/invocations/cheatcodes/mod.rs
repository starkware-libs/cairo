use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use self::{
    roll::build_roll,
    declare::build_declare,
    start_prank::build_start_prank,
    warp::build_warp,
    invoke::build_invoke,
    mock_call::build_mock_call,
};

use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod declare;
mod roll;
mod start_prank;
mod warp;
mod invoke;
mod mock_call;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CheatcodesConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CheatcodesConcreteLibFunc::Roll(_) => build_roll(builder),
        CheatcodesConcreteLibFunc::Warp(_) => build_warp(builder),
        CheatcodesConcreteLibFunc::Declare(_) => build_declare(builder),
        CheatcodesConcreteLibFunc::StartPrank(_) => build_start_prank(builder),
        CheatcodesConcreteLibFunc::Invoke(_) => build_invoke(builder),
        CheatcodesConcreteLibFunc::MockCall(_) => build_mock_call(builder),
    }
}
