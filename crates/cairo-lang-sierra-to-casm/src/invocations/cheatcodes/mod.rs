use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use self::call::build_call;
use self::declare::build_declare;
use self::declare_cairo0::build_declare_cairo0;
use self::deploy::build_deploy;
use self::invoke::build_invoke;
use self::mock_call::build_mock_call;
use self::prepare::build_prepare;
use self::print::build_protostar_print;
use self::start_prank::build_start_prank;
use self::start_roll::build_start_roll;
use self::start_warp::build_start_warp;
use self::stop_prank::build_stop_prank;
use self::stop_roll::build_stop_roll;
use self::stop_warp::build_stop_warp;
use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod call;
mod declare;
mod declare_cairo0;
mod deploy;
mod invoke;
mod mock_call;
mod prepare;
mod print;
mod start_prank;
mod start_roll;
mod start_warp;
mod stop_prank;
mod stop_roll;
mod stop_warp;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CheatcodesConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CheatcodesConcreteLibFunc::StartRoll(_) => build_start_roll(builder),
        CheatcodesConcreteLibFunc::StopRoll(_) => build_stop_roll(builder),
        CheatcodesConcreteLibFunc::StartWarp(_) => build_start_warp(builder),
        CheatcodesConcreteLibFunc::StopWarp(_) => build_stop_warp(builder),
        CheatcodesConcreteLibFunc::Declare(_) => build_declare(builder),
        CheatcodesConcreteLibFunc::DeclareCairo0(_) => build_declare_cairo0(builder),
        CheatcodesConcreteLibFunc::StartPrank(_) => build_start_prank(builder),
        CheatcodesConcreteLibFunc::StopPrank(_) => build_stop_prank(builder),
        CheatcodesConcreteLibFunc::Invoke(_) => build_invoke(builder),
        CheatcodesConcreteLibFunc::MockCall(_) => build_mock_call(builder),
        CheatcodesConcreteLibFunc::Deploy(_) => build_deploy(builder),
        CheatcodesConcreteLibFunc::Prepare(_) => build_prepare(builder),
        CheatcodesConcreteLibFunc::Call(_) => build_call(builder),
        CheatcodesConcreteLibFunc::Print(_) => build_protostar_print(builder),
    }
}
