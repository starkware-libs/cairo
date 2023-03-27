use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use self::declare::build_declare;
use self::declare_cairo0::build_declare_cairo0;
use self::deploy::build_deploy;
use self::deploy_cairo0::build_deploy_cairo0;
use self::invoke::build_invoke;
use self::mock_call::build_mock_call;
use self::prepare::build_prepare;
use self::prepare_cairo0::build_prepare_cairo0;
use self::roll::build_roll;
use self::start_prank::build_start_prank;
use self::stop_prank::build_stop_prank;
use self::warp::build_warp;
use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod declare;
mod declare_cairo0;
mod deploy;
mod deploy_cairo0;
mod invoke;
mod mock_call;
mod prepare;
mod prepare_cairo0;
mod roll;
mod start_prank;
mod stop_prank;
mod warp;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CheatcodesConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CheatcodesConcreteLibFunc::Roll(_) => build_roll(builder),
        CheatcodesConcreteLibFunc::Warp(_) => build_warp(builder),
        CheatcodesConcreteLibFunc::Declare(_) => build_declare(builder),
        CheatcodesConcreteLibFunc::DeclareCairo0(_) => build_declare_cairo0(builder),
        CheatcodesConcreteLibFunc::StartPrank(_) => build_start_prank(builder),
        CheatcodesConcreteLibFunc::StopPrank(_) => build_stop_prank(builder),
        CheatcodesConcreteLibFunc::Invoke(_) => build_invoke(builder),
        CheatcodesConcreteLibFunc::MockCall(_) => build_mock_call(builder),
        CheatcodesConcreteLibFunc::Deploy(_) => build_deploy(builder),
        CheatcodesConcreteLibFunc::DeployCairo0(_) => build_deploy_cairo0(builder),
        CheatcodesConcreteLibFunc::Prepare(_) => build_prepare(builder),
        CheatcodesConcreteLibFunc::PrepareCairo0(_) => build_prepare_cairo0(builder),
    }
}
