use cairo_lang_casm::builder::Var;
use cairo_lang_sierra::extensions::gas_reserve::GasReserveConcreteLibfunc;
use num_bigint::BigInt;

use crate::invocations::int::SmallDiffHelper;
use crate::invocations::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra `GasReserve` operations.
pub fn build(
    libfunc: &GasReserveConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasReserveConcreteLibfunc::Create(_) => build_gas_reserve_create(builder),
    }
}

/// Handles the gas_reserve_create invocation.
fn build_gas_reserve_create(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let data = SmallDiffHelper::new(builder, BigInt::from(u128::MAX) + BigInt::from(1))?;
    let no_overflow_res: &[&[Var]] = &[&[data.range_check], &[data.a_minus_b], &[data.b]];
    let overflow_res: &[&[Var]] = &[&[data.range_check], &[data.a]];
    data.finalize(no_overflow_res, overflow_res)
}
