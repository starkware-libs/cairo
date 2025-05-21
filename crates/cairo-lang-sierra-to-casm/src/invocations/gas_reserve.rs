use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::gas_reserve::GasReserveConcreteLibfunc;
use num_bigint::BigInt;

use crate::invocations::int::SmallDiffHelper;
use crate::invocations::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, add_input_variables,
};

/// Builds instructions for Sierra `GasReserve` operations.
pub fn build(
    libfunc: &GasReserveConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasReserveConcreteLibfunc::Create(_) => build_gas_reserve_create(builder),
        GasReserveConcreteLibfunc::Utilize(_) => build_gas_reserve_utilize(builder),
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

/// Handles the gas_reserve_utilize invocation.
fn build_gas_reserve_utilize(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [gas_counter, reserve] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref gas_counter;
        deref reserve;
    };

    casm_build_extend! {casm_builder,
        let updated_gas = gas_counter + reserve;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[updated_gas]], None)],
        Default::default(),
    ))
}
