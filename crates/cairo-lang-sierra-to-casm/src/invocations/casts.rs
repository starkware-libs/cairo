use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::casts::{CastConcreteLibfunc, DowncastConcreteLibfunc};
use num_bigint::BigUint;
use num_traits::Pow;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

/// Builds instructions for Sierra cast operations.
pub fn build(
    libfunc: &CastConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CastConcreteLibfunc::Downcast(libfunc) => build_downcast(builder, libfunc),
        CastConcreteLibfunc::Upcast(_) => build_identity(builder),
    }
}

/// Builds Casm instructions for [CastConcreteLibfunc::Downcast].
pub fn build_downcast(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &DowncastConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder,
        buffer(0) range_check;
        deref value;
    );

    // The casm code below assumes both types are at most 128 bits.
    assert!(
        libfunc.from_nbits <= 128 && libfunc.to_nbits <= 128,
        "Downcasting from types of size > 128 bit is not supported."
    );

    let two = BigUint::from(2u64);
    let bound = two.clone().pow(libfunc.to_nbits);
    let rc_bound = two.pow(128_usize);

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;

        // Use a hint to guess whether the result is in range (is_valid=1) or overflows
        // (is_valid=0).
        tempvar is_valid;
        const limit = bound.clone();
        hint TestLessThan {lhs: value, rhs: limit} into {dst: is_valid};
        jump Success if is_valid != 0;
        // Failure.
        // value >= bound  <=>  value - bound >= 0.
        // Note that we know that 0 <= value < 2^128.
        tempvar shifted_value = value - limit;
        assert shifted_value = *(range_check++);
        jump Failure;

        // Success.
        Success:
        // Verify that the value is in range:
        // value < limit  <=>  value + (rc_bound - bound) < rc_bound.
        const pos_shift = rc_bound - bound;
        tempvar shifted_value = value + pos_shift;
        assert shifted_value = *(range_check++);
    };

    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Failure", &[&[range_check]], Some(target_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
