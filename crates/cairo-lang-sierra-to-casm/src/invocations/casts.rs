use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::casts::{
    CastConcreteLibfunc, CastType, DowncastConcreteLibfunc,
};
use cairo_lang_sierra::extensions::utils::Range;
use num_bigint::BigInt;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::range_reduction::build_felt252_range_reduction;
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};
use crate::references::ReferenceExpression;

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
    if libfunc.from_range.is_full_felt252_range() {
        return build_felt252_range_reduction(builder, &libfunc.to_range, true);
    }
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder,
        buffer(1) range_check;
        deref value;
    );

    // The casm code below assumes both types are at most 128 bits.
    assert!(
        libfunc.from_range.is_small_range() && libfunc.to_range.is_small_range(),
        "Downcasting is not supported for types of size > 128 bit."
    );

    casm_build_extend!(casm_builder, let orig_range_check = range_check;);

    match libfunc.cast_type() {
        CastType { overflow_above: false, overflow_below: false } => {
            return handle_downcast_no_overflow(builder);
        }
        CastType { overflow_above: true, overflow_below: false } => add_downcast_overflow_above(
            &mut casm_builder,
            value,
            range_check,
            &libfunc.to_range.upper,
        ),
        CastType { overflow_above: false, overflow_below: true } => add_downcast_overflow_below(
            &mut casm_builder,
            value,
            range_check,
            &libfunc.to_range.lower,
        ),
        CastType { overflow_above: true, overflow_below: true } => {
            add_downcast_overflow_both(&mut casm_builder, value, range_check, &libfunc.to_range)
        }
    }

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

/// Builds Casm instructions for [CastConcreteLibfunc::Downcast] for a trivial case where no
/// overflow is possible.
fn handle_downcast_no_overflow(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let success = vec![
        ReferenceExpression::from_cell(range_check.clone()),
        ReferenceExpression::from_cell(value.clone()),
    ];
    let unreachable_failure = vec![ReferenceExpression::from_cell(range_check.clone())];
    Ok(builder.build(
        vec![],
        vec![],
        [success.into_iter(), unreachable_failure.into_iter()].into_iter(),
    ))
}

/// Adds instructions for downcasting where the value may only overflow above.
/// Note: The type of `value` must be unsigned.
fn add_downcast_overflow_above(
    casm_builder: &mut CasmBuilder,
    value: Var,
    range_check: Var,
    upper_bound: &BigInt,
) {
    casm_build_extend! {casm_builder,
        // Use a hint to guess whether the result is in range (is_valid=1) or overflows
        // (is_valid=0).
        // `value` is non-negative as it must be unsigned.
        tempvar is_valid;
        const value_limit_imm = upper_bound.clone();
        hint TestLessThan {lhs: value, rhs: value_limit_imm} into {dst: is_valid};
        jump Success if is_valid != 0;
    }
    // Validating we overflowed above.
    validate_ge(casm_builder, range_check, value, upper_bound);
    casm_build_extend!(casm_builder, jump Failure;);

    casm_build_extend!(casm_builder, Success:);
    validate_lt(casm_builder, range_check, value, upper_bound);
}

/// Adds instructions for downcasting where the value may only overflow below.
fn add_downcast_overflow_below(
    casm_builder: &mut CasmBuilder,
    value: Var,
    range_check: Var,
    lower_bound: &BigInt,
) {
    casm_build_extend! {casm_builder,
        const minus_lower_bound = -lower_bound;
        let canonical_value = value + minus_lower_bound;
        // Use a hint to guess whether the result is in range (is_valid=1) or overflows
        // (is_valid=0).
        tempvar is_valid;
        const rc_bound_imm = (BigInt::from(u128::MAX) + 1) as BigInt;
        hint TestLessThan {lhs: canonical_value, rhs: rc_bound_imm} into {dst: is_valid};
        jump Success if is_valid != 0;
        // Overflow below.
    }
    validate_lt(casm_builder, range_check, value, lower_bound);
    casm_build_extend!(casm_builder, jump Failure;);

    casm_build_extend!(casm_builder, Success:);
    validate_ge(casm_builder, range_check, value, lower_bound);
}

/// Adds instructions for downcasting where the value may both overflow and underflow.
fn add_downcast_overflow_both(
    casm_builder: &mut CasmBuilder,
    value: Var,
    range_check: Var,
    to_range: &Range,
) {
    casm_build_extend! {casm_builder,
        const minus_to_min_value = -to_range.lower.clone();
        let canonical_value = value + minus_to_min_value;
        // Use a hint to guess whether the result is in range (is_valid=1) or overflows
        // (is_valid=0).
        tempvar is_valid;
        const to_range_size = to_range.size();
        hint TestLessThan {lhs: canonical_value, rhs: to_range_size} into {dst: is_valid};
        jump Success if is_valid != 0;
        // Failure.
        const rc_bound_imm = (BigInt::from(u128::MAX) + 1) as BigInt;
        tempvar is_overflow_above;
        // If the `canonical_value` is negative (and therefore larger than 2**128) this is an overflow
        // below.
        hint TestLessThan {lhs: canonical_value, rhs: rc_bound_imm} into {dst: is_overflow_above};
        jump OverflowAbove if is_overflow_above != 0;
    }
    // Overflow below.
    let prev = casm_builder.curr_ap_change();
    validate_lt(casm_builder, range_check, value, &to_range.lower);
    if prev == casm_builder.curr_ap_change() {
        // `validate_ge` would have `ap_change=1` so we need to match it.
        casm_build_extend!(casm_builder, tempvar _x;);
    }
    casm_build_extend!(casm_builder, jump Failure;);

    casm_build_extend!(casm_builder, OverflowAbove:);
    let prev = casm_builder.curr_ap_change();
    validate_ge(casm_builder, range_check, value, &to_range.upper);
    if prev == casm_builder.curr_ap_change() {
        // `validate_lt` would have `ap_change=1` so we need to match it.
        casm_build_extend!(casm_builder, tempvar _x;);
    }
    casm_build_extend!(casm_builder, jump Failure;);

    casm_build_extend!(casm_builder, Success:);
    validate_ge(casm_builder, range_check, value, &to_range.lower);
    validate_lt(casm_builder, range_check, value, &to_range.upper);
}

/// Validates that `value` is smaller than `bound`.
fn validate_lt(casm_builder: &mut CasmBuilder, range_check: Var, value: Var, bound: &BigInt) {
    casm_build_extend! {casm_builder,
        // value < bound  <=>  value + (2**128 - bound) < 2**128.
        const pos_shift = (BigInt::from(u128::MAX) + 1 - bound) as BigInt;
        maybe_tempvar shifted_value = value + pos_shift;
        assert shifted_value = *(range_check++);
    };
}

/// Validates that `value` is greater or equal to `bound`.
/// If `bound` is zero, only range checks without an additional calculation.
fn validate_ge(casm_builder: &mut CasmBuilder, range_check: Var, value: Var, bound: &BigInt) {
    casm_build_extend! {casm_builder,
        // value >= bound  <=>  value - bound >= 0.
        const bound = bound.clone();
        maybe_tempvar shifted_value = value - bound;
        assert shifted_value = *(range_check++);
    };
}
