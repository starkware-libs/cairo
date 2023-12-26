use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::casts::{
    CastConcreteLibfunc, CastType, DowncastConcreteLibfunc, IntTypeInfo,
};
use num_bigint::BigInt;
use num_traits::Zero;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
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
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder,
        buffer(1) range_check;
        deref value;
    );

    // The casm code below assumes both types are at most 128 bits.
    assert!(
        libfunc.from_info.nbits <= 128 && libfunc.to_info.nbits <= 128,
        "Downcasting from types of size > 128 bit is not supported."
    );

    casm_build_extend!(casm_builder, let orig_range_check = range_check;);

    let to_values = TypeValues::new(&libfunc.to_info);
    match libfunc.from_info.cast_type(&libfunc.to_info) {
        CastType { overflow_above: false, overflow_below: false } => {
            return handle_downcast_no_overflow(builder);
        }
        CastType { overflow_above: true, overflow_below: false } => {
            // Signed to signed would go the `both` or `no_overflow` cases.
            // Signed to unsigned would go the `both` or `overflow_below` case.
            assert!(
                !libfunc.from_info.signed,
                "`from` type must be unsigned for the overflow above case."
            );
            add_downcast_overflow_above(&mut casm_builder, value, range_check, &(to_values.max + 1))
        }
        CastType { overflow_above: false, overflow_below: true } => {
            add_downcast_overflow_below(&mut casm_builder, value, range_check, &to_values.min)
        }
        CastType { overflow_above: true, overflow_below: true } => {
            add_downcast_overflow_both(&mut casm_builder, value, range_check, &to_values)
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

#[derive(Debug)]
struct TypeValues {
    /// The minimum value of the type.
    min: BigInt,
    /// The maximum value of the type.
    max: BigInt,
    /// The size of the type.
    size: BigInt,
}
impl TypeValues {
    fn new(info: &IntTypeInfo) -> Self {
        Self {
            min: if info.signed { -(BigInt::from(1) << (info.nbits - 1)) } else { BigInt::from(0) },
            max: (BigInt::from(1) << (if info.signed { info.nbits - 1 } else { info.nbits })) - 1,
            size: BigInt::from(1) << info.nbits,
        }
    }
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
    to_values: &TypeValues,
) {
    casm_build_extend! {casm_builder,
        const minus_to_min_value = -to_values.min.clone();
        let canonical_value = value + minus_to_min_value;
        // Use a hint to guess whether the result is in range (is_valid=1) or overflows
        // (is_valid=0).
        tempvar is_valid;
        const to_range_size = to_values.size.clone();
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
    validate_lt(casm_builder, range_check, value, &to_values.min);
    casm_build_extend!(casm_builder, jump Failure;);

    casm_build_extend!(casm_builder, OverflowAbove:);
    validate_ge(casm_builder, range_check, value, &(&to_values.max + 1));
    casm_build_extend!(casm_builder, jump Failure;);

    casm_build_extend!(casm_builder, Success:);
    validate_ge(casm_builder, range_check, value, &to_values.min);
    validate_lt(casm_builder, range_check, value, &(&to_values.max + 1));
}

/// Validates that `value` is smaller than `bound`.
fn validate_lt(casm_builder: &mut CasmBuilder, range_check: Var, value: Var, bound: &BigInt) {
    casm_build_extend! {casm_builder,
        // value < bound  <=>  value + (2**128 - bound) < 2**128.
        const pos_shift = (BigInt::from(u128::MAX) + 1 - bound) as BigInt;
        tempvar shifted_value = value + pos_shift;
        assert shifted_value = *(range_check++);
    };
}

/// Validates that `value` is greater or equal to `bound`.
fn validate_ge(casm_builder: &mut CasmBuilder, range_check: Var, value: Var, bound: &BigInt) {
    if bound.is_zero() {
        casm_build_extend! {casm_builder, assert value = *(range_check++);};
    } else {
        casm_build_extend! {casm_builder,
            // value >= bound  <=>  value - bound >= 0.
            const bound = bound.clone();
            tempvar shifted_value = value - bound;
            assert shifted_value = *(range_check++);
        };
    }
}
