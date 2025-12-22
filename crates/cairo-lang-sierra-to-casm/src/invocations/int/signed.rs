use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::{IntMulTraits, IntOperator};
use cairo_lang_sierra::extensions::utils::Range;
use cairo_lang_sierra::program::{BranchInfo, BranchTarget};
use num_bigint::BigInt;

use super::{add_input_variables, build_const, build_small_diff, build_small_wide_mul};
use crate::invocations::range_reduction::build_felt252_range_reduction;
use crate::invocations::{
    BuiltinInfo, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError, misc,
};

/// Handles a signed integer conversion from felt252.
/// `[min_value, max_value]` is the range of the signed integer.
pub fn build_sint_from_felt252(
    builder: CompiledInvocationBuilder<'_>,
    min_value: i128,
    max_value: i128,
) -> Result<CompiledInvocation, InvocationError> {
    assert!(min_value <= 0, "min_value must be non-positive");
    assert!(max_value > 0, "max_value must be positive");
    build_felt252_range_reduction(builder, &Range::closed(min_value, max_value), false)
}

/// Handles overflowing addition and subtraction of signed integers.
///
/// This function implements overflow detection and handling for signed integer operations.
/// The algorithm works in three stages:
///
/// 1. **Canonicalization**: Shifts the valid range `[min_value, max_value]` to `[0, range_size)`
///    by adding `-min_value` to the result. This simplifies range checking.
///
/// 2. **Range detection**: Uses three branches to handle different cases:
///    - `IsInRange`: Value is within the valid range `[min_value, max_value]`
///    - `IsAbove`: Value exceeds `max_value` (overflow)
///    - `Below`: Value is below `min_value` (underflow)
///
/// 3. **Range checking**: For each branch, performs range checks using transformations that ensure
///    the check value fits within `[0, 2^128)`. This is necessary because Cairo's range check
///    builtin only works with non-negative values less than `2^128`.
///
/// The function returns three branches:
/// - `Fallthrough`: Operation succeeded, value is in range
/// - `Below`: Underflow occurred, returns wrapped value `value + range_size`
/// - `Above`: Overflow occurred, returns wrapped value `value - range_size`
///
/// # Parameters
/// - `min_value`: Lower bound of the signed integer range (must be ≤ 0)
/// - `max_value`: Upper bound of the signed integer range (must be > 0)
/// - `op`: The operation to perform (`OverflowingAdd` or `OverflowingSub`)
///
/// # Assumptions
/// - `min_value <= 0` and `max_value > 0`
/// - The range size `(max_value - min_value + 1) < 2^128`
pub fn build_sint_overflowing_operation(
    builder: CompiledInvocationBuilder<'_>,
    min_value: i128,
    max_value: i128,
    op: IntOperator,
) -> Result<CompiledInvocation, InvocationError> {
    assert!(min_value <= 0, "min_value must be non-positive");
    assert!(max_value > 0, "max_value must be positive");
    let [range_check, lhs, rhs] = builder.try_get_single_cells()?;
    let [
        BranchInfo { target: BranchTarget::Fallthrough, results: _ },
        BranchInfo { target: BranchTarget::Statement(underflow_handle_statement_id), results: _ },
        BranchInfo { target: BranchTarget::Statement(overflow_handle_statement_id), results: _ },
    ] = builder.invocation.branches.as_slice()
    else {
        panic!("malformed invocation");
    };
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref lhs;
        deref rhs;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar value;
    }
    match op {
        IntOperator::OverflowingAdd => {
            casm_build_extend! {casm_builder, assert value = lhs + rhs;};
        }
        IntOperator::OverflowingSub => {
            casm_build_extend! {casm_builder, assert value = lhs - rhs;};
        }
    }
    // Canonicalize the range: shift [min_value, max_value] to [0, range_size).
    // This allows us to check if the value is in range by testing if canonical_value < range_size.
    casm_build_extend! {casm_builder,
        const positive_range_fixer = -BigInt::from(min_value);
        const range_size = BigInt::from(max_value) - BigInt::from(min_value) + BigInt::from(1);
        let canonical_value = value + positive_range_fixer;
        tempvar is_in_range;
        hint TestLessThan {lhs: canonical_value, rhs: range_size} into {dst: is_in_range};
        jump IsInRange if is_in_range != 0;
        // Value is not in the canonical range. Determine if it's above or below.
        tempvar is_above;
        // Bound for addition or subtraction of any 2 numbers in [i128::MIN, i128::MAX].
        // The maximum possible result from adding/subtracting two i128 values is:
        // max(2 * i128::MAX, i128::MAX - i128::MIN) + 1
        // = max(2*(2**127 - 1), 2**127 - 1 -(-2**127)) + 1
        // = max(2**128 - 2, 2**128 - 1) + 1
        // = 2**128
        // We use this bound to distinguish between values that are above max_value (overflow)
        // and values that are below min_value (underflow) when working modulo the field.
        const above_bound = BigInt::from(u128::MAX) + BigInt::from(1);
        hint TestLessThan {lhs: value, rhs: above_bound} into {dst: is_above};
        jump IsAbove if is_above != 0;
        // Below range case (underflow): value < min_value
        // To range-check that value < min_value, we transform it to a non-negative value:
        // value + 2**128 - min_value < 2**128  ⟺  value < min_value
        // This works because if value < min_value (a negative number), adding 2**128 makes it
        // positive and less than 2**128 when working in the field.
        const min_value_fixer =
            BigInt::from(u128::MAX) + BigInt::from(1) - BigInt::from(min_value);
        tempvar rc_val = value + min_value_fixer;
        assert rc_val = *(range_check++);
        // Return the wrapped value for underflow handling
        let fixed_below = value + range_size;
        jump Below;
    IsAbove:
        // Above range case (overflow): value > max_value
        // To range-check that value > max_value, we check that value - (max_value + 1) >= 0:
        // value - (max_value + 1) >= 0  ⟺  value > max_value
        // Since value > max_value and max_value >= 0, the result is non-negative.
        const max_value_plus_one = BigInt::from(max_value) + BigInt::from(1);
        tempvar rc_val = value - max_value_plus_one;
        assert rc_val = *(range_check++);
        // Return the wrapped value for overflow handling
        let fixed_above = value - range_size;
        jump Above;
    IsInRange:
        // Value is within the valid range [min_value, max_value].
        // Range-check the canonical value (which is in [0, range_size)).
        tempvar rc_val = canonical_value;
        assert rc_val = *(range_check++);
    }
    // For full i128 range (i128::MIN to i128::MAX), the previous range check on canonical_value
    // already ensures the value is in range. For smaller types (i8, i16, i32, i64), we need an
    // additional check to ensure value <= max_value (the canonical check only ensures
    // value >= min_value after canonicalization).
    if !(min_value == i128::MIN && max_value == i128::MAX) {
        casm_build_extend! {casm_builder,
            // To range-check that value <= max_value, we transform it:
            // value + 2**128 - max_value - 1 < 2**128  ⟺  value <= max_value
            // This works because if value <= max_value, adding (2**128 - max_value - 1) keeps
            // the result within [0, 2**128) when working in the field.
            const fixer_limit = BigInt::from(u128::MAX) - BigInt::from(max_value);
            tempvar rc_val = value + fixer_limit;
            assert rc_val = *(range_check++);
        };
    }
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Below", &[&[range_check], &[fixed_below]], Some(*underflow_handle_statement_id)),
            ("Above", &[&[range_check], &[fixed_above]], Some(*overflow_handle_statement_id)),
        ],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}

/// Builds instructions for Sierra i8/i16/i32/i64 operations.
pub fn build_sint<
    TSintTraits: SintTraits + IntMulTraits,
    const MIN_VALUE: i128,
    const MAX_VALUE: i128,
>(
    libfunc: &SintConcrete<TSintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        SintConcrete::Const(libfunc) => build_const(libfunc, builder),
        SintConcrete::Equal(_) => misc::build_cell_eq(builder),
        SintConcrete::ToFelt252(_) => misc::build_identity(builder),
        SintConcrete::FromFelt252(_) => build_sint_from_felt252(builder, MIN_VALUE, MAX_VALUE),
        SintConcrete::WideMul(_) => build_small_wide_mul(builder),
        SintConcrete::Operation(libfunc) => {
            build_sint_overflowing_operation(builder, MIN_VALUE, MAX_VALUE, libfunc.operator)
        }
        SintConcrete::Diff(_) => {
            build_small_diff(builder, BigInt::from(MAX_VALUE) + 1 - BigInt::from(MIN_VALUE))
        }
    }
}
