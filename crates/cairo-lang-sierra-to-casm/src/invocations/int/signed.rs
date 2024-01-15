use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::{IntMulTraits, IntOperator};
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use cairo_lang_sierra::extensions::utils::Range;
use cairo_lang_sierra::program::{BranchInfo, BranchTarget};
use num_bigint::BigInt;

use super::{add_input_variables, build_const, build_small_diff, build_small_wide_mul};
use crate::invocations::range_reduction::build_felt252_range_reduction;
use crate::invocations::{
    misc, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo, InvocationError,
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

/// Handles addition of signed integers.
/// `[min_value, max_value]` is the range of the signed integer.
/// Note: this function assumes that `min_value <= 0` and `max_value > 0` and that the range of
/// possible values is smaller than 2**128.
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
    casm_build_extend! {casm_builder,
        const positive_range_fixer = -BigInt::from(min_value);
        const range_size = BigInt::from(max_value) - BigInt::from(min_value) + BigInt::from(1);
        // Shift the valid range to [0, range_size).
        let canonical_value = value + positive_range_fixer;
        tempvar is_in_range;
        hint TestLessThan {lhs: canonical_value, rhs: range_size} into {dst: is_in_range};
        jump IsInRange if is_in_range != 0;
        tempvar is_above;
        // Bound for addition or subtraction of any 2 numbers in [i128::MIN, i128::MAX].
        // max(2 * i128::MAX, i128::MAX - i128::MIN) + 1
        // ==> max(2*(2**127 - 1), 2**127 - 1 -(-2**127)) + 1 ==> 2**128
        const above_bound = BigInt::from(u128::MAX) + BigInt::from(1);
        hint TestLessThan {lhs: value, rhs: above_bound} into {dst: is_above};
        jump IsAbove if is_above != 0;
        // Below range case.
        // We need to assert that the value is smaller than the lower limit:
        // value + 2**128 - min_value < 2**128 ==> value < min_value
        const min_value_fixer =
            BigInt::from(u128::MAX) + BigInt::from(1) - BigInt::from(min_value);
        tempvar rc_val = value + min_value_fixer;
        assert rc_val = *(range_check++);
        let fixed_below = value + range_size;
        jump Below;
    IsAbove:
        // We need to assert that the value is larger than the upper limit:
        // value - (max_value + 1) >= 0 ==> value > max_value
        const max_value_plus_one = BigInt::from(max_value) + BigInt::from(1);
        tempvar rc_val = value - max_value_plus_one;
        assert rc_val = *(range_check++);
        let fixed_above = value - range_size;
        jump Above;
    IsInRange:
        tempvar rc_val = canonical_value;
        assert rc_val = *(range_check++);
    }
    // For i128, the previous range check already made sure the value is in range.
    if !(min_value == i128::MIN && max_value == i128::MAX) {
        casm_build_extend! {casm_builder,
            // value + 2**128 - max_value - 1 < 2**128 ==> value <= max_value
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
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Builds instructions for Sierra i8/i16/i32/i64 operations.
pub fn build_sint<
    TSintTraits: SintTraits + IntMulTraits + IsZeroTraits,
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
        SintConcrete::IsZero(_) => misc::build_is_zero(builder),
        SintConcrete::WideMul(_) => build_small_wide_mul(builder),
        SintConcrete::Operation(libfunc) => {
            build_sint_overflowing_operation(builder, MIN_VALUE, MAX_VALUE, libfunc.operator)
        }
        SintConcrete::Diff(_) => {
            build_small_diff(builder, BigInt::from(MAX_VALUE) + 1 - BigInt::from(MIN_VALUE))
        }
    }
}
