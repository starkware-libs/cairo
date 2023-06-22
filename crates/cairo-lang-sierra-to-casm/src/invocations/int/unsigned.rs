use std::ops::Shl;

use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::unsigned::{UintConcrete, UintTraits};
use cairo_lang_sierra::extensions::int::{IntMulTraits, IntOperator};
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use num_bigint::{BigInt, ToBigInt};

use super::{build_const, build_small_diff, build_small_wide_mul};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    add_input_variables, bitwise, get_non_fallthrough_statement_id, misc, CompiledInvocation,
    CompiledInvocationBuilder, CostValidationInfo, InvocationError,
};

/// Handles a small uint overflowing add operation.
/// All parameters values are smaller than `limit`.
fn build_small_uint_overflowing_add(
    builder: CompiledInvocationBuilder<'_>,
    limit: u128,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
            let orig_range_check = range_check;
            tempvar no_overflow;
            let deferred_a_plus_b = a + b;
            const limit_fixer = (u128::MAX - limit + 1);
            const limit = limit;
            hint TestLessThan {lhs: deferred_a_plus_b, rhs: limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Overflow:
            // Here we know that `limit <= a + b < 2 * limit - 1`.
            tempvar temp_a_plus_b = deferred_a_plus_b;
            tempvar fixed_a_plus_b = temp_a_plus_b - limit;
            assert fixed_a_plus_b = *(range_check++);
            jump Target;
        NoOverflow:
            // Here we know that `0 <= a + b < limit`
            // ==> `a + b + 2**128 - limit < limit + 2**128 - limit`
            // ==> `a + b + 2**128 - limit < 2**128`.

            tempvar temp_fixed_a_plus_b;
            tempvar a_plus_b = deferred_a_plus_b;
            assert temp_fixed_a_plus_b = a_plus_b + limit_fixer;
            assert temp_fixed_a_plus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_plus_b]], None),
            ("Target", &[&[range_check], &[fixed_a_plus_b]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a small uint conversion from felt252.
fn build_small_uint_from_felt252<const LIMIT: u128, const K: u8>(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const limit = LIMIT;
        tempvar is_small;
        hint TestLessThan {lhs: value, rhs: limit} into {dst: is_small};
        jump IsSmall if is_small != 0;
        tempvar shifted_value = value - limit;
    }
    match K {
        1 => {
            let auxiliary_vars: [_; 4] = std::array::from_fn(|_| casm_builder.alloc_var(false));
            validate_under_limit::<K>(
                &mut casm_builder,
                &(-Felt252::from(LIMIT)).to_biguint().to_bigint().unwrap(),
                shifted_value,
                range_check,
                &auxiliary_vars,
            );
            casm_build_extend! {casm_builder, jump Done;};
        }
        2 => {
            let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
            validate_under_limit::<K>(
                &mut casm_builder,
                &(-Felt252::from(LIMIT)).to_biguint().to_bigint().unwrap(),
                shifted_value,
                range_check,
                &auxiliary_vars,
            );
        }
        _ => unreachable!("Only K value of 1 or 2 are supported."),
    }
    casm_build_extend! {casm_builder,
        IsSmall:
        assert value = *(range_check++);
        // value + 2**128 - limit < 2**128 ==> value < limit
        const fixer_limit = (u128::MAX - LIMIT + 1);
        tempvar value_upper_limit = value + fixer_limit;
        assert value_upper_limit = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Done", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a small uint conversion from felt252.
fn build_divmod<const BOUND: u128>(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // Sanity check: make sure BOUND is not too large.
    assert!(BigInt::from(BOUND).shl(128) < Felt252::prime().to_bigint().unwrap(),);

    let [range_check, a, b] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar r_plus_1;
        tempvar b_minus_r_minus_1;
        tempvar bq;
        tempvar q;
        tempvar r;
        hint DivMod { lhs: a, rhs: b } into { quotient: q, remainder: r };

        // Verify `0 <= r`.
        assert r = *(range_check++);

        // Verify `r < b` by constraining `0 <= b - (r + 1)`.
        const one = 1;
        assert r_plus_1 = r + one;
        assert b_minus_r_minus_1 = b - r_plus_1;
        assert b_minus_r_minus_1 = *(range_check++);

        // Check that `0 <= q < 2**128`.
        assert q = *(range_check++);

        // Check that `a = q * b + r`. Both hands are in the range [0, 2**128 * BOUND),
        // since q < 2**128 and b < BOUND.
        // Therefore, both hands are in the range [0, PRIME), and thus the equality
        // is an equality as integers (rather than only as field elements).
        assert bq = b * q;
        assert a = bq + r;
    }
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[q], &[r]], None)],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a uint square root operation.
pub fn build_sqrt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(3) range_check;
        deref value;
    };

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar fixed_root;
        tempvar root_squared;
        tempvar value_minus_root_squared;
        tempvar root_times_two;
        tempvar diff;
        tempvar root;

        // Calculate the square root.
        hint SquareRoot { value: value} into { dst: root };

        // Assert root is in [0, 2**125) by asserting:
        // (root + (2**128-1) - (2**125-1)) is in [0, 2**128) and root is in [0, 2**128).
        // The second assertion is needed because if root is very large (e.g., P - 1) the first
        // assertion may be true.
        const u125_upper_fixer = BigInt::from(u128::MAX - (u128::pow(2, 125) - 1));
        assert fixed_root = root + u125_upper_fixer;
        assert root = *(range_check++);
        assert fixed_root = *(range_check++);

        // Assert root**2 is in [0, value] by asserting (value - root**2) is in [0, 2**128).
        // Since we know root**2 is in [0, 2**250) (because we asserted root is in [0, 2**125))
        // and that value is in [0, 2**250) this is enough.
        assert root_squared = root * root;
        assert value_minus_root_squared = value - root_squared;
        assert value_minus_root_squared = *(range_check++);

        // Assert value is in [0, (root + 1)**2 ) by asserting (2*root - (value - root**2)) is in
        // [0, 2**128). this is equivalent because
        // (root + 1)**2 - 1 - value = 2*root - (value - root**2) .
        assert root_times_two = root + root;
        assert diff = root_times_two - value_minus_root_squared;
        assert diff = *(range_check++);
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[root]], None)],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Builds instructions for Sierra u8/u16/u32/u64 operations.
pub fn build_uint<TUintTraits: UintTraits + IntMulTraits + IsZeroTraits, const LIMIT: u128>(
    libfunc: &UintConcrete<TUintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        UintConcrete::Const(libfunc) => build_const(libfunc, builder),
        UintConcrete::SquareRoot(_) => build_sqrt(builder),
        UintConcrete::Equal(_) => misc::build_cell_eq(builder),
        UintConcrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => build_small_uint_overflowing_add(builder, LIMIT),
            IntOperator::OverflowingSub => build_small_diff(builder, BigInt::from(LIMIT)),
        },
        UintConcrete::ToFelt252(_) => misc::build_identity(builder),
        UintConcrete::FromFelt252(_) => build_small_uint_from_felt252::<LIMIT, 2>(builder),
        UintConcrete::IsZero(_) => misc::build_is_zero(builder),
        UintConcrete::Divmod(_) => build_divmod::<LIMIT>(builder),
        UintConcrete::Bitwise(_) => bitwise::build(builder),
        UintConcrete::WideMul(_) => build_small_wide_mul(builder),
    }
}
