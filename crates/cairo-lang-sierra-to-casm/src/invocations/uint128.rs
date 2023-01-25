use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::uint::IntOperator;
use cairo_lang_sierra::extensions::uint128::Uint128Concrete;
use num_bigint::BigInt;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};

/// Builds instructions for Sierra u128 operations.
pub fn build(
    libfunc: &Uint128Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint128Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => build_u128_overflowing_add(builder),
            IntOperator::OverflowingSub => build_u128_overflowing_sub(builder),
        },
        Uint128Concrete::DivMod(_) => build_u128_divmod(builder),
        Uint128Concrete::WideMul(_) => build_u128_widemul(builder),
        Uint128Concrete::JumpNotZero(_) => misc::build_jump_nz(builder),
        Uint128Concrete::Const(libfunc) => super::uint::build_const(libfunc, builder),
        Uint128Concrete::FromFelt(_) => build_u128_from_felt(builder),
        Uint128Concrete::ToFelt(_) => misc::build_identity(builder),
        Uint128Concrete::LessThan(_) => super::uint::build_less_than(builder),
        Uint128Concrete::Equal(_) => misc::build_cell_eq(builder),
        Uint128Concrete::LessThanOrEqual(_) => super::uint::build_less_than_or_equal(builder),
    }
}

/// Handles a u128 overflowing add operation.
fn build_u128_overflowing_add(
    builder: CompiledInvocationBuilder<'_>,
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
            tempvar no_overflow;
            tempvar a_plus_b = a + b;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThan {lhs: a_plus_b, rhs: u128_limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Overflow:
            // Here we know that 2**128 <= a + b < 2 * (2**128 - 1).
            tempvar wrapping_a_plus_b = a_plus_b - u128_limit;
            assert wrapping_a_plus_b = *(range_check++);
            jump Target;
        NoOverflow:
            assert a_plus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_plus_b]], None),
            ("Target", &[&[range_check], &[wrapping_a_plus_b]], Some(failure_handle_statement_id)),
        ],
    ))
}

/// Handles a u128 overflowing sub operation.
fn build_u128_overflowing_sub(
    builder: CompiledInvocationBuilder<'_>,
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
            tempvar no_overflow;
            tempvar a_minus_b = a - b;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Underflow:
            // Here we know that 0 - (2**128 - 1) <= a - b < 0.
            tempvar wrapping_a_minus_b = a_minus_b + u128_limit;
            assert wrapping_a_minus_b = *(range_check++);
            jump Target;
        NoOverflow:
            assert a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_minus_b]], None),
            ("Target", &[&[range_check], &[wrapping_a_minus_b]], Some(failure_handle_statement_id)),
        ],
    ))
}

/// Handles a u128 divmod operation.
fn build_u128_divmod(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(3) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
            tempvar r_plus_1;
            tempvar b_minus_r_minus_1;
            tempvar q_is_small;
            tempvar b_or_q_bound_rc_value;
            tempvar bq;
            tempvar q;
            tempvar r;
            hint DivMod { lhs: a, rhs: b } into { quotient: q, remainder: r };
            // Both `q` and `r` must be uint128.
            // We must check `r` explicitly: we later check that `0 <= b - (r + 1)` and
            // `b * q + r = a`, however, if `r = -1` we may pass both of these checks (say, if
            // `b = a + 1` and `q = 1`).
            // We must also check `q` explicitly; the only arithmetic constraint on `q` is
            // `b * q + r = a`, and if `b = 2`, `a = 1` and `r = 0`, we can take `q` to be the
            // inverse of 2 (`(PRIME + 1) / 2`, much larger than 2^128) and pass this
            // constraint.
            assert q = *(range_check++);
            assert r = *(range_check++);
            // Verify `r < b` by constraining `0 <= b - (r + 1)`.
            const one = 1;
            assert r_plus_1 = r + one;
            assert b = b_minus_r_minus_1 + r_plus_1;
            assert b_minus_r_minus_1 = *(range_check++);
            // Verify `b * q + r = a`.
            // Since both `b` and `q` can be 2^128-1, we may overflow on `b * q`. To verify this
            // is not the case, use the fact that `b * q` must be less than 2^128. We know
            // `min(b, q)` must be less than 2^64. We guess which is less and verify.
            const u64_bound = u64::MAX as u128 + 1;
            hint TestLessThan {lhs: q, rhs: u64_bound} into {dst: q_is_small};
            const u128_bound_minus_u64_bound = u128::MAX - u64::MAX as u128;
            jump QIsSmall if q_is_small != 0;
            // `q >= 2^64`, so to verify `b < 2^64` we assert `2^128 - 2^64 + b` is in the range
            // check bound.
            assert b_or_q_bound_rc_value = b + u128_bound_minus_u64_bound;
            jump VerifyBQ;
        QIsSmall:
            // `q < 2^64`, compute `2^64 - q`.
            assert b_or_q_bound_rc_value = q + u128_bound_minus_u64_bound;
        VerifyBQ:
            // Now, b_or_q_bound_rc_value contains either `2^128 - 2^64 + q` or
            // `2^128 - 2^64 + b`. Verify this value is in [0, 2^128).
            assert b_or_q_bound_rc_value = *(range_check++);
            // Range validations done; verify `b * q + r = a` and that's it.
            assert bq = b * q;
            assert a = bq + r;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[q], &[r]], None)],
    ))
}

/// Handles a u128 overflowing widemul operation.
fn build_u128_widemul(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(8) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
        tempvar a0;
        tempvar a1;
        const u64_limit = u64::MAX as u128 + 1;
        // Break a into two 64bit halves s.t. a = a1 * 2**64 + a0.
        hint DivMod { lhs: a, rhs: u64_limit } into { quotient: a1, remainder: a0 };

        // Verify that a0 < 2**64 by constraining a0 + (2**128-1) - (2**64-1) < 2**128.
        const u64_upper_fixer = u128::MAX - u64::MAX as u128;
        tempvar fixed_a0 = a0 + u64_upper_fixer;
        assert fixed_a0 = *(range_check++);
        // Verify that a0, a1 are in [0, 2**128).
        assert a0 = *(range_check++);
        assert a1 = *(range_check++);
        // Overall, we now have a0 in [0, 2**64) and a1 in [0, 2**128).

        // Check the break: a = a1 * 2**64 + a0.
        // Note: `a` is uint128, the assertion will fail if a1 >= 2**64.
        tempvar a1_times_2_64 = a1 * u64_limit;
        assert a = a1_times_2_64 + a0;


        tempvar a0_b = a0 * b;
        tempvar a1_b = a1 * b;
        // An overview of the calculation to follow:
        // The final 256 bits result should equal a1_b * 2 ** 64 + a0_b, where the lower 128
        // bits are packed into `lower_uint128` and the higher bits go into `upper_uint128`.
        //
        // Since a0_b, a1_b are comprised of verified u64 * u128 => each fits within 192 bits.
        // * The lower 128 bits of a0_b should go into the resulting `lower_uint128` and the
        // upper 64 bits must carry over to the resulting `upper_uint128`.
        // * Let's mark `b = b1 * 2**64 + b0` (same split as in `a`). Then
        // a1_b = a1 * (b1 * 2**64 + b0) = a1b1 * (2**64) + a1b0. The bottom 64 bits of a1b0
        // (which are also the lower 64 bits of a1_b) should be summed into the 64-msbs of
        // `lower_uint128` and the remaining bits of a1b0 (which is u64*u64=>u128) as well
        // as a1b1*(2**64) should fit into `upper_uint128`.

        tempvar partial_upper_word;
        tempvar a1_b0_bottom;
        // Break a1_b into 128 and 64 bits parts, as explained above.
        hint DivMod { lhs: a1_b, rhs: u64_limit } into { quotient: partial_upper_word, remainder: a1_b0_bottom };

        // Verify that a1_b0_bottom is in [0, 2**64) and partial_upper_word in [0, 2**128).
        tempvar fixed_a1_b0_bottom = a1_b0_bottom + u64_upper_fixer;
        assert fixed_a1_b0_bottom = *(range_check++);
        assert a1_b0_bottom = *(range_check++);
        assert partial_upper_word = *(range_check++);
        // Check the break.
        tempvar partial_upper_word_times_2_64 = partial_upper_word * u64_limit;
        assert a1_b = partial_upper_word_times_2_64 + a1_b0_bottom;

        // Build the resulting two uint128 words from the calculated parts:
        tempvar shifted_a1_b0_bottom = a1_b0_bottom * u64_limit;
        tempvar carry;
        tempvar fixed_carry;
        tempvar shifted_carry;
        tempvar lower_uint128_with_carry;

        tempvar upper_uint128;
        tempvar lower_uint128;

        // Lower uint128 word:
        assert lower_uint128_with_carry = a0_b + shifted_a1_b0_bottom;
        // Note that `lower_uint128_with_carry` is bounded by 193 bits, as `a0_b` is capped
        // at 192 bits and `shifted_a1_b0_bottom` can contribute at most 1 additional bit,
        // added to (the carry of) `lower_uint128_with_carry = a0_b + shifted_a1_b0_bottom`.
        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        hint DivMod { lhs: lower_uint128_with_carry, rhs: u128_limit } into { quotient: carry, remainder: lower_uint128 };

        // Verify that `carry` is in [0, 2**65) and `lower_uint128` is in [0, 2**128).
        const carry_range_fixer = u128::MAX - (2u128.pow(65) - 1);
        assert fixed_carry = carry + carry_range_fixer;
        assert fixed_carry = *(range_check++);
        assert carry = *(range_check++);
        assert lower_uint128 = *(range_check++);
        // Verify the outputted `lower_uint128` and `carry` from the DivMod hint.
        assert shifted_carry = carry * u128_limit;
        assert lower_uint128_with_carry = shifted_carry + lower_uint128;
        // Note that reconstruction of the felt `lower_uint128_with_carry` is performed
        // with no wrap-around: `carry` was capped at 65 bits and then shifted 128 bits.
        // `lower_uint128` is range-checked for 128 bits. Overall, within 193 bits range.

        // Upper uint128 word:
        assert upper_uint128 = partial_upper_word + carry;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[upper_uint128], &[lower_uint128]], None)],
    ))
}

/// Handles a casting a felt into u128.
fn build_u128_from_felt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check_expression, expr_value] = builder.try_get_refs()?;
    let range_check = range_check_expression.try_unpack_single()?;
    let value = expr_value.try_unpack_single()?;

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let u128_bound: BigInt = BigInt::from(u128::MAX) + 1; // = 2**128.
    // Represent the maximal possible value (PRIME - 1) as 2**128 * max_x + max_y.
    let max_x: i128 = 10633823966279327296825105735305134080;
    let max_y: i128 = 0;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(3) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
            tempvar is_u128;
            const u128_limit = u128_bound.clone();
            hint TestLessThan { lhs: value, rhs: u128_limit } into { dst: is_u128 };
            jump NoOverflow if is_u128 != 0;
            // Allocating all values required so that `x` and `y` would be last.
            tempvar x_2_128;
            tempvar x_minus_max_x;
            tempvar rced_value;
            tempvar x;
            tempvar y;
            // Write value as 2**128 * x + y.
            hint DivMod { lhs: value, rhs: u128_limit } into { quotient: x, remainder: y };
            // Check x in [0, 2**128).
            assert x = *(range_check++);
            // Check y in [0, 2**128).
            assert y = *(range_check++);
            // Check that value = 2**128 * x + y (mod PRIME).
            assert x_2_128 = x * u128_limit;
            assert value = x_2_128 + y;
            // Check that there is no overflow in the computation of 2**128 * x + y.
            // Start by checking if x==max_x.
            const minus_max_x = -max_x;
            assert x_minus_max_x = x + minus_max_x;
            jump XNotMaxX if x_minus_max_x != 0;
            // If x == max_x, check that y <= max_y.
            const le_max_y_fix = (u128_bound.clone() - max_y - 1) as BigInt;
            assert rced_value = y + le_max_y_fix;
            jump WriteRcedValue;
        XNotMaxX:
            // If x != max_x, check that x < max_x.
            const lt_max_x_fix = (u128_bound - max_x) as BigInt;
            assert rced_value = x + lt_max_x_fix;
        WriteRcedValue:
            // In both cases, range-check the calculated value.
            assert rced_value = *(range_check++);
            // If x != 0, jump to the end.
            jump FailureHandle if x != 0;
        InfiniteLoop:
            // Otherwise, start an infinite loop.
            jump InfiniteLoop;
        NoOverflow:
            assert value = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("FailureHandle", &[&[range_check], &[x], &[y]], Some(failure_handle_statement_id)),
        ],
    ))
}
