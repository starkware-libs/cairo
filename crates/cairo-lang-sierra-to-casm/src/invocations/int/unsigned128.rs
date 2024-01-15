use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::unsigned128::Uint128Concrete;
use cairo_lang_sierra::extensions::int::IntOperator;
use num_bigint::BigInt;
use num_traits::{Num, One};

use super::{build_128bit_diff, build_const};
use crate::invocations::{
    add_input_variables, bitwise, get_non_fallthrough_statement_id, misc, CompiledInvocation,
    CompiledInvocationBuilder, CostValidationInfo, InvocationError,
};

/// Builds instructions for Sierra u128 operations.
pub fn build(
    libfunc: &Uint128Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint128Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => build_u128_overflowing_add(builder),
            IntOperator::OverflowingSub => build_128bit_diff(builder),
        },
        Uint128Concrete::Divmod(_) => build_u128_divmod(builder),
        Uint128Concrete::GuaranteeMul(_) => build_u128_guarantee_mul(builder),
        Uint128Concrete::MulGuaranteeVerify(_) => build_u128_mul_guarantee_verify(builder),
        Uint128Concrete::IsZero(_) => misc::build_is_zero(builder),
        Uint128Concrete::Const(libfunc) => build_const(libfunc, builder),
        Uint128Concrete::FromFelt252(_) => build_u128_from_felt252(builder),
        Uint128Concrete::ToFelt252(_) => misc::build_identity(builder),
        Uint128Concrete::Equal(_) => misc::build_cell_eq(builder),
        Uint128Concrete::SquareRoot(_) => super::unsigned::build_sqrt(builder),
        Uint128Concrete::ByteReverse(_) => build_u128_byte_reverse(builder),
        Uint128Concrete::Bitwise(_) => bitwise::build(builder),
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
            let orig_range_check = range_check;
            tempvar no_overflow;
            tempvar a_plus_b = a + b;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThan {lhs: a_plus_b, rhs: u128_limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Overflow:
            // Here we know that 2**128 <= a + b < 2 * 2**128 - 1.
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
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
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
            let orig_range_check = range_check;
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
            // `q < 2^64`, so to verify `q < 2^64` we assert `2^128 - 2^64 + q` is in the range
            // check bound.
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
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Builds the `u128_guarantee_mul` libfunc.
fn build_u128_guarantee_mul(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
        tempvar res_high;
        tempvar res_low;
        hint WideMul128 { lhs: a, rhs: b } into { high: res_high, low: res_low };
        ap += 2;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[res_high], &[res_low], &[a, b, res_high, res_low]], None)],
        Default::default(),
    ))
}

/// Builds the `u128_mul_guarantee_verify` libfunc.
fn build_u128_mul_guarantee_verify(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // The inputs are 4 u128 numbers `a, b, res_high, res_low` for which we need to
    // verify that:
    //   `a * b = 2**128 * res_high + res_low`.
    let [range_check_ref, guarantee] = builder.try_get_refs()?;
    let [range_check] = range_check_ref.try_unpack()?;
    let [a, b, res_high, res_low] = guarantee.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(8) range_check;
        deref a;
        deref b;
        deref res_high;
        deref res_low;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
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
        // bits are packed into `res_low` and the higher bits go into `res_high`.
        //
        // Since a0_b, a1_b are comprised of verified u64 * u128 => each fits within 192 bits.
        // * The lower 128 bits of a0_b should go into the resulting `res_low` and the
        // upper 64 bits must carry over to the resulting `res_high`.
        // * Let's mark `b = b1 * 2**64 + b0` (same split as in `a`). Then
        // a1_b = a1 * (b1 * 2**64 + b0) = a1b1 * (2**64) + a1b0. The bottom 64 bits of a1b0
        // (which are also the lower 64 bits of a1_b) should be summed into the 64-msbs of
        // `res_low` and the remaining bits of a1b0 (which is u64*u64=>u128) as well
        // as a1b1*(2**64) should fit into `res_high`.

        tempvar partial_upper_word;
        tempvar a1_b0_bottom;
        // Break a1_b into 128 and 64 bits parts, as explained above.
        hint DivMod {
            lhs: a1_b,
            rhs: u64_limit
        } into { quotient: partial_upper_word, remainder: a1_b0_bottom };

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

        // Lower uint128 word:
        assert lower_uint128_with_carry = a0_b + shifted_a1_b0_bottom;
        // Note that `lower_uint128_with_carry` is bounded by 193 bits, as `a0_b` is capped
        // at 192 bits and `shifted_a1_b0_bottom` can contribute at most 1 additional bit,
        // added to (the carry of) `lower_uint128_with_carry = a0_b + shifted_a1_b0_bottom`.
        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        hint DivMod {
            lhs: lower_uint128_with_carry,
            rhs: u128_limit
        } into { quotient: carry, remainder: res_low };

        // Verify that `carry` is in [0, 2**65) and `res_low` is in [0, 2**128).
        const carry_range_fixer = u128::MAX - (2u128.pow(65) - 1);
        assert fixed_carry = carry + carry_range_fixer;
        assert fixed_carry = *(range_check++);
        assert carry = *(range_check++);
        assert res_low = *(range_check++);
        // Verify the outputted `res_low` and `carry` from the DivMod hint.
        assert shifted_carry = carry * u128_limit;
        assert lower_uint128_with_carry = shifted_carry + res_low;
        // Note that reconstruction of the felt252 `lower_uint128_with_carry` is performed
        // with no wrap-around: `carry` was capped at 65 bits and then shifted 128 bits.
        // `res_low` is range-checked for 128 bits. Overall, within 193 bits range.

        // Upper uint128 word:
        assert res_high = partial_upper_word + carry;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check]], None)],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a casting a felt252 into u128.
fn build_u128_from_felt252(
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
            let orig_range_check = range_check;
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
            // Otherwise, fail. As `x` must be non-zero in the overflow case, this is unreachable.
            fail;
        NoOverflow:
            assert value = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("FailureHandle", &[&[range_check], &[x], &[y]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
/// Handles instruction for reversing the bytes of a u128.
pub fn build_u128_byte_reverse(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [bitwise, input] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref input;
        buffer(20) bitwise;
    };

    let masks = [
        0x00ff00ff00ff00ff00ff00ff00ff00ff_u128,
        0x00ffff0000ffff0000ffff0000ffff00_u128,
        0x00ffffffff00000000ffffffff000000_u128,
        0x00ffffffffffffffff00000000000000_u128,
    ];

    // The algorithm works in steps. Generally speaking, on the i-th step,
    // we switch between every two consecutive sequences of 2 ** i bytes.
    // To illustrate how it works, here are the steps when running
    // on a 64-bit word = [b0, b1, b2, b3, b4, b5, b6, b7].
    //
    // step 1:
    // [b0, b1, b2, b3, b4, b5, b6, b7] -
    // [b0, 0,  b2, 0,  b4, 0,  b6, 0 ] +
    // [0,  0,  b0, 0,  b2, 0,  b4, 0,  b6] =
    // [0,  b1, b0, b3, b2, b5, b4, b7, b6]
    //
    // step 2:
    // [0, b1, b0, b3, b2, b5, b4, b7, b6] -
    // [0, b1, b0, 0,  0,  b5, b4, 0,  0 ] +
    // [0, 0,  0,  0,  0,  b1, b0, 0,  0,  b5, b4] =
    // [0, 0,  0,  b3, b2, b1, b0, b7, b6, b5, b4]
    //
    // step 3:
    // [0, 0, 0, b3, b2, b1, b0, b7, b6, b5, b4] -
    // [0, 0, 0, b3, b2, b1, b0, 0,  0,  0,  0 ] +
    // [0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  b3, b2, b1, b0] =
    // [0, 0, 0, 0,  0,  0,  0,  b7, b6, b5, b4, b3, b2, b1, b0]
    //
    // Next, we divide by 2 ** (8 + 16 + 32) and get [b7, b6, b5, b4, b3, b2, b1, b0].
    let mut temp = input;
    let mut shift = BigInt::from(1 << 16);
    for mask_imm in masks.into_iter() {
        let shift_imm = &shift - BigInt::one();
        casm_build_extend! {casm_builder,
            assert temp = *(bitwise++);
            const mask_imm = mask_imm;
            const shift_imm = shift_imm;
            tempvar mask = mask_imm;
            assert mask = *(bitwise++);
            tempvar and = *(bitwise++);
            let _xor = *(bitwise++);
            let _or = *(bitwise++);

            tempvar shifted_var = and * shift_imm;
            tempvar x = temp + shifted_var;
        };

        shift = &shift * &shift;
        temp = x;
    }

    // Now rather than swapping the high and low 64 bits, we split them into
    // two 64-bit words.

    // Right align the value.
    casm_build_extend! {casm_builder,
        // The inverse of `2**(8 + 16 + 32 + 64)` in the field.
        const shift_inverse =
            -BigInt::from_str_radix("800000000000011000000000000000000", 16).unwrap();
        let result = temp * shift_inverse;
    }

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[bitwise], &[result]], None)],
        Default::default(),
    ))
}
