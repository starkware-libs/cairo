use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::unsigned256::Uint256Concrete;
use num_bigint::BigInt;

use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CompiledInvocation,
    CompiledInvocationBuilder, CostValidationInfo, InvocationError,
};

/// Builds instructions for Sierra u256 operations.
pub fn build(
    libfunc: &Uint256Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint256Concrete::IsZero(_) => build_u256_is_zero(builder),
        Uint256Concrete::Divmod(_) => build_u256_divmod(builder),
        Uint256Concrete::SquareRoot(_) => build_u256_sqrt(builder),
    }
}

/// Generates casm instructions for `u256_is_zero()`.
fn build_u256_is_zero(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [x, y] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder, deref x; deref y; );
    casm_build_extend! {casm_builder,
        jump Target if x != 0;
        jump Target if y != 0;
    };

    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Target", &[&[x, y]], Some(target_statement_id))],
        Default::default(),
    ))
}

/// Generates casm instructions for `u256_safe_divmod()`.
fn build_u256_divmod(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, dividend, divisor] = builder.try_get_refs()?;
    let [range_check] = range_check.try_unpack()?;
    let [dividend0, dividend1] = dividend.try_unpack()?;
    let [divisor0, divisor1] = divisor.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(5) range_check;
        deref dividend0;
        deref dividend1;
        deref divisor0;
        deref divisor1;
    };

    casm_build_extend! {casm_builder,
        const zero = 0;
        const one = 1;
        const u128_bound_minus_u64_bound = u128::MAX - u64::MAX as u128;
        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        let orig_range_check = range_check;

        // Quotient 128-bit limbs.
        tempvar quotient0;
        tempvar quotient1;
        // Remainder 128-bit limbs.
        tempvar remainder0;
        tempvar remainder1;

        // Divide in a hint.
        hint Uint256DivMod {
            dividend0: dividend0,
            dividend1: dividend1,
            divisor0: divisor0,
            divisor1: divisor1
        } into {
            quotient0: quotient0,
            quotient1: quotient1,
            remainder0: remainder0,
            remainder1: remainder1
        };

        // Verify the hint ranges.
        assert quotient0 = *(range_check++);
        assert quotient1 = *(range_check++);
        assert remainder0 = *(range_check++);
        assert remainder1 = *(range_check++);

        // Assert remainder is less than divisor.
        tempvar diff1 = divisor1 - remainder1;
        tempvar diff0;
        tempvar diff0_min_1;
        jump HighDiff if diff1 != 0;
        assert diff0 = divisor0 - remainder0;
        assert diff0_min_1 = diff0 - one;
        assert diff0_min_1 = *(range_check++);
        jump After;
    HighDiff:
        // Align the branches.
        ap += 1;
        assert diff1 = *(range_check++);
    After:
    }
    // Do basic calculations.
    casm_build_extend! {casm_builder,
        tempvar q0d0_low;
        tempvar q0d0_high;
        hint WideMul128 { lhs: quotient0, rhs: divisor0 } into { low: q0d0_low, high: q0d0_high };
    }
    casm_build_extend! {casm_builder,
        // Validating `quotient * divisor + remainder - dividend = 0`.
        // Validate limb0.
        // Note that limb0 is a combination of 3 u128s, so its absolute value should be less than
        // 3 * 2**128.
        tempvar part0 = q0d0_low + remainder0;
        tempvar part1 = part0 - dividend0;
        // leftover is in range:
        // [(0 * 2 - u128::MAX) / u128_limit, (u128::MAX * 2 - 0) / u128_limit] ==> [0, 1].
        tempvar leftover = part1 / u128_limit;
        assert leftover = leftover * leftover;
        // Validate limb1.
        // We know that limb2 and limb3 should be 0.
        // Therfore quotient1 or divisor1 should also be 0.
        // We also know that quotient0*divisor1 and quotient1*divisor0 should be smaller than 2**128.
        // Therefore the smaller of each pair must be smaller than 2**64.
        // So by checking this we can avoid wraparound on the prime.
        tempvar qd1_small;
        tempvar qd1_large;
        jump DIVISOR1_EQ_ZERO if quotient1 != 0;
        // quotient1 is 0 - no need to multiply it by the divisor.
        tempvar quotient0_less_than_divisor1;
        hint TestLessThan { lhs: quotient0, rhs: divisor1 } into { dst: quotient0_less_than_divisor1 };
        jump QUOTIENT0_LESS_THAN_DIVISOR1 if quotient0_less_than_divisor1 != 0;
        assert qd1_small = divisor1;
        assert qd1_large = quotient0;
        jump MERGE;
    QUOTIENT0_LESS_THAN_DIVISOR1:
        assert qd1_small = quotient0;
        assert qd1_large = divisor1;
        jump MERGE;
    DIVISOR1_EQ_ZERO:
        // divisor1 is 0 - no need to multiply it by the quotient.
        assert divisor1 = zero;
        tempvar quotient1_less_than_divisor0;
        hint TestLessThan { lhs: quotient1, rhs: divisor0 } into { dst: quotient1_less_than_divisor0 };
        jump QUOTIENT1_LESS_THAN_DIVISOR0 if quotient1_less_than_divisor0 != 0;
        assert qd1_small = divisor0;
        assert qd1_large = quotient1;
        jump MERGE;
    QUOTIENT1_LESS_THAN_DIVISOR0:
        assert qd1_small = quotient1;
        assert qd1_large = divisor0;
    MERGE:
        tempvar qd1_small_fixed = qd1_small + u128_bound_minus_u64_bound;
        assert qd1_small_fixed = *(range_check++);
        tempvar qd1 = qd1_small * qd1_large;
        tempvar part0 = leftover + q0d0_high;
        tempvar part1 = part0 + remainder1;
        assert dividend1 = part1 + qd1;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(
            "Fallthrough",
            &[
                &[range_check],
                &[quotient0, quotient1],
                &[remainder0, remainder1],
                &[quotient0, divisor0, q0d0_high, q0d0_low],
            ],
            None,
        )],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Generates casm instructions for `u256_sqrt`.
fn build_u256_sqrt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_refs()?;
    let [range_check] = range_check.try_unpack()?;
    let [value_low, value_high] = value.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(6) range_check;
        deref value_low;
        deref value_high;
    };

    casm_build_extend! {casm_builder,
        const u64_limit = (BigInt::from(u64::MAX) + 1) as BigInt;
        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        const u128_bound_minus_u65_bound = BigInt::from(2).pow(128) - BigInt::from(2).pow(65);
        const u128_half = u128::MAX / 2 + 1;
        let orig_range_check = range_check;

        // Square root 64-bit limbs.
        tempvar sqrt0;
        tempvar sqrt1;
        // Remainder (value - sqrt**2) 128-bit limbs.
        tempvar remainder_low;
        tempvar remainder_high;

        tempvar sqrt_mul_2_minus_remainder_ge_u128;

        // Divide in a hint.
        hint Uint256SquareRoot {
            value_low: value_low,
            value_high: value_high
        } into {
            sqrt0: sqrt0,
            sqrt1: sqrt1,
            remainder_low: remainder_low,
            remainder_high: remainder_high,
            sqrt_mul_2_minus_remainder_ge_u128: sqrt_mul_2_minus_remainder_ge_u128
        };

        // Verify the hint consistency and ranges.
        // Assert range on sqrt limbs.
        // Note that for an honest prover we have 0 <= sqrt0, sqrt1 < 2**64.
        // But for soundness it is sufficient to check that:
        // 0 <= sqrt0, sqrt1 and sqrt0 + sqrt1 < 2**65,
        // which guarantees that 0 <= sqrt0, sqrt1 < 2**65.
        assert sqrt0 = *(range_check++);
        assert sqrt1 = *(range_check++);
        tempvar sqrt0_plus_sqrt1 = sqrt0 + sqrt1;
        tempvar a = sqrt0_plus_sqrt1 + u128_bound_minus_u65_bound;
        assert a = *(range_check++);

        // Assert range on remainder limbs.
        assert remainder_low = *(range_check++);
        // Assert that remainder_high is 0 or 1. Possible since remainder should be smaller than
        // 2 * sqrt, so it should be smaller than 2**129.
        assert remainder_high = remainder_high * remainder_high;

        // Square in parts.
        // Check that sqrt * sqrt + remainder - value = 0.
        // This can be rewritten by opening to limbs:
        //   2**0   * (sqrt0 * sqrt0 + remainder_low - value_low) +
        //   2**64  * (2 * sqrt0 * sqrt1) +
        //   2**128 * (sqrt1 * sqrt1 + remainder_high - value_high)

        // Limb0.
        // First accumulate the coefficients of 2**0. They are bounded by 2**128, so no overflow
        // can happen.
        tempvar element = sqrt0 * sqrt0;
        tempvar accum0 = remainder_low + element;
        tempvar accum1 = accum0 - value_low;
        // The result is in [-2**128, 2*2**128).
        // It should be divisible by 2**64, since all the parts not divisible by 2**64 were added,
        // and the end result should be 0.
        // Divide by 2**64 and check that we got an integer. This is the carry for the next
        // computation.
        tempvar accum2 = accum1 / u64_limit;
        tempvar temp = accum2 + u128_half;
        assert temp = *(range_check++);

        // The next limb computation is similar, only we also accumulate the carry from the previous
        // computations.
        // Limb1.
        tempvar element = sqrt0 * sqrt1;
        tempvar accum3 = accum2 + element;
        tempvar accum4 = accum3 + element;
        tempvar accum5 = accum4 / u64_limit;
        assert accum5 = *(range_check++);

        // The upper u128 word.
        tempvar accum6 = accum5 + remainder_high;
        tempvar element = sqrt1 * sqrt1;
        assert value_high = accum6 + element;

        // We have validated that value is larger than sqrt ** 2, since we validated that
        // `sqrt ** 2 + remainder = value` - and `remainder` is positive.
        // All that remains is to show that value is smaller than (sqrt + 1) ** 2.
        // It is enough to show that:
        // `(sqrt + 1) ** 2 - value > 0`
        // `sqrt ** 2 + 2 * sqrt + 1 - value > 0`
        // `2 * sqrt - (value - sqrt ** 2) + 1 > 0`
        // `2 * sqrt - remainder + 1 > 0`
        // `2 * sqrt - remainder >= 0`

        // Calculate the u128 representation of sqrt.
        tempvar shifted_sqrt1 = sqrt1 * u64_limit;
        tempvar sqrt = sqrt0 + shifted_sqrt1;

        // Making sure `2 * sqrt - remainder >= 0`.
        tempvar shifted_remainder_high = remainder_high * u128_limit;
        tempvar remainder = remainder_low + shifted_remainder_high;
        tempvar sqrt_mul_2 = sqrt + sqrt;
        tempvar sqrt_mul_2_minus_remainder = sqrt_mul_2 - remainder;
        tempvar fixed_sqrt_mul_2_minus_remainder;
        // Since we just want to make sure `2 * sqrt - remainder` is positive, we can trust the
        // hint to make sure that we are just in [0, 2**129) range.
        // We know it is in that range since `sqrt` is in [0, 2**128) so `2 * sqrt` is in
        // [0, 2**129).
        jump SqrtMul2MinusRemainderGeU128 if sqrt_mul_2_minus_remainder_ge_u128 != 0;
        assert sqrt_mul_2_minus_remainder = *(range_check++);
        jump Done;
        SqrtMul2MinusRemainderGeU128:
        assert fixed_sqrt_mul_2_minus_remainder = sqrt_mul_2_minus_remainder - u128_limit;
        assert fixed_sqrt_mul_2_minus_remainder = *(range_check++);
        Done:
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[sqrt]], None)],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
