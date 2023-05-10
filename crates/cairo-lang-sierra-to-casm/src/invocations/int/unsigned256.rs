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
    let [dividend_low, dividend_high] = dividend.try_unpack()?;
    let [divisor_low, divisor_high] = divisor.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(16) range_check;
        deref dividend_low;
        deref dividend_high;
        deref divisor_low;
        deref divisor_high;
    };

    casm_build_extend! {casm_builder,
        const one = 1;
        const u64_limit = (BigInt::from(u64::MAX) + 1) as BigInt;
        const u128_bound_minus_u64_bound = u128::MAX - u64::MAX as u128;
        const u128_half = u128::MAX / 2 + 1;
        let orig_range_check = range_check;

        // Remainder 128-bit limbs.
        tempvar remainder_low;
        tempvar remainder_high;
        // Quotient 128-bit limbs. These are needed for the output.
        tempvar quotient_low;
        tempvar quotient_high;
        // quotient_low 64-bit limbs.
        tempvar quotient0;
        tempvar quotient1;
        // divisor_low 64-bit limbs.
        tempvar divisor0;
        tempvar divisor1;
        // 64-bit limbs for either quotient_high or divisor_high - since only once of them can
        // be non-zero.
        tempvar extra0;
        tempvar extra1;

        // Divide in a hint.
        hint Uint256DivMod {
            dividend_low: dividend_low,
            dividend_high: dividend_high,
            divisor_low: divisor_low,
            divisor_high: divisor_high
        } into {
            quotient0:quotient0,
            quotient1:quotient1,
            divisor0:divisor0,
            divisor1:divisor1,
            extra0:extra0,
            extra1:extra1,
            remainder_low:remainder_low,
            remainder_high:remainder_high
        };

        // Verify the hint consistency and ranges.
        // Assert range on quotient limbs.
        assert quotient0 = *(range_check++);
        tempvar a = quotient0 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);
        assert quotient1 = *(range_check++);
        tempvar a = quotient1 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);

        // Assert range on divisor limbs.
        assert divisor0 = *(range_check++);
        tempvar a = divisor0 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);
        assert divisor1 = *(range_check++);
        tempvar a = divisor1 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);

        // Assert range on extra limbs.
        assert extra0 = *(range_check++);
        tempvar a = extra0 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);
        assert extra1 = *(range_check++);
        tempvar a = extra1 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);

        // Assert range on remainder limbs.
        assert remainder_low = *(range_check++);
        assert remainder_high = *(range_check++);

        // Assert remainder is less than divisor.
        tempvar high_diff = divisor_high - remainder_high;
        tempvar low_diff = divisor_low - remainder_low;
        tempvar low_diff_min_1 = low_diff - one;
        jump HighDiff if high_diff != 0;
        assert low_diff_min_1 = *(range_check++);
        jump After;
        HighDiff:
        assert high_diff = *(range_check++);
        After:

        // Check consistency for divisor and quotient.
        tempvar shifted_divisor1 = divisor1 * u64_limit;
        assert divisor_low = divisor0 + shifted_divisor1;
        tempvar shifted_quotient1 = quotient1 * u64_limit;
        assert quotient_low = quotient0 + shifted_quotient1;

        // Multiply in parts.
        // Check that divisor * quotient + remainder - dividend = 0.
        // This can be rewritten by opening to limbs:
        //   2**0   * (divisor0 * quotient0 + remainder_low - dividend_low) +
        //   2**64  * (divisor1 * quotient0 + divisor0 * quotient1) +
        //   2**128 * (divisor1 * quotient1 + remainder_high - dividend_high + ...) +
        //   2**192 * (...) = 0

        // Limb0.
        // First accumulate the coefficients of 2**0. They are bounded by 2**128, so no overflow
        // can happen.
        const zero_const = 0;
        tempvar zero = zero_const;
        tempvar element = divisor0 * quotient0;
        tempvar accum0 = remainder_low + element;
        tempvar accum1 = accum0 - dividend_low;
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
        tempvar element = divisor1 * quotient0;
        tempvar accum3 = accum2 + element;
        tempvar element = divisor0 * quotient1;
        tempvar accum4 = accum3 + element;
        tempvar accum5 = accum4 / u64_limit;
        assert accum5 = *(range_check++);

        // Limb2.
        tempvar accum6 = accum5 + remainder_high;
        tempvar accum7 = accum6 - dividend_high;
        tempvar element = divisor1 * quotient1;
        tempvar accum8 = accum7 + element;
    }

    casm_build_extend! {casm_builder,
        jump DivisorIsLarge if divisor_high != 0;

        // Here, quotient is large.
        let quotient2 = extra0;
        let quotient3 = extra1;

        // Limb2 cont.
        tempvar element = divisor0 * quotient2;
        tempvar accum9 = accum8 + element;
        tempvar accuma = accum9 / u64_limit;
        tempvar temp = accuma + u128_half;
        assert temp = *(range_check++);

        // Limb3.
        tempvar element = divisor1 * quotient2;
        tempvar accumb = accuma + element;
        tempvar element = divisor0 * quotient3;
        let accumc = accumb + element;
        assert zero = accumc;

        // Check that the high part is zero.
        assert zero = divisor1 * quotient3;

        // Push quotient_high for output.
        tempvar shifted_quotient3 = quotient3 * u64_limit;
        assert quotient_high = quotient2 + shifted_quotient3;
        jump End;

        DivisorIsLarge:
        let divisor2 = extra0;
        let divisor3 = extra1;

        // Check consistency.
        tempvar shifted_divisor3 = divisor3 * u64_limit;
        assert divisor_high = divisor2 + shifted_divisor3;

        // Limb2 cont.
        tempvar element = divisor2 * quotient0;
        tempvar accum9 = accum8 + element;
        tempvar accuma = accum9 / u64_limit;
        tempvar temp = accuma + u128_half;
        assert temp = *(range_check++);

        // Limb3.
        tempvar element = divisor2 * quotient1;
        tempvar accumb = accuma + element;
        tempvar element = divisor3 * quotient0;
        let accumc = accumb + element;
        assert zero = accumc;

        // Check that the high part is zero.
        assert zero = divisor3 * quotient1;

        // Push quotient_high for output.
        assert quotient_high = zero;

        End:
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(
            "Fallthrough",
            &[&[range_check], &[quotient_low, quotient_high], &[remainder_low, remainder_high]],
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
        buffer(16) range_check;
        deref value_low;
        deref value_high;
    };

    casm_build_extend! {casm_builder,
        const u64_limit = (BigInt::from(u64::MAX) + 1) as BigInt;
        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        const u128_bound_minus_u64_bound = u128::MAX - u64::MAX as u128;
        const u128_half = u128::MAX / 2 + 1;
        const zero = 0;
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
        assert sqrt0 = *(range_check++);
        tempvar a = sqrt0 + u128_bound_minus_u64_bound;
        assert a = *(range_check++);
        assert sqrt1 = *(range_check++);
        tempvar a = sqrt1 + u128_bound_minus_u64_bound;
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
        tempvar accum7 = accum6 - value_high;
        tempvar element = sqrt1 * sqrt1;
        tempvar accum8 = accum7 + element;
        assert accum8 = zero;

        // We have validated that value is larger than root ** 2, since we validated that
        // `root ** 2 + remainder = value` - and `remainder` is positive.
        // All that remains is to show that value is smaller than (root + 1) ** 2.
        // It is enough to show that:
        // `(root + 1) ** 2 - value > 0`
        // `root ** 2 + 2 * root + 1 - value > 0`
        // `2 * root - (value - root ** 2) + 1 > 0`
        // `2 * root - remainder + 1 > 0`
        // `2 * root - remainder >= 0`

        // Calculate the u128 representation of sqrt.
        tempvar shifted_sqrt1 = sqrt1 * u64_limit;
        tempvar sqrt = sqrt0 + shifted_sqrt1;

        // Making sure `2 * root - remainder >= 0`.
        tempvar shifted_remainder_high = remainder_high * u128_limit;
        tempvar remainder = remainder_low + shifted_remainder_high;
        tempvar sqrt_mul_2 = sqrt + sqrt;
        tempvar sqrt_mul_2_minus_remainder = sqrt_mul_2 - remainder;
        tempvar fixed_sqrt_mul_2_minus_remainder;
        // Since we just want to make sure `2 * root - remainder` is positive, we can trust the
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
