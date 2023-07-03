use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::unsigned512::Uint512Concrete;
use num_bigint::BigInt;

use crate::invocations::{
    add_input_variables, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError,
};

/// Builds instructions for Sierra u512 operations.
pub fn build(
    libfunc: &Uint512Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint512Concrete::DivModU256(_) => build_u512_safe_divmod_by_u256(builder),
    }
}

/// Generates casm instructions for `u512_safe_divmod_by_u256()`.
fn build_u512_safe_divmod_by_u256(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, dividend, divisor] = builder.try_get_refs()?;
    let [range_check] = range_check.try_unpack()?;
    let [dividend0, dividend1, dividend2, dividend3] = dividend.try_unpack()?;
    let [divisor0, divisor1] = divisor.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(9) range_check;
        deref dividend0;
        deref dividend1;
        deref dividend2;
        deref dividend3;
        deref divisor0;
        deref divisor1;
    };

    casm_build_extend! {casm_builder,
        const zero = 0;
        const one = 1;
        const u128_bound_minus_4 = u128::MAX - 3;
        const u128_bound_minus_u64_bound = u128::MAX - u64::MAX as u128;
        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        let orig_range_check = range_check;

        // Quotient 128-bit limbs.
        tempvar quotient0;
        tempvar quotient1;
        tempvar quotient2;
        tempvar quotient3;
        // Remainder 128-bit limbs.
        tempvar remainder0;
        tempvar remainder1;

        // Divide in a hint.
        hint Uint512DivModByUint256 {
            dividend0: dividend0,
            dividend1: dividend1,
            dividend2: dividend2,
            dividend3: dividend3,
            divisor0: divisor0,
            divisor1: divisor1
        } into {
            quotient0: quotient0,
            quotient1: quotient1,
            quotient2: quotient2,
            quotient3: quotient3,
            remainder0: remainder0,
            remainder1: remainder1
        };

        // Verify the hint ranges.
        assert quotient0 = *(range_check++);
        assert quotient1 = *(range_check++);
        assert quotient2 = *(range_check++);
        assert quotient3 = *(range_check++);
        assert remainder0 = *(range_check++);
        assert remainder1 = *(range_check++);

        // Assert remainder is less than divisor.
        tempvar diff1 = divisor1 - remainder1;

        // Allocate memory cells for the hints,
        // as well as for the memory used by just one branch.
        ap += 12;
        tempvar diff0;
        tempvar diff0_min_1;

        jump HighDiff if diff1 != 0;
        assert diff0 = divisor0 - remainder0;
        assert diff0_min_1 = diff0 - one;
        assert diff0_min_1 = *(range_check++);
        jump After;
    HighDiff:
        assert diff1 = *(range_check++);
    After:
    }
    // Do basic calculations.
    casm_build_extend! {casm_builder,
        tempvar q0d0_low;
        tempvar q0d0_high;
        hint WideMul128 { lhs: quotient0, rhs: divisor0 } into { low: q0d0_low, high: q0d0_high };
        tempvar q1d0_low;
        tempvar q1d0_high;
        hint WideMul128 { lhs: quotient1, rhs: divisor0 } into { low: q1d0_low, high: q1d0_high };
        tempvar q0d1_low;
        tempvar q0d1_high;
        hint WideMul128 { lhs: quotient0, rhs: divisor1 } into { low: q0d1_low, high: q0d1_high };
        tempvar q1d1_low;
        tempvar q1d1_high;
        hint WideMul128 { lhs: quotient1, rhs: divisor1 } into { low: q1d1_low, high: q1d1_high };
        tempvar q2d0_low;
        tempvar q2d0_high;
        hint WideMul128 { lhs: quotient2, rhs: divisor0 } into { low: q2d0_low, high: q2d0_high };
    }
    casm_build_extend! {casm_builder,
        // Validating `quotient * divisor + remainder - dividend = 0`.
        // Validate limb0.
        tempvar part0 = q0d0_low + remainder0;
        tempvar part1 = part0 - dividend0;
        tempvar leftover = part1 / u128_limit;
        // leftover is an integer in range:
        // [(0 * 2 - u128::MAX) / u128_limit, (u128::MAX * 2 - 0) / u128_limit] ==> [0, 1].
        assert leftover = leftover * leftover;
        // Validate limb1.
        tempvar part0 = leftover + q0d0_high;
        tempvar part1 = part0 + q1d0_low;
        tempvar part2 = part1 + q0d1_low;
        tempvar part3 = part2 + remainder1;
        tempvar part4 = part3 - dividend1;
        tempvar leftover = part4 / u128_limit;
        // leftover is an integer in range:
        // [(0 + 0 * 4 - u128::MAX) / u128_limit, (1 + u128::MAX * 4 - 0) / u128_limit] ==> [0, 3].
        assert leftover = *(range_check++);
        tempvar a = leftover + u128_bound_minus_4;
        assert a = *(range_check++);
        // Validate limb2.
        tempvar part0 = leftover + q1d0_high;
        tempvar part1 = part0 + q0d1_high;
        tempvar part2 = part1 + q1d1_low;
        tempvar part3 = part2 + q2d0_low;
        tempvar part4 = part3 - dividend2;
        tempvar leftover = part4 / u128_limit;
        // leftover is an integer in range:
        // [(0 + 0 * 4 - u128::MAX) / u128_limit, (3 + u128::MAX * 4 - 0) / u128_limit] ==> [0, 3].
        assert leftover = *(range_check++);
        tempvar a = leftover + u128_bound_minus_4;
        assert a = *(range_check++);
        // Validate limb3.
        // Because quotient * divisor + remainder = dividend < 2**512,
        // either quotient3 or divisor1 should be 0.
        // We also know that quotient2 * divisor1 and quotient3 * divisor0 should be smaller
        // than 2**128.
        // Therefore the smaller value from each pair must be smaller than 2**64.
        // Note that the other pair is zero.
        // So by checking this we can avoid wraparound on the prime.
        tempvar qd3_small;
        tempvar qd3_large;
        jump DIVISOR1_EQ_ZERO if quotient3 != 0;
        // quotient3 is 0 - no need to multiply it by the divisor.
        tempvar quotient2_less_than_divisor1;
        hint TestLessThan { lhs: quotient2, rhs: divisor1 } into { dst: quotient2_less_than_divisor1 };
        jump QUOTIENT2_LESS_THAN_DIVISOR1 if quotient2_less_than_divisor1 != 0;
        assert qd3_small = divisor1;
        assert qd3_large = quotient2;
        jump MERGE;
    QUOTIENT2_LESS_THAN_DIVISOR1:
        assert qd3_small = quotient2;
        assert qd3_large = divisor1;
        jump MERGE;
    DIVISOR1_EQ_ZERO:
        // divisor1 is 0 - no need to multiply it by the quotient.
        assert divisor1 = zero;
        tempvar quotient3_less_than_divisor0;
        hint TestLessThan { lhs: quotient3, rhs: divisor0 } into { dst: quotient3_less_than_divisor0 };
        jump QUOTIENT3_LESS_THAN_DIVISOR0 if quotient3_less_than_divisor0 != 0;
        assert qd3_small = divisor0;
        assert qd3_large = quotient3;
        jump MERGE;
    QUOTIENT3_LESS_THAN_DIVISOR0:
        assert qd3_small = quotient3;
        assert qd3_large = divisor0;
    MERGE:
        tempvar qd3_small_fixed = qd3_small + u128_bound_minus_u64_bound;
        assert qd3_small_fixed = *(range_check++);
        tempvar qd3 = qd3_small * qd3_large;
        tempvar part0 = leftover + q2d0_high;
        tempvar part1 = part0 + q1d1_high;
        assert dividend3 = part1 + qd3;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(
            "Fallthrough",
            &[
                &[range_check],
                &[quotient0, quotient1, quotient2, quotient3],
                &[remainder0, remainder1],
                &[quotient0, divisor0, q0d0_high, q0d0_low],
                &[quotient0, divisor1, q0d1_high, q0d1_low],
                &[quotient1, divisor0, q1d0_high, q1d0_low],
                &[quotient1, divisor1, q1d1_high, q1d1_low],
                &[quotient2, divisor0, q2d0_high, q2d0_low],
            ],
            None,
        )],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
