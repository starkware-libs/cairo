use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::uint256::Uint256Concrete;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

/// Builds instructions for Sierra u256 operations.
pub fn build(
    libfunc: &Uint256Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint256Concrete::IsZero(_) => build_u256_is_zero(builder),
        Uint256Concrete::Divmod(_) => build_u256_divmod(builder),
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
        const u64_limit = (BigInt::from(u64::MAX) + 1) as BigInt;
        const u128_bound_minus_u64_bound = u128::MAX - u64::MAX as u128;
        let orig_range_check = range_check;

        tempvar remainder_low;
        tempvar remainder_high;
        tempvar quotient0;
        tempvar quotient1;
        tempvar divisor0;
        tempvar divisor1;
        tempvar extra0;
        tempvar extra1;

        // Allocate tempvars for output.
        tempvar quotient_low;
        tempvar quotient_high;

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

        // Check consistency.
        tempvar shifted_divisor1 = divisor1 * u64_limit;
        assert divisor_low = divisor0 + shifted_divisor1;

        // Push quotient_low for output.
        tempvar shifted_quotient1 = quotient1 * u64_limit;
        assert quotient_low = quotient0 + shifted_quotient1;

        // Multiply in parts.
        // Limb0.
        const zero_const = 0;
        tempvar zero = zero_const;
        tempvar element = divisor0 * quotient0;
        tempvar accum0 = remainder_low + element;
        tempvar accum1 = accum0 - dividend_low;
        tempvar accum2 = accum1 / u64_limit;
        assert accum2 = *(range_check++);

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
        jump DivisorIsSmall if divisor_high != 0;

        // Here, quotient is small.
        let quotient2 = extra0;
        let quotient3 = extra1;

        // Limb2 cont.
        tempvar element = divisor0 * quotient2;
        tempvar accum9 = accum8 + element;
        tempvar accuma = accum9 / u64_limit;
        assert accuma = *(range_check++);

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

        DivisorIsSmall:
        let divisor2 = extra0;
        let divisor3 = extra1;

        // Check consistency.
        tempvar shifted_divisor3 = divisor3 * u64_limit;
        assert divisor_high = divisor2 + shifted_divisor3;

        // Limb2 cont.
        tempvar element = divisor2 * quotient0;
        tempvar accum9 = accum8 + element;
        tempvar accuma = accum9 / u64_limit;
        assert accuma = *(range_check++);

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
