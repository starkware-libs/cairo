use cairo_casm::builder::CasmBuilder;
use cairo_casm::casm_build_extend;
use cairo_casm::operand::{CellRef, ResOperand};
use cairo_sierra::extensions::uint128::{
    IntOperator, Uint128Concrete, Uint128OperationConcreteLibFunc,
};
use num_bigint::BigInt;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{CellExpression, ReferenceExpression};

#[cfg(test)]
#[path = "uint128_test.rs"]
mod test;

/// Builds instructions for Sierra u128 operations.
pub fn build(
    libfunc: &Uint128Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc { operator, .. }) => {
            build_u128_op(builder, *operator)
        }
        Uint128Concrete::JumpNotZero(_) => misc::build_jump_nz(builder),
        Uint128Concrete::Const(libfunc) => Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Immediate(BigInt::from(libfunc.c)))]
                .into_iter(),
        )),
        Uint128Concrete::FromFelt(_) => build_u128_from_felt(builder),
        Uint128Concrete::ToFelt(_) => misc::build_identity(builder),
        Uint128Concrete::LessThan(_) => build_u128_lt(builder),
        Uint128Concrete::Equal(_) => build_u128_eq(builder),
        Uint128Concrete::LessThanOrEqual(_) => build_u128_le(builder),
    }
}

/// Fetches, verifies and returns the range check, a and b references.
pub fn unwrap_range_check_based_binary_op_refs(
    builder: &CompiledInvocationBuilder<'_>,
) -> Result<(ResOperand, CellRef, CellRef), InvocationError> {
    let [range_check_expression, expr_a, expr_b] = builder.try_get_refs()?;
    Ok((
        range_check_expression.try_unpack_single()?.to_buffer(0)?,
        expr_a.try_unpack_single()?.to_deref()?,
        expr_b.try_unpack_single()?.to_deref()?,
    ))
}

/// Handles a u128 operation with the given op.
fn build_u128_op(
    builder: CompiledInvocationBuilder<'_>,
    op: IntOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    match op {
        IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
            let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
            let mut casm_builder = CasmBuilder::default();
            let range_check = casm_builder.add_var(range_check);
            let a = casm_builder.add_var(ResOperand::Deref(a));
            let b = casm_builder.add_var(ResOperand::Deref(b));
            let (possible_overflow, overflow_fixed) = match op {
                IntOperator::OverflowingAdd => {
                    casm_build_extend! {casm_builder,
                        tempvar no_overflow;
                        tempvar a_plus_b = a + b;
                        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
                        hint TestLessThan {lhs: a_plus_b, rhs: u128_limit} into {dst: no_overflow};
                        jump NoOverflow if no_overflow != 0;
                        // Overflow:
                        // Here we know that 2**128 <= a + b < 2 * (2**128 - 1).
                        tempvar wrapping_a_plus_b = a_plus_b - u128_limit;
                    };
                    (a_plus_b, wrapping_a_plus_b)
                }
                IntOperator::OverflowingSub => {
                    casm_build_extend! {casm_builder,
                        tempvar no_overflow;
                        tempvar a_minus_b = a - b;
                        const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
                        hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: no_overflow};
                        jump NoOverflow if no_overflow != 0;
                        // Underflow:
                        // Here we know that 0 - (2**128 - 1) <= a - b < 0.
                        tempvar wrapping_a_minus_b = a_minus_b + u128_limit;
                    };
                    (a_minus_b, wrapping_a_minus_b)
                }
                _ => unreachable!("Only supported options in arm."),
            };
            casm_build_extend! {casm_builder,
                    assert overflow_fixed = *(range_check++);
                    jump Target;
                NoOverflow:
                    assert possible_overflow = *(range_check++);
            };
            Ok(builder.build_from_casm_builder(
                casm_builder,
                [
                    ("Fallthrough", &[&[range_check], &[possible_overflow]], None),
                    (
                        "Target",
                        &[&[range_check], &[overflow_fixed]],
                        Some(failure_handle_statement_id),
                    ),
                ],
            ))
        }
        IntOperator::DivMod => {
            let mut casm_builder = CasmBuilder::default();
            let range_check = casm_builder.add_var(range_check);
            let a = casm_builder.add_var(ResOperand::Deref(a));
            let b = casm_builder.add_var(ResOperand::Deref(b));
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
        IntOperator::WideMul => {
            let mut casm_builder = CasmBuilder::default();
            let range_check = casm_builder.add_var(range_check);
            let a = casm_builder.add_var(ResOperand::Deref(a));
            let b = casm_builder.add_var(ResOperand::Deref(b));

            casm_build_extend! {casm_builder,
                tempvar a0;
                tempvar a1;
                tempvar b0;
                tempvar b1;
                const u64_limit = u64::MAX as u128 + 1;

                // Breaks a and b to 64bit halves:
                hint DivMod { lhs: a, rhs: u64_limit } into { quotient: a1, remainder: a0 }; // a = a1 * 2**64 + a0.
                hint DivMod { lhs: b, rhs: u64_limit } into { quotient: b1, remainder: b0 }; // b = b1 * 2**64 + b0.

                // Verify that a0, a1, b0, b1 in [0, 2**128).
                assert a0 = *(range_check++);
                assert a1 = *(range_check++);
                assert b0 = *(range_check++);
                assert b1 = *(range_check++);

                // Verify that a0, b0 < 2**64 by constraining {var} + (2**128-1) - (2**64-1) < 2**128.
                const u64_upper_fixer = u128::MAX - u64::MAX as u128;
                tempvar fixed_a0 = a0 + u64_upper_fixer;
                tempvar fixed_b0 = b0 + u64_upper_fixer;
                assert fixed_a0 = *(range_check++);
                assert fixed_b0 = *(range_check++);

                // Check the a,b break:
                // a = a1 * 2**64 + a0.
                // Note: `a` is uint128, the assertion will fail if a1 >= 2**64.
                tempvar a1_times_2_64 = a1 * u64_limit;
                assert a = a1_times_2_64 + a0;
                // b = b1 * 2**64 + b0.
                // Note: `b` is uint128, the assertion will fail if b1 >= 2**64.
                tempvar b1_times_2_64 = b1 * u64_limit;
                assert b = b1_times_2_64 + b0;

                // These four ai_bi are each comprised verified u64 * u64 => within u128.
                tempvar a0_b0 = a0 * b0;
                tempvar a0_b1 = a0 * b1;
                tempvar a1_b0 = a1 * b0;
                tempvar a1_b1 = a1 * b1;

                // Build the resulting two uint128 words from the calculated parts:
                tempvar a0_b1_plus_a1_b0 = a0_b1 + a1_b0;
                tempvar shifted_a0_b1_plus_a1_b0 = a0_b1_plus_a1_b0 * u64_limit;

                tempvar lower_uint128_with_carry;
                tempvar carry;
                tempvar fixed_carry;
                tempvar shifted_carry;

                tempvar upper_uint128;
                tempvar lower_uint128;

                // Lower uint128 word:
                assert lower_uint128_with_carry = shifted_a0_b1_plus_a1_b0 + a0_b0;
                // Note that `lower_uint128_with_carry` is always bounded by 194 bits since:
                // * `a0_b0`, `a0_b1` and `a1_b0` are each capped by 128 bits.
                // * `a0_b1 + a1_b0` is capped by 129 bits => `shifted_a0_b1_plus_a1_b0` is capped
                //    by 129+64=193 bits.
                // * `a0_b0` can contribute at most 1 additional bit, added to (the carry of)
                //   `lower_uint128_with_carry = shifted_a0_b1_plus_a1_b0 + a0_b0`.
                const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
                hint DivMod { lhs: lower_uint128_with_carry, rhs: u128_limit } into { quotient: carry, remainder: lower_uint128 };
                // Verify the resulting lower_uint128 is indeed uint128 and that carry >= 0.
                assert lower_uint128 = *(range_check++);
                assert carry = *(range_check++);
                // Verify that carry < 2**66 by constraining carry + (2**128-1) - (2**66-1) < 2**128.
                const carry_range_fixer = u128::MAX - (2u128.pow(66) - 1);
                assert fixed_carry = carry + carry_range_fixer;
                assert fixed_carry = *(range_check++);
                // Verify the outputted `lower_uint128` and `carry` from the DivMod hint.
                assert shifted_carry = carry * u128_limit;
                assert lower_uint128_with_carry = shifted_carry + lower_uint128;
                // Note that reconstruction of the felt `lower_uint128_with_carry` is performed
                // with no wrap-around: `carry` was capped at 66 bits and then shifted 128 bits.
                // `lower_uint128` is range-checked for 128 bits. Overall, within 194 bits range.

                // Upper uint128 word:
                assert upper_uint128 = a1_b1 + carry;


            };
            Ok(builder.build_from_casm_builder(
                casm_builder,
                [("Fallthrough", &[&[range_check], &[upper_uint128], &[lower_uint128]], None)],
            ))
        }
        IntOperator::OverflowingMul => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
    }
}

/// Handles a casting a felt into u128.
fn build_u128_from_felt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check_expression, expr_value] = builder.try_get_refs()?;
    let range_check = range_check_expression.try_unpack_single()?.to_buffer(3)?;
    let value = expr_value.try_unpack_single()?.to_deref()?;

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let u128_bound: BigInt = BigInt::from(u128::MAX) + 1; // = 2**128.
    // Represent the maximal possible value (PRIME - 1) as 2**128 * max_x + max_y.
    let max_x: i128 = 10633823966279327296825105735305134080;
    let max_y: i128 = 0;
    let mut casm_builder = CasmBuilder::default();
    // Defining params and constants.
    let range_check = casm_builder.add_var(range_check);
    let value = casm_builder.add_var(ResOperand::Deref(value));
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

fn build_u128_lt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    let range_check = casm_builder.add_var(range_check);
    let a = casm_builder.add_var(ResOperand::Deref(a));
    let b = casm_builder.add_var(ResOperand::Deref(b));
    casm_build_extend! {casm_builder,
            tempvar a_ge_b;
            tempvar a_minus_b = a - b;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: a_ge_b};
            jump False if a_ge_b != 0;
            tempvar wrapping_a_minus_b = a_minus_b + u128_limit;
            assert wrapping_a_minus_b = *(range_check++);
            jump True;
        False:
            assert a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check]], None),
            ("True", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
    ))
}

fn build_u128_le(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    let range_check = casm_builder.add_var(range_check);
    let a = casm_builder.add_var(ResOperand::Deref(a));
    let b = casm_builder.add_var(ResOperand::Deref(b));
    casm_build_extend! {casm_builder,
            tempvar a_gt_b;
            tempvar b_minus_a = b - a;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThanOrEqual {lhs: u128_limit, rhs: b_minus_a} into {dst: a_gt_b};
            jump False if a_gt_b != 0;
            assert b_minus_a = *(range_check++);
            jump True;
        False:
            tempvar wrapping_a_minus_b = b_minus_a + u128_limit;
            assert wrapping_a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check]], None),
            ("True", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
    ))
}

// Handle u128 equality check.
fn build_u128_eq(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_a, expr_b] = builder.try_get_refs()?;
    let a = expr_a.try_unpack_single()?.to_deref()?;
    let b = expr_b.try_unpack_single()?.to_deref()?;

    // The target line to jump to if a != b.
    let target_statement_id = get_non_fallthrough_statement_id(&builder);

    let mut casm_builder = CasmBuilder::default();
    let a = casm_builder.add_var(ResOperand::Deref(a));
    let b = casm_builder.add_var(ResOperand::Deref(b));
    casm_build_extend! {casm_builder,
            // diff = a - b => (diff == 0) <==> (a == b)
            tempvar diff = a - b;

            jump NotEqual if diff != 0;
            jump Equal;
        NotEqual:
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Equal", &[], Some(target_statement_id))],
    ))
}
