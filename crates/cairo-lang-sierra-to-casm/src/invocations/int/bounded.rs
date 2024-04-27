use std::ops::Shl;

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::bounded_int::{
    BoundedIntConcreteLibfunc, BoundedIntDivRemAlgorithm,
};
use cairo_lang_sierra::extensions::felt252::Felt252BinaryOperator;
use cairo_lang_sierra::extensions::utils::Range;
use num_bigint::BigInt;
use num_traits::One;

use crate::invocations::felt252::build_felt252_op_with_var;
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CompiledInvocation,
    CompiledInvocationBuilder, CostValidationInfo, InvocationError,
};

/// Builds instructions for bounded int operations.
pub fn build(
    libfunc: &BoundedIntConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoundedIntConcreteLibfunc::Add(_) => {
            build_felt252_op_with_var(builder, Felt252BinaryOperator::Add)
        }
        BoundedIntConcreteLibfunc::Sub(_) => {
            build_felt252_op_with_var(builder, Felt252BinaryOperator::Sub)
        }
        BoundedIntConcreteLibfunc::Mul(_) => {
            build_felt252_op_with_var(builder, Felt252BinaryOperator::Mul)
        }
        BoundedIntConcreteLibfunc::DivRem(libfunc) => {
            build_div_rem(builder, &libfunc.lhs, &libfunc.rhs)
        }
        BoundedIntConcreteLibfunc::Constrain(libfunc) => {
            build_constrain(builder, &libfunc.boundary)
        }
    }
}

/// Build div rem on bounded ints.
/// Only supported cases are the cases where `BoundedIntDivRemAlgorithm::new` returns `Some`.
pub fn build_div_rem(
    builder: CompiledInvocationBuilder<'_>,
    lhs: &Range,
    rhs: &Range,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, a, b] = builder.try_get_single_cells()?;

    let alg = BoundedIntDivRemAlgorithm::new(lhs, rhs).unwrap();

    let mut casm_builder = CasmBuilder::default();
    let rc_slack = match &alg {
        BoundedIntDivRemAlgorithm::KnownSmallRhs => 2,
        BoundedIntDivRemAlgorithm::KnownSmallQuotient(_) => 3,
        BoundedIntDivRemAlgorithm::KnownSmallLhs(_) => 4,
    };
    add_input_variables! {casm_builder,
        buffer(rc_slack) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar r_plus_1;
        tempvar b_minus_r_minus_1;
    };
    let q_is_small = matches!(alg, BoundedIntDivRemAlgorithm::KnownSmallLhs(_))
        .then(|| casm_builder.alloc_var(false));
    let b_or_q_bound_rc_value = (!matches!(alg, BoundedIntDivRemAlgorithm::KnownSmallRhs))
        .then(|| casm_builder.alloc_var(false));
    casm_build_extend! {casm_builder,
        tempvar bq;
        tempvar q;
        tempvar r;
        hint DivMod { lhs: a, rhs: b } into { quotient: q, remainder: r };
    };
    // The following assertion is added in any case.
    // It is added in this order since `u128_safe_divmod` is using this order.
    if !matches!(alg, BoundedIntDivRemAlgorithm::KnownSmallRhs) {
        casm_build_extend!(casm_builder, assert q = *(range_check++););
    }
    casm_build_extend! {casm_builder,
        // Verify `0 <= r`.
        assert r = *(range_check++);
        // Verify `r < b` by constraining `0 <= b - (r + 1)`.
        const one = 1;
        assert r_plus_1 = r + one;
        assert b_minus_r_minus_1 = b - r_plus_1;
        assert b_minus_r_minus_1 = *(range_check++);

    };
    // The following assertion is added in any case.
    // It is added in this order since any non `u128` `u*_safe_divmod` is using this order.
    if matches!(alg, BoundedIntDivRemAlgorithm::KnownSmallRhs) {
        casm_build_extend!(casm_builder, assert q = *(range_check++););
    }

    // Validating `(q + 1) * b <= prime` and therefore `q * b + r < q * b + b <= prime`.
    match alg {
        BoundedIntDivRemAlgorithm::KnownSmallRhs => {
            // For this case `q + 1 <= q_max + 1 <= 2**128` and `b < rhs.upper` therefore
            // `(q + 1) * b < 2**128 * rhs.upper <= prime`.
        }
        BoundedIntDivRemAlgorithm::KnownSmallQuotient(q_bound) => {
            let b_or_q_bound_rc_value = b_or_q_bound_rc_value.unwrap();
            // For this case `(q + 1) <= q_bound`, and `b < rhs.upper <= 2**128` therefore
            // `(q + 1) * b < q_bound * 2**128 < prime`.
            casm_build_extend! {casm_builder,
                const u128_bound_minus_q_upper = (BigInt::one().shl(128) - q_bound) as BigInt;
                assert b_or_q_bound_rc_value = q + u128_bound_minus_q_upper;
                assert b_or_q_bound_rc_value = *(range_check++);
            }
        }
        BoundedIntDivRemAlgorithm::KnownSmallLhs(lhs_upper_sqrt) => {
            let q_is_small = q_is_small.unwrap();
            let b_or_q_bound_rc_value = b_or_q_bound_rc_value.unwrap();
            casm_build_extend! {casm_builder,
                // For this case we know that `(lhs_upper_sqrt + 1) * 2**128 < prime`.
                // First note that `(q + 1) * b <= (min(q, b) + 1) * max(q, b)`, since:
                // ```
                // max((min(q, b) + 1) * max(q, b), (max(q, b) + 1) * min(q, b)) ==
                // max(min(q, b) * max(q, b) + max(q, b), max(q, b) * min(q, b) + min(q, b)) ==
                // min(q, b) * max(q, b) + max(max(q, b), min(q, b)) ==
                // min(q, b) * max(q, b) + max(q, b) == (min(q, b) + 1) * max(q, b)
                // ```
                // Since `b * q < lhs.upper`, `min(b, q) <= sqrt(lhs.upper)`.
                // Since `b` and `q` are less than 2**128, `max(b, q) < 2**128`.
                // Therefore `(min(q, b) + 1) * max(q, b) <= (lhs_upper_sqrt + 1) * 2**128 < prime`.
                // We can now guess which whether `b` or `q` is small enough and verify.
                const limiter_bound = lhs_upper_sqrt.clone();
                hint TestLessThan {lhs: q, rhs: limiter_bound} into {dst: q_is_small};
                const u128_bound_minus_limiter_bound = (BigInt::one().shl(128) - lhs_upper_sqrt) as BigInt;
                jump QIsSmall if q_is_small != 0;
                // `q >= lhs_upper_sqrt`, so to verify `b < lhs_upper_sqrt` we assert
                // `2^128 - lhs_upper_sqrt + b` is in the range check bound.
                assert b_or_q_bound_rc_value = b + u128_bound_minus_limiter_bound;
                jump VerifyBQ;
            QIsSmall:
                // `q < lhs_upper_sqrt`, so to verify `q < lhs_upper_sqrt` we assert
                // `2^128 - lhs_upper_sqrt + q` is in the range check bound.
                assert b_or_q_bound_rc_value = q + u128_bound_minus_limiter_bound;
            VerifyBQ:
                // Now, b_or_q_bound_rc_value contains either `2^128 - lhs_upper_sqrt + q` or
                // `2^128 - lhs_upper_sqrt + b`. Verify this value is in [0, 2^128).
                assert b_or_q_bound_rc_value = *(range_check++);
            }
        }
    }
    casm_build_extend! {casm_builder,
        // Check that `a = q * b + r`.
        // We know that `q * b + r < prime` and thus there is no wraparound.
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

/// Build constrain on bounded ints.
fn build_constrain(
    builder: CompiledInvocationBuilder<'_>,
    boundary: &BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const under_fixer = (BigInt::one().shl(128) - boundary) as BigInt;
        const rc_bound_imm = BigInt::one().shl(128) as BigInt;
        const minus_boundary = -boundary;
        tempvar is_under;
        let canonical_value = value + minus_boundary;
        hint TestLessThanOrEqual {lhs: rc_bound_imm, rhs: canonical_value} into {dst: is_under};
        jump Under if is_under != 0;
    // Over:
        maybe_tempvar shifted_value = canonical_value;
        assert shifted_value = *(range_check++);
        jump Over;
    Under:
        // value < boundary  <=>  value + (2**128 - boundary) < 2**128.
        maybe_tempvar shifted_value = value + under_fixer;
        assert shifted_value = *(range_check++);
    };
    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Over", &[&[range_check], &[value]], Some(target_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
