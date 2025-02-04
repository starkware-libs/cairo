use std::ops::Shl;

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::bounded_int::{
    BoundedIntConcreteLibfunc, BoundedIntDivRemAlgorithm,
};
use cairo_lang_sierra::extensions::felt252::Felt252BinaryOperator;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::utils::Range;
use num_bigint::BigInt;
use num_traits::One;

use crate::invocations::felt252::build_felt252_op_with_var;
use crate::invocations::misc::{build_identity, build_is_zero};
use crate::invocations::{
    BuiltinInfo, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError, add_input_variables, get_non_fallthrough_statement_id,
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
        BoundedIntConcreteLibfunc::TrimMin(libfunc)
        | BoundedIntConcreteLibfunc::TrimMax(libfunc) => {
            build_trim(builder, &libfunc.trimmed_value)
        }
        BoundedIntConcreteLibfunc::IsZero(_) => build_is_zero(builder),
        BoundedIntConcreteLibfunc::WrapNonZero(_) => build_identity(builder),
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

    let alg = BoundedIntDivRemAlgorithm::try_new(lhs, rhs).unwrap();

    let mut casm_builder = CasmBuilder::default();
    let rc_slack = match &alg {
        BoundedIntDivRemAlgorithm::KnownSmallRhs => 2,
        BoundedIntDivRemAlgorithm::KnownSmallQuotient { .. }
        | BoundedIntDivRemAlgorithm::KnownSmallLhs { .. } => 3,
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

    let (q_is_small, b_or_q_bound_rc_value) = match alg {
        BoundedIntDivRemAlgorithm::KnownSmallRhs => (None, None),
        BoundedIntDivRemAlgorithm::KnownSmallQuotient { .. } => {
            (None, Some(casm_builder.alloc_var(false)))
        }
        BoundedIntDivRemAlgorithm::KnownSmallLhs { .. } => {
            (Some(casm_builder.alloc_var(false)), Some(casm_builder.alloc_var(false)))
        }
    };
    casm_build_extend! {casm_builder,
        tempvar bq;
        tempvar q;
        tempvar r;
        hint DivMod { lhs: a, rhs: b } into { quotient: q, remainder: r };
    };

    // The following assertion is added in all algorithms.
    // For backward compatibility with the `u*_safe_divmod` libfuncs, we put it either here
    // or below according to `alg`.
    let range_check_q_after = matches!(alg, BoundedIntDivRemAlgorithm::KnownSmallRhs);
    if !range_check_q_after {
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
    if range_check_q_after {
        casm_build_extend!(casm_builder, assert q = *(range_check++););
    }

    // Validating `(q + 1) * b <= prime` and therefore `q * b + r < q * b + b <= prime`.
    match alg {
        BoundedIntDivRemAlgorithm::KnownSmallRhs => {
            // For this case `q + 1 <= q_max + 1 <= 2**128` and `b < rhs.upper` therefore
            // `(q + 1) * b < 2**128 * rhs.upper <= prime`.
        }
        BoundedIntDivRemAlgorithm::KnownSmallQuotient { q_upper_bound } => {
            let b_or_q_bound_rc_value = b_or_q_bound_rc_value.unwrap();
            // For this case `(q + 1) <= q_bound`, and `b < rhs.upper <= 2**128` therefore
            // `(q + 1) * b < q_bound * 2**128 < prime`.
            casm_build_extend! {casm_builder,
                const u128_bound_minus_q_upper = (BigInt::one().shl(128) - q_upper_bound) as BigInt;
                assert b_or_q_bound_rc_value = q + u128_bound_minus_q_upper;
                assert b_or_q_bound_rc_value = *(range_check++);
            }
        }
        BoundedIntDivRemAlgorithm::KnownSmallLhs { lhs_upper_sqrt } => {
            let q_is_small = q_is_small.unwrap();
            let b_or_q_bound_rc_value = b_or_q_bound_rc_value.unwrap();
            casm_build_extend! {casm_builder,
                // Note that for a honest prover, `min{q, b} < root`, as otherwise
                // `lhs_upper > a >= q * b >= root ** 2` (and on the other hand,
                // by the definition of `root`: `root ** 2 >= lhs_upper`).
                // Therefore we require `min{q, b} < root`, which guarantees that:
                //   q * b + r < min{q, b} * max{q, b} + 2**128 <=
                //   (root - 1) * 2**128 + 2**128 <= root * 2**128 < prime.
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
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
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
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}

/// Build trim on bounded ints.
fn build_trim(
    builder: CompiledInvocationBuilder<'_>,
    trimmed_value: &BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let [value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder, deref value; );
    casm_build_extend! {casm_builder,
        const trimmed_value = trimmed_value.clone();
        maybe_tempvar diff = value - trimmed_value;
        jump Target if diff != 0;
    };
    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Target", &[&[value]], Some(target_statement_id))],
        Default::default(),
    ))
}
