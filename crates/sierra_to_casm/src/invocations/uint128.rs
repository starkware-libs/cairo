use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::{CellRef, ResOperand};
use num_bigint::BigInt;
use sierra::extensions::uint128::{
    IntOperator, Uint128BinaryOperationConcreteLibFunc, Uint128Concrete,
    Uint128OperationConcreteLibFunc, Uint128OperationWithConstConcreteLibFunc,
};
use sierra_ap_change::core_libfunc_ap_change;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

#[cfg(test)]
#[path = "uint128_test.rs"]
mod test;

/// Builds instructions for Sierra u128 operations.
pub fn build(
    libfunc: &Uint128Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Binary(
            Uint128BinaryOperationConcreteLibFunc { operator, .. },
        )) => build_u128_op(builder, *operator),
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Const(
            Uint128OperationWithConstConcreteLibFunc { operator: _, c: _, .. },
        )) => Err(InvocationError::NotImplemented(builder.invocation.clone())),
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
    match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: expr_a, .. },
            ReferenceValue { expression: expr_b, .. },
        ] => Ok((
            range_check_expression.try_unpack_single()?.to_buffer(0)?,
            expr_a.try_unpack_single()?.to_deref()?,
            expr_b.try_unpack_single()?.to_deref()?,
        )),

        refs => Err(InvocationError::WrongNumberOfArguments { expected: 3, actual: refs.len() }),
    }
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
            let u128_limit =
                casm_builder.add_var(ResOperand::Immediate(BigInt::from(u128::MAX) + 1));
            let range_check = casm_builder.add_var(range_check);
            let a = casm_builder.add_var(ResOperand::Deref(a));
            let b = casm_builder.add_var(ResOperand::Deref(b));
            let (possible_overflow, overflow_fixed) = match op {
                IntOperator::OverflowingAdd => {
                    casm_build_extend! {casm_builder,
                        tempvar no_overflow;
                        tempvar a_plus_b;
                        assert a_plus_b = a + b;
                        hint TestLessThan {lhs: a_plus_b, rhs: u128_limit} into {dst: no_overflow};
                        jump NoOverflow if no_overflow != 0;
                        // Overflow:
                        // Here we know that 2**128 <= a + b < 2 * (2**128 - 1).
                        tempvar wrapping_a_plus_b;
                        assert a_plus_b = wrapping_a_plus_b + u128_limit;
                    };
                    (a_plus_b, wrapping_a_plus_b)
                }
                IntOperator::OverflowingSub => {
                    casm_build_extend! {casm_builder,
                        tempvar no_overflow;
                        tempvar a_minus_b;
                        assert a = a_minus_b + b;
                        hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: no_overflow};
                        jump NoOverflow if no_overflow != 0;
                        // Underflow:
                        // Here we know that 0 - (2**128 - 1) <= a - b < 0.
                        tempvar wrapping_a_minus_b;
                        assert wrapping_a_minus_b = a_minus_b + u128_limit;
                    };
                    (a_minus_b, wrapping_a_minus_b)
                }
                _ => unreachable!("Only supported options in arm."),
            };
            casm_build_extend! {casm_builder,
                    assert *(range_check++) = overflow_fixed;
                    jump Target;
                NoOverflow:
                    assert *(range_check++) = possible_overflow;
            };
            let CasmBuildResult {
                instructions,
                awaiting_relocations,
                label_state,
                fallthrough_state,
            } = casm_builder.build();
            // TODO(orizi): Extract the assertion out of the libfunc implementation.
            assert_eq!(
                core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
                [fallthrough_state.ap_change, label_state["Target"].ap_change]
                    .map(sierra_ap_change::ApChange::Known)
            );
            let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
            Ok(builder.build(
                instructions,
                vec![RelocationEntry {
                    instruction_idx: *relocation_index,
                    relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
                }],
                [
                    vec![
                        ReferenceExpression::from_cell(CellExpression::from_res_operand(
                            fallthrough_state.get_adjusted(range_check),
                        )),
                        ReferenceExpression::from_cell(CellExpression::Deref(
                            fallthrough_state.get_adjusted_as_cell_ref(possible_overflow),
                        )),
                    ]
                    .into_iter(),
                    vec![
                        ReferenceExpression::from_cell(CellExpression::from_res_operand(
                            label_state["Target"].get_adjusted(range_check),
                        )),
                        ReferenceExpression::from_cell(CellExpression::Deref(
                            label_state["Target"].get_adjusted_as_cell_ref(overflow_fixed),
                        )),
                    ]
                    .into_iter(),
                ]
                .into_iter(),
            ))
        }
        IntOperator::DivMod => {
            let mut casm_builder = CasmBuilder::default();
            let u128_bound_minus_u64_bound = casm_builder
                .add_var(ResOperand::Immediate(BigInt::from(u128::MAX) - BigInt::from(u64::MAX)));
            let u64_bound = casm_builder.add_var(ResOperand::Immediate(BigInt::from(u64::MAX) + 1));
            let one = casm_builder.add_var(ResOperand::Immediate(BigInt::from(1)));
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
                assert *(range_check++) = q;
                assert *(range_check++) = r;
                // Verify `r < b` by constraining `0 <= b - (r + 1)`.
                assert r_plus_1 = r + one;
                assert b = b_minus_r_minus_1 + r_plus_1;
                assert *(range_check++) = b_minus_r_minus_1;
                // Verify `b * q + r = a`.
                // Since both `b` and `q` can be 2^128-1, we may overflow on `b * q`. To verify this
                // is not the case, use the fact that `b * q` must be less than 2^128. We know
                // `min(b, q)` must be less than 2^64. We guess which is less and verify.
                hint TestLessThan {lhs: q, rhs: u64_bound} into {dst: q_is_small};
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
                assert *(range_check++) = b_or_q_bound_rc_value;
                // Range validations done; verify `b * q + r = a` and that's it.
                assert bq = b * q;
                assert a = bq + r;
            };

            let CasmBuildResult {
                instructions,
                awaiting_relocations,
                label_state: _,
                fallthrough_state,
            } = casm_builder.build();
            // TODO(orizi): Extract the assertion out of the libfunc implementation.
            assert_eq!(
                core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
                [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
            );

            assert!(
                awaiting_relocations.is_empty(),
                "Malformed casm builder usage (no non-fallthrough branch in divmod)."
            );

            Ok(builder.build(
                instructions,
                vec![],
                vec![
                    vec![
                        ReferenceExpression::from_cell(CellExpression::from_res_operand(
                            fallthrough_state.get_adjusted(range_check),
                        )),
                        ReferenceExpression::from_cell(CellExpression::Deref(
                            fallthrough_state.get_adjusted_as_cell_ref(q),
                        )),
                        ReferenceExpression::from_cell(CellExpression::Deref(
                            fallthrough_state.get_adjusted_as_cell_ref(r),
                        )),
                    ]
                    .into_iter(),
                ]
                .into_iter(),
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
    let (range_check, value) = match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => (
            range_check_expression.try_unpack_single()?.to_buffer(3)?,
            expr_value.try_unpack_single()?.to_deref()?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let u128_bound: BigInt = BigInt::from(u128::MAX) + 1; // = 2**128.
    // Represent the maximal possible value (PRIME - 1) as 2**128 * max_x + max_y.
    let max_x: i128 = 10633823966279327296825105735305134080;
    let max_y: i128 = 0;
    let mut casm_builder = CasmBuilder::default();
    // Defining params and constants.
    let range_check = casm_builder.add_var(range_check);
    let value = casm_builder.add_var(ResOperand::Deref(value));
    let u128_limit = casm_builder.add_var(ResOperand::Immediate(u128_bound.clone()));
    let le_max_y_fix = casm_builder.add_var(ResOperand::Immediate(u128_bound.clone() - max_y - 1));
    let lt_max_x_fix = casm_builder.add_var(ResOperand::Immediate(u128_bound - max_x));
    let minus_max_x = casm_builder.add_var(ResOperand::Immediate(BigInt::from(-max_x)));
    casm_build_extend! {casm_builder,
            tempvar is_u128;
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
            assert *(range_check++) = x;
            // Check y in [0, 2**128).
            assert *(range_check++) = y;
            // Check that value = 2**128 * x + y (mod PRIME).
            assert x_2_128 = x * u128_limit;
            assert value = x_2_128 + y;
            // Check that there is no overflow in the computation of 2**128 * x + y.
            // Start by checking if x==max_x.
            assert x_minus_max_x = x + minus_max_x;
            jump XNotMaxX if x_minus_max_x != 0;
            // If x == max_x, check that y <= max_y.
            assert rced_value = y + le_max_y_fix;
            jump WriteRcedValue;
        XNotMaxX:
            // If x != max_x, check that x < max_x.
            assert rced_value = x + lt_max_x_fix;
        WriteRcedValue:
            // In both cases, range-check the calculated value.
            assert *(range_check++) = rced_value;
            // If x != 0, jump to the end.
            jump FailureHandle if x != 0;
        InfiniteLoop:
            // Otherwise, start an infinite loop.
            jump InfiniteLoop;
        NoOverflow:
            assert *(range_check++) = value;
    };
    let CasmBuildResult { instructions, awaiting_relocations, label_state, fallthrough_state } =
        casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["FailureHandle"].ap_change]
            .map(sierra_ap_change::ApChange::Known)
    );
    let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
    Ok(builder.build(
        instructions,
        vec![RelocationEntry {
            instruction_idx: *relocation_index,
            relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
        }],
        [
            vec![
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    fallthrough_state.get_adjusted(range_check),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    fallthrough_state.get_adjusted_as_cell_ref(value),
                )),
            ]
            .into_iter(),
            vec![
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    label_state["FailureHandle"].get_adjusted(range_check),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    label_state["FailureHandle"].get_adjusted_as_cell_ref(x),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    label_state["FailureHandle"].get_adjusted_as_cell_ref(y),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}

fn build_u128_lt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    let u128_limit = casm_builder.add_var(ResOperand::Immediate(BigInt::from(u128::MAX) + 1));
    let range_check = casm_builder.add_var(range_check);
    let a = casm_builder.add_var(ResOperand::Deref(a));
    let b = casm_builder.add_var(ResOperand::Deref(b));
    casm_build_extend! {casm_builder,
            tempvar a_ge_b;
            tempvar a_minus_b;
            assert a = a_minus_b + b;
            hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: a_ge_b};
            jump False if a_ge_b != 0;
            tempvar wrapping_a_minus_b;
            assert wrapping_a_minus_b = a_minus_b + u128_limit;
            assert *(range_check++) = wrapping_a_minus_b;
            jump True;
        False:
            assert *(range_check++) = a_minus_b;
    };
    let CasmBuildResult { instructions, awaiting_relocations, label_state, fallthrough_state } =
        casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["True"].ap_change]
            .map(sierra_ap_change::ApChange::Known)
    );
    let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
    Ok(builder.build(
        instructions,
        vec![RelocationEntry {
            instruction_idx: *relocation_index,
            relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
        }],
        [
            vec![ReferenceExpression::from_cell(CellExpression::from_res_operand(
                fallthrough_state.get_adjusted(range_check),
            ))]
            .into_iter(),
            vec![ReferenceExpression::from_cell(CellExpression::from_res_operand(
                label_state["True"].get_adjusted(range_check),
            ))]
            .into_iter(),
        ]
        .into_iter(),
    ))
}

fn build_u128_le(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    let u128_limit = casm_builder.add_var(ResOperand::Immediate(BigInt::from(u128::MAX) + 1));
    let range_check = casm_builder.add_var(range_check);
    let a = casm_builder.add_var(ResOperand::Deref(a));
    let b = casm_builder.add_var(ResOperand::Deref(b));
    casm_build_extend! {casm_builder,
            tempvar a_gt_b;
            tempvar b_minus_a;
            assert b = b_minus_a + a;
            hint TestLessThanOrEqual {lhs: u128_limit, rhs: b_minus_a} into {dst: a_gt_b};
            jump False if a_gt_b != 0;
            assert *(range_check++) = b_minus_a;
            jump True;
        False:
            tempvar wrapping_a_minus_b;
            assert wrapping_a_minus_b = b_minus_a + u128_limit;
            assert *(range_check++) = wrapping_a_minus_b;
    };
    let CasmBuildResult { instructions, awaiting_relocations, label_state, fallthrough_state } =
        casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["True"].ap_change]
            .map(sierra_ap_change::ApChange::Known)
    );
    let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
    Ok(builder.build(
        instructions,
        vec![RelocationEntry {
            instruction_idx: *relocation_index,
            relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
        }],
        [
            vec![ReferenceExpression::from_cell(CellExpression::from_res_operand(
                fallthrough_state.get_adjusted(range_check),
            ))]
            .into_iter(),
            vec![ReferenceExpression::from_cell(CellExpression::from_res_operand(
                label_state["True"].get_adjusted(range_check),
            ))]
            .into_iter(),
        ]
        .into_iter(),
    ))
}

// Handle u128 equality check.
fn build_u128_eq(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (a, b) = match builder.refs {
        [ReferenceValue { expression: expr_a, .. }, ReferenceValue { expression: expr_b, .. }] => {
            (expr_a.try_unpack_single()?.to_deref()?, expr_b.try_unpack_single()?.to_deref()?)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    // The target line to jump to if a != b.
    let target_statement_id = get_non_fallthrough_statement_id(&builder);

    let mut casm_builder = CasmBuilder::default();
    let a = casm_builder.add_var(ResOperand::Deref(a));
    let b = casm_builder.add_var(ResOperand::Deref(b));
    casm_build_extend! {casm_builder,
            // diff = a - b => (diff == 0) <==> (a == b)
            tempvar diff;
            assert a = diff + b;

            jump NotEqual if diff != 0;
            jump Equal;
        NotEqual:
    };
    let CasmBuildResult { instructions, awaiting_relocations, fallthrough_state, label_state } =
        casm_builder.build();

    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["Equal"].ap_change]
            .map(sierra_ap_change::ApChange::Known)
    );
    let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
    Ok(builder.build(
        instructions,
        vec![RelocationEntry {
            instruction_idx: *relocation_index,
            relocation: Relocation::RelativeStatementId(target_statement_id),
        }],
        vec![vec![].into_iter(), vec![].into_iter()].into_iter(),
    ))
}
