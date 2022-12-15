use casm::ap_change::ApplyApChange;
use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::operand::{ap_cell_ref, DerefOrImmediate, ResOperand};
use casm::{casm, casm_build_extend};
use itertools::chain;
use num_bigint::BigInt;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::extensions::integer::{
    IntOperator, Uint128BinaryOperationConcreteLibFunc, Uint128Concrete,
    Uint128OperationConcreteLibFunc, Uint128OperationWithConstConcreteLibFunc,
};
use sierra_ap_change::core_libfunc_ap_change;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    get_bool_comparison_target_statement_id, patch_jnz_to_end,
    unwrap_range_check_based_binary_op_refs,
};
use crate::references::{
    try_unpack_deref, BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue,
};
use crate::relocations::{Relocation, RelocationEntry};

#[cfg(test)]
#[path = "uint128_test.rs"]
mod test;

/// Builds instructions for Sierra uint128 operations.
pub fn build(
    libfunc: &Uint128Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Binary(
            Uint128BinaryOperationConcreteLibFunc { operator, .. },
        )) => build_uint128_op(builder, *operator),
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Const(
            Uint128OperationWithConstConcreteLibFunc { operator: _, c: _, .. },
        )) => Err(InvocationError::NotImplemented(builder.invocation.clone())),
        Uint128Concrete::JumpNotZero(_) => misc::build_jump_nz(builder),
        Uint128Concrete::Const(libfunc) => Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Immediate(BigInt::from(libfunc.c)))]
                .into_iter(),
        )),
        Uint128Concrete::FromFelt(_) => build_uint128_from_felt(builder),
        Uint128Concrete::ToFelt(_) => misc::build_identity(builder),
        Uint128Concrete::LessThan(_) => build_uint128_lt(builder),
        Uint128Concrete::LessThanOrEqual(_) => build_uint128_le(builder),
    }
}

/// Handles a uint128 operation with the given op.
fn build_uint128_op(
    builder: CompiledInvocationBuilder<'_>,
    op: IntOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    match op {
        IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
            let failure_handle_statement_id = get_bool_comparison_target_statement_id(&builder);
            let mut casm_builder = CasmBuilder::default();
            let uint128_limit =
                casm_builder.add_var(ResOperand::Immediate(BigInt::from(u128::MAX) + 1));
            let range_check = casm_builder.add_var(ResOperand::Deref(range_check));
            let a = casm_builder.add_var(ResOperand::Deref(a));
            let b = casm_builder.add_var(ResOperand::Deref(b));
            let (possible_overflow, overflow_fixed) = match op {
                IntOperator::OverflowingAdd => {
                    casm_build_extend! {casm_builder,
                        alloc no_overflow;
                        alloc a_plus_b;
                        a_plus_b = a + b;
                        hint TestLessThan {lhs: a_plus_b, rhs: uint128_limit} into {dst: no_overflow};
                        jump NoOverflow if no_overflow != 0;
                        // Overflow:
                        // Here we know that 2**128 <= a + b < 2 * (2**128 - 1).
                        alloc wrapping_a_plus_b;
                        a_plus_b = wrapping_a_plus_b + uint128_limit;
                    };
                    (a_plus_b, wrapping_a_plus_b)
                }
                IntOperator::OverflowingSub => {
                    casm_build_extend! {casm_builder,
                        alloc no_overflow;
                        alloc a_minus_b;
                        a = a_minus_b + b;
                        hint TestLessThan {lhs: a_minus_b, rhs: uint128_limit} into {dst: no_overflow};
                        jump NoOverflow if no_overflow != 0;
                        // Underflow:
                        // Here we know that 0 - (2**128 - 1) <= a - b < 0.
                        alloc wrapping_a_minus_b;
                        wrapping_a_minus_b = a_minus_b + uint128_limit;
                    };
                    (a_minus_b, wrapping_a_minus_b)
                }
                _ => unreachable!("Only supported options in arm."),
            };
            casm_build_extend! {casm_builder,
                    *(range_check++) = overflow_fixed;
                    jump Target;
                NoOverflow:
                   *(range_check++) = possible_overflow;
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
            let code = casm! {
                %{ (memory[ap + 0], memory[ap + 1]) = divmod(a, b) %}
                // Both `q` and `r` must be uint128.
                // We must check `r` explicitly: we later check that `0 <= b - (r + 1)` and
                // `b * q + r = a`, however, if `r = -1` we may pass both of these checks (say, if
                // `b = a + 1` and `q = 1`).
                // We must also check `q` explicitly; the only arithmetic constraint on `q` is
                // `b * q + r = a`, and if `b = 2`, `a = 1` and `r = 0`, we can take `q` to be the
                // inverse of 2 (`(PRIME + 1) / 2`, much larger than 2^128) and pass this
                // constraint.
                [ap + 0] = [[&range_check]], ap++;
                [ap + 0] = [[&range_check.unchecked_apply_known_ap_change(1)] + 1], ap++;
                // Verify `r < b` by constraining `0 <= b - (r + 1)`.
                [ap + 0] = [ap + -1] + 1, ap++;
                (b.unchecked_apply_known_ap_change(3)) = [ap + 0] + [ap + -1], ap++;
                [ap + -1] = [[&range_check.unchecked_apply_known_ap_change(4)] + 2], ap++;
                // Verify `b * q + r = a`.
                [ap + -1] = [ap + -5] * (b.unchecked_apply_known_ap_change(5));
                (a.unchecked_apply_known_ap_change(5)) = [ap + -1] + [ap + -4];
            };
            Ok(builder.build(
                code.instructions,
                vec![],
                vec![
                    vec![
                        ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                            op: FeltBinaryOperator::Add,
                            a: range_check.unchecked_apply_known_ap_change(5),
                            b: DerefOrImmediate::from(3),
                        })),
                        ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-5))),
                        ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-4))),
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

/// Handles a casting a felt into uint128.
fn build_uint128_from_felt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, value) = match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => (try_unpack_deref(range_check_expression)?, try_unpack_deref(expr_value)?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let failure_handle_statement_id = get_bool_comparison_target_statement_id(&builder);
    let uint128_bound: BigInt = BigInt::from(u128::MAX) + 1; // = 2**128.
    // Represent the maximal possible value (PRIME - 1) as 2**128 * max_x + max_y.
    let max_x: i128 = 10633823966279327296825105735305134080;
    let max_y: i128 = 0;
    let mut casm_builder = CasmBuilder::default();
    // Defining params and constants.
    let range_check = casm_builder.add_var(ResOperand::Deref(range_check));
    let value = casm_builder.add_var(ResOperand::Deref(value));
    let uint128_limit = casm_builder.add_var(ResOperand::Immediate(uint128_bound.clone()));
    let le_max_y_fix =
        casm_builder.add_var(ResOperand::Immediate(uint128_bound.clone() - max_y - 1));
    let lt_max_x_fix = casm_builder.add_var(ResOperand::Immediate(uint128_bound - max_x));
    let minus_max_x = casm_builder.add_var(ResOperand::Immediate(BigInt::from(-max_x)));
    casm_build_extend! {casm_builder,
            alloc is_uint128;
            hint TestLessThan { lhs: value, rhs: uint128_limit } into { dst: is_uint128 };
            jump NoOverflow if is_uint128 != 0;
            // Allocating all values required so that `x` and `y` would be last.
            alloc x_2_128;
            alloc x_minus_max_x;
            alloc rced_value;
            alloc x;
            alloc y;
            // Write value as 2**128 * x + y.
            hint DivMod { lhs: value, rhs: uint128_limit } into { quotient: x, remainder: y };
            // Check x in [0, 2**128).
            *(range_check++) = x;
            // Check y in [0, 2**128).
            *(range_check++) = y;
            // Check that value = 2**128 * x + y (mod PRIME).
            x_2_128 = x * uint128_limit;
            value = x_2_128 + y;
            // Check that there is no overflow in the computation of 2**128 * x + y.
            // Start by checking if x==max_x.
            x_minus_max_x = x + minus_max_x;
            jump XNotMaxX if x_minus_max_x != 0;
            // If x == max_x, check that y <= max_y.
            rced_value = y + le_max_y_fix;
            jump WriteRcedValue;
        XNotMaxX:
            // If x != max_x, check that x < max_x.
            rced_value = x + lt_max_x_fix;
        WriteRcedValue:
            // In both cases, range-check the calculated value.
            *(range_check++) = rced_value;
            // If x != 0, jump to the end.
            jump FailureHandle if x != 0;
        InfiniteLoop:
            // Otherwise, start an infinite loop.
            jump InfiniteLoop;
        NoOverflow:
            *(range_check++) = value;
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

fn build_uint128_lt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    let target_statement_id = get_bool_comparison_target_statement_id(&builder);

    // Split the code into two blocks, to get the offset of the first block as the jump target in
    // case `a >= b`.
    let mut jnz_and_lt_code = casm! {
        // Check if `a >= b`.
        %{ memory[ap + 0] = memory b <= memory a %}
        jmp rel 0 if [ap + 0] != 0, ap++;
        // `a < b` <===> `b - a - 1 >= 0`.
        [ap + 0] = (a.unchecked_apply_known_ap_change(1)) + 1, ap++; // Compute `a + 1`.
        // Compute `b - a - 1`.
        (b.unchecked_apply_known_ap_change(2)) = [ap + 0] + [ap + -1], ap++;
        [ap - 1] = [[&range_check.unchecked_apply_known_ap_change(3)]];
        jmp rel 0; // Fixed in relocations.
    };
    let ge_code = casm! {
        // `a >= b` <===> `a - b >= 0`.
        // Compute `a - b`.
        (a.unchecked_apply_known_ap_change(1)) = [ap + 0] + (b.unchecked_apply_known_ap_change(1)),
            ap++;
        [ap - 1] = [[&range_check.unchecked_apply_known_ap_change(2)]];
    };

    // Since the jump offset of the positive (X<Y) case depends only on the above CASM code,
    // compute it here and manually replace the value in the `jmp`.
    // The target should be just after the `jmp rel 0` statement, which ends the X>=Y case.
    patch_jnz_to_end(&mut jnz_and_lt_code, 0);

    let relocation_index = jnz_and_lt_code.instructions.len() - 1;
    Ok(builder.build(
        chain!(jnz_and_lt_code.instructions, ge_code.instructions).collect(),
        vec![RelocationEntry {
            instruction_idx: relocation_index,
            relocation: Relocation::RelativeStatementId(target_statement_id),
        }],
        [2, 3]
            .map(|ap_change| {
                vec![ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(ap_change),
                    b: DerefOrImmediate::from(1),
                }))]
                .into_iter()
            })
            .into_iter(),
    ))
}

fn build_uint128_le(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, a, b) = unwrap_range_check_based_binary_op_refs(&builder)?;
    let target_statement_id = get_bool_comparison_target_statement_id(&builder);

    // Split the code into two blocks, to get the offset of the first block as the jump target in
    // case `a > b`.
    let mut jnz_and_le_code = casm! {
        // Check if `a > b`.
        %{ memory[ap + 0] = memory b < memory a %}
        jmp rel 0 if [ap + 0] != 0, ap++;
        // `a <= b` <===> `b - a >= 0`.
        // Compute `b - a`.
        (b.unchecked_apply_known_ap_change(1)) = [ap + 0] + (a.unchecked_apply_known_ap_change(1)),
            ap++;
        [ap - 1] = [[&range_check.unchecked_apply_known_ap_change(2)]];
        jmp rel 0; // Fixed in relocations.
    };
    let gt_code = casm! {
        // `a > b` <===> `a - b - 1 >= 0`.
        [ap + 0] = (b.unchecked_apply_known_ap_change(1)) + 1, ap++; // Compute `b + 1`.
        (a.unchecked_apply_known_ap_change(2)) = [ap + 0] + [ap - 1], ap++; // Compute `a - b - 1`.
        [ap - 1] = [[&range_check.unchecked_apply_known_ap_change(3)]];
    };

    // Since the jump offset of the positive (X<Y) case depends only on the above CASM code,
    // compute it here and manually replace the value in the `jmp`.
    // The target should be just after the `jmp rel 0` statement, which ends the X>=Y case.
    patch_jnz_to_end(&mut jnz_and_le_code, 0);

    let relocation_index = jnz_and_le_code.instructions.len() - 1;
    Ok(builder.build(
        chain!(jnz_and_le_code.instructions, gt_code.instructions).collect(),
        vec![RelocationEntry {
            instruction_idx: relocation_index,
            relocation: Relocation::RelativeStatementId(target_statement_id),
        }],
        [3, 2]
            .map(|ap_change| {
                vec![ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(ap_change),
                    b: DerefOrImmediate::from(1),
                }))]
                .into_iter()
            })
            .into_iter(),
    ))
}
