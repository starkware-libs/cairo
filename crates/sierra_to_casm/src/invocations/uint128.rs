use casm::ap_change::ApplyApChange;
use casm::casm;
use casm::operand::{ap_cell_ref, DerefOrImmediate};
use itertools::chain;
use num_bigint::BigInt;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::extensions::integer::{
    IntOperator, Uint128BinaryOperationConcreteLibFunc, Uint128Concrete,
    Uint128OperationConcreteLibFunc, Uint128OperationWithConstConcreteLibFunc,
};

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
            let uint128_limit: BigInt = BigInt::from(u128::MAX) + 1;
            // The code up to the success branch.
            let mut before_success_branch = match op {
                IntOperator::OverflowingAdd => casm! {
                    [ap + 0] = a + b, ap++;
                    %{ memory[ap + 0] = memory [ap - 1] < (uint128_limit.clone()) %}
                    jmp rel 0 if [ap + 0] != 0, ap++;
                    // Overflow:
                    // Here we know that 2**128 <= a + b < 2 * (2**128 - 1).
                    [ap + 0] = [ap - 2] + (-uint128_limit), ap++;
                    [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];
                    jmp rel 0; // Fixed in relocations.
                },
                IntOperator::OverflowingSub => casm! {
                    a = [ap + 0] + b, ap++;
                    %{ memory[ap + 0] = memory [ap - 1] < (uint128_limit.clone())  %}
                    jmp rel 0 if [ap + 0] != 0, ap++;
                    // Underflow:
                    // Here we know that 0 - (2**128 - 1) <= a - b < 0.
                    [ap + 0] = [ap - 2] + uint128_limit, ap++;
                    [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];
                    jmp rel 0; // Fixed in relocations.
                },
                _ => unreachable!("Only supported options in arm."),
            };
            patch_jnz_to_end(&mut before_success_branch, 1);
            let relocation_index = before_success_branch.instructions.len() - 1;
            let success_branch = casm! {
                // No overflow:
                [ap - 2] = [[range_check.unchecked_apply_known_ap_change(2)]];
            };

            Ok(builder.build(
                chain!(before_success_branch.instructions, success_branch.instructions).collect(),
                vec![RelocationEntry {
                    instruction_idx: relocation_index,
                    relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
                }],
                [
                    vec![
                        ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                            op: FeltBinaryOperator::Add,
                            a: range_check.unchecked_apply_known_ap_change(2),
                            b: DerefOrImmediate::from(1),
                        })),
                        ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-2))),
                    ]
                    .into_iter(),
                    vec![
                        ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                            op: FeltBinaryOperator::Add,
                            a: range_check.unchecked_apply_known_ap_change(3),
                            b: DerefOrImmediate::from(1),
                        })),
                        ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-1))),
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
                [ap + 0] = [[range_check]], ap++;
                [ap + 0] = [[range_check.unchecked_apply_known_ap_change(1)] + 1], ap++;
                // Verify `r < b` by constraining `0 <= b - (r + 1)`.
                [ap + 0] = [ap + -1] + 1, ap++;
                (b.unchecked_apply_known_ap_change(3)) = [ap + 0] + [ap + -1], ap++;
                [ap + -1] = [[range_check.unchecked_apply_known_ap_change(4)] + 2], ap++;
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
    // TODO(lior): Reorder the temporary variables so that the result is at the top of the stack,
    //   and fix OutputVarReferenceInfo::NewTempVar::idx in the Sierra definition.
    // The code up to the success branch.
    let mut before_success_branch = casm! {
        %{ memory[ap + 0] = memory value < (uint128_bound.clone()) %}
        jmp rel 0 if [ap + 0] != 0, ap++; // Jump to success branch. Address updated later.
        // Write value as 2**128 * x + y.
        %{ (memory[ap + 0], memory[ap + 1]) = divmod(
            memory (value.unchecked_apply_known_ap_change(1)),
            (uint128_bound.clone())
        ) %}
        // Guess x and check that it is in the range [0, 2**128).
        [ap] = [[(range_check.unchecked_apply_known_ap_change(1))]], ap++;
        // Guess y and check that it is in the range [0, 2**128).
        [ap] = [[(range_check.unchecked_apply_known_ap_change(2))] + 1], ap++;
        // Check that value = 2**128 * x + y (mod PRIME).
        [ap] = [ap - 2] * (uint128_bound.clone()), ap++;
        (value.unchecked_apply_known_ap_change(4)) = [ap - 1] + [ap - 2];
        // Check that there is no overflow in the computation of 2**128 * x + y.
        // Start by checking if x==max_x.
        [ap] = [ap - 3] + (-max_x), ap++;
        jmp rel 6 if [ap - 1] != 0;
        // If x == max_x, check that y <= max_y.
        [ap] = [ap - 3] + (uint128_bound.clone() - max_y - 1), ap++;
        jmp rel 4;
        // If x != max_x, check that x < max_x.
        [ap] = [ap - 4] + (uint128_bound - max_x), ap++;
        // In both cases, range-check the calculated value.
        [ap - 1] = [[(range_check.unchecked_apply_known_ap_change(6))] + 2];
        // If x != 0, jump to the end.
        jmp rel 0 if [ap - 5] != 0; // Fixed in relocations.
        // Otherwise, start an infinite loop.
        jmp rel 0;
    };
    patch_jnz_to_end(&mut before_success_branch, 0);
    let relocation_index = before_success_branch.instructions.len() - 2;
    let success_branch = casm! {
        // No overflow:
        value = [[(range_check.unchecked_apply_known_ap_change(1))]];
    };

    Ok(builder.build(
        chain!(before_success_branch.instructions, success_branch.instructions).collect(),
        vec![RelocationEntry {
            instruction_idx: relocation_index,
            relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
        }],
        [
            vec![
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(1),
                    b: DerefOrImmediate::Immediate(BigInt::from(1)),
                })),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    value.unchecked_apply_known_ap_change(1),
                )),
            ]
            .into_iter(),
            vec![
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(6),
                    b: DerefOrImmediate::Immediate(BigInt::from(3)),
                })),
                ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-5))),
                ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-4))),
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
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];
        jmp rel 0; // Fixed in relocations.
    };
    let ge_code = casm! {
        // `a >= b` <===> `a - b >= 0`.
        // Compute `a - b`.
        (a.unchecked_apply_known_ap_change(1)) = [ap + 0] + (b.unchecked_apply_known_ap_change(1)),
            ap++;
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(2)]];
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
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(2)]];
        jmp rel 0; // Fixed in relocations.
    };
    let gt_code = casm! {
        // `a > b` <===> `a - b - 1 >= 0`.
        [ap + 0] = (b.unchecked_apply_known_ap_change(1)) + 1, ap++; // Compute `b + 1`.
        (a.unchecked_apply_known_ap_change(2)) = [ap + 0] + [ap - 1], ap++; // Compute `a - b - 1`.
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];
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
