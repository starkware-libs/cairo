use casm::ap_change::{ApChange, ApplyApChange};
use casm::casm;
use casm::instructions::InstructionBody;
use casm::operand::{ap_cell_ref, DerefOrImmediate};
use itertools::chain;
use num_bigint::{BigInt, ToBigInt};
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::integer::{
    IntOperator, Uint128BinaryOperationConcreteLibFunc, Uint128Concrete,
    Uint128OperationConcreteLibFunc, Uint128OperationWithConstConcreteLibFunc,
};
use sierra::program::{BranchInfo, BranchTarget};
use utils::{extract_matches, try_extract_matches};

use super::misc::build_jump_nz;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

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
        Uint128Concrete::JumpNotZero(_) => build_jump_nz(builder),
        Uint128Concrete::Const(libfunc) => Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Immediate(
                libfunc.c.to_bigint().unwrap(),
            ))]
            .into_iter(),
        )),
        Uint128Concrete::FromFelt(_) => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
        Uint128Concrete::ToFelt(_) => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
    }
}

/// Handles a uint128 operation with the given op.
fn build_uint128_op(
    builder: CompiledInvocationBuilder<'_>,
    op: IntOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let get_deref = |expr: &ReferenceExpression| {
        expr.try_unpack_single()
            .ok()
            .and_then(|cell| try_extract_matches!(cell, CellExpression::Deref))
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)
    };
    let (range_check, a, b) = match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: expr_a, .. },
            ReferenceValue { expression: expr_b, .. },
        ] => (get_deref(range_check_expression)?, get_deref(expr_a)?, get_deref(expr_b)?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };
    match op {
        IntOperator::Add | IntOperator::Sub => {
            let failure_handle_statement_id = match builder.invocation.branches.as_slice() {
                [BranchInfo { target: BranchTarget::Statement(statement_id), .. }, _] => {
                    statement_id
                }
                _ => panic!("malformed invocation"),
            };
            let uint128_limit: BigInt = u128::MAX.to_bigint().unwrap() + 1;
            // The code up to the success branch.
            let mut before_success_branch = match op {
                IntOperator::Add => casm! {
                    [ap + 0] = a + b, ap++;
                    %{ memory[ap + 0] = (uint128_limit.clone() - 1) < memory [ap - 1] %}
                    jmp rel 0 if [ap + 0] != 0, ap++;
                    // Overflow:
                    // Here we know that 2**128 <= a + b < 2 * (2**128 - 1).
                    [ap + 0] = [ap - 2] + (-uint128_limit), ap++;
                    [ap - 1] = [[range_check.apply_ap_change(ApChange::Known(3)).unwrap()]];
                    jmp rel 0; // Fixed in relocations.
                },
                IntOperator::Sub => casm! {
                    a = [ap + 0] + b, ap++;
                    %{ memory[ap + 0] = (uint128_limit.clone() - 1) < memory [ap - 1] %}
                    jmp rel 0 if [ap + 0] != 0, ap++;
                    // Underflow:
                    // Here we know that 0 - (2**128 - 1) <= a - b < 0.
                    [ap + 0] = [ap - 2] + uint128_limit, ap++;
                    [ap - 1] = [[range_check.apply_ap_change(ApChange::Known(3)).unwrap()]];
                    jmp rel 0; // Fixed in relocations.
                },
                _ => unreachable!("Only supported options in arm."),
            };
            let branch_offset = before_success_branch.current_code_offset
                - before_success_branch.instructions[0].body.op_size();
            *extract_matches!(
                &mut extract_matches!(
                    &mut before_success_branch.instructions[1].body,
                    InstructionBody::Jnz
                )
                .jump_offset,
                DerefOrImmediate::Immediate
            ) = branch_offset.to_bigint().unwrap();
            let relocation_index = before_success_branch.instructions.len() - 1;
            let success_branch = casm! {
                // No overflow:
                [ap - 2] = [[range_check.apply_ap_change(ApChange::Known(2)).unwrap()]];
            };

            Ok(builder.build(
                chain!(before_success_branch.instructions, success_branch.instructions).collect(),
                vec![RelocationEntry {
                    instruction_idx: relocation_index,
                    relocation: Relocation::RelativeStatementId(*failure_handle_statement_id),
                }],
                [ApChange::Known(2), ApChange::Known(3)].into_iter(),
                [
                    vec![
                        ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                            op: FeltOperator::Add,
                            a: range_check.apply_ap_change(ApChange::Known(2)).unwrap(),
                            b: DerefOrImmediate::from(1),
                        })),
                        ReferenceExpression::from_cell(CellExpression::Deref(ap_cell_ref(-2))),
                    ]
                    .into_iter(),
                    vec![ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltOperator::Add,
                        a: range_check.apply_ap_change(ApChange::Known(3)).unwrap(),
                        b: DerefOrImmediate::from(1),
                    }))]
                    .into_iter(),
                ]
                .into_iter(),
            ))
        }
        IntOperator::Mul
        | IntOperator::Div
        | IntOperator::Mod
        | IntOperator::WrappingAdd
        | IntOperator::WrappingSub
        | IntOperator::WrappingMul => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
    }
}
