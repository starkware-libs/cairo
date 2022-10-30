use casm::ap_change::{ApChange, ApplyApChange};
use casm::casm;
use casm::instructions::InstructionBody;
use casm::operand::DerefOrImmediate;
use itertools::chain;
use num_bigint::ToBigInt;
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::gas::GasConcreteLibFunc;
use sierra::program::{BranchInfo, BranchTarget};
use utils::{extract_matches, try_extract_matches};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &GasConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasConcreteLibFunc::GetGas(_) => build_get_gas(builder),
        GasConcreteLibFunc::RefundGas(_) => build_refund_gas(builder),
        GasConcreteLibFunc::BurnGas(_) => Ok(builder.build_only_reference_changes([].into_iter())),
    }
}

/// Handles the get gas invocation.
fn build_get_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // TODO(orizi): Add Range-Check usage.
    let requested_count = builder
        .program_info
        .metadata
        .gas_info
        .variable_values
        .get(&builder.idx)
        .ok_or(InvocationError::UnknownVariableData)?;
    let (range_check_expression, gas_counter_expression) = match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: gas_counter_expression, .. },
        ] => (range_check_expression, gas_counter_expression),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let gas_counter_value = try_extract_matches!(
        gas_counter_expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
    let range_check = try_extract_matches!(
        range_check_expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    let failure_handle_statement_id = match builder.invocation.branches.as_slice() {
        [
            BranchInfo { target: BranchTarget::Fallthrough, .. },
            BranchInfo { target: BranchTarget::Statement(statement_id), .. },
        ] => statement_id,
        _ => panic!("malformed invocation"),
    };

    let gas_counter_value_for_branches =
        gas_counter_value.apply_ap_change(ApChange::Known(1)).unwrap();
    // The code up to the success branch.
    let mut before_success_branch = casm! {
        %{ memory[ap + 0] = ((*requested_count + 1) as i128) < memory gas_counter_value %}
        jmp rel 0 if [ap + 0] != 0, ap++;

        // requested_count + 1 >= gas_counter_value => requested_count > gas_counter:
        // TODO(orizi): Make into one command when wider constants are supported.
        [ap + 0] = gas_counter_value_for_branches + (1 - *requested_count as i128), ap++;
        [ap + 0] = [ap - 1] * (-1), ap++;
        [ap - 1] = [[range_check.apply_ap_change(ApChange::Known(3)).unwrap()]];

        jmp rel 0; // Fixed in relocations.
    };
    let branch_offset = before_success_branch.current_code_offset;
    *extract_matches!(
        &mut extract_matches!(
            &mut before_success_branch.instructions[0].body,
            InstructionBody::Jnz
        )
        .jump_offset,
        DerefOrImmediate::Immediate
    ) = branch_offset.to_bigint().unwrap();
    let relocation_index = before_success_branch.instructions.len() - 1;
    let success_branch = casm! {
       // requested_count + 1 < gas_counter_value => requested_count <= gas_counter:
       [ap + 0] = gas_counter_value_for_branches + (-requested_count as i128), ap++;
       [ap - 1] = [[range_check.apply_ap_change(ApChange::Known(2)).unwrap()]];
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
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltOperator::Sub,
                    a: gas_counter_value.apply_ap_change(ApChange::Known(2)).unwrap(),
                    b: DerefOrImmediate::Immediate(requested_count.to_bigint().unwrap()),
                })),
            ]
            .into_iter(),
            vec![
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltOperator::Add,
                    a: range_check.apply_ap_change(ApChange::Known(3)).unwrap(),
                    b: DerefOrImmediate::from(1),
                })),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    gas_counter_value.apply_ap_change(ApChange::Known(3)).unwrap(),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}

/// Handles the refund gas invocation.
fn build_refund_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let requested_count = builder
        .program_info
        .metadata
        .gas_info
        .variable_values
        .get(&builder.idx)
        .ok_or(InvocationError::UnknownVariableData)?;
    let expression = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    let gas_counter_value = try_extract_matches!(
        expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    Ok(builder.build_only_reference_changes(
        [if *requested_count == 0 {
            ReferenceExpression::from_cell(CellExpression::Deref(gas_counter_value))
        } else {
            ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                op: FeltOperator::Add,
                a: gas_counter_value,
                b: DerefOrImmediate::Immediate(requested_count.to_bigint().unwrap()),
            }))
        }]
        .into_iter(),
    ))
}
