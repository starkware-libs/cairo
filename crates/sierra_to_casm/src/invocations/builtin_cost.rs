use casm::ap_change::ApplyApChange;
use casm::operand::{CellRef, DerefOrImmediate, Register};
use casm::{casm, casm_extend};
use itertools::chain;
use sierra::extensions::builtin_cost::{BuiltinCostConcreteLibFunc, CostTokenType};
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::program::{BranchInfo, BranchTarget};
use utils::try_extract_matches;

use super::{patch_jnz_to_end, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &BuiltinCostConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BuiltinCostConcreteLibFunc::BuiltinGetGas(_) => build_builtin_get_gas(builder),
    }
}

/// Handles the get gas invocation.
fn build_builtin_get_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // TODO(lior): Share code with get_gas().
    let (range_check_expression, gas_counter_expression, builtin_cost_expression) =
        match builder.refs {
            [
                ReferenceValue { expression: range_check_expression, .. },
                ReferenceValue { expression: gas_counter_expression, .. },
                ReferenceValue { expression: builtin_cost_expression, .. },
            ] => (range_check_expression, gas_counter_expression, builtin_cost_expression),
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: refs.len(),
                });
            }
        };

    let failure_handle_statement_id = match builder.invocation.branches.as_slice() {
        [
            BranchInfo { target: BranchTarget::Fallthrough, .. },
            BranchInfo { target: BranchTarget::Statement(statement_id), .. },
        ] => statement_id,
        _ => panic!("malformed invocation"),
    };

    let builtin_cost = try_extract_matches!(
        builtin_cost_expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    let variable_values = &builder.program_info.metadata.gas_info.variable_values;

    // Compute the requested amount of gas. Start with [CostTokenType::Step].
    let requested_steps = *variable_values
        .get(&(builder.idx, CostTokenType::Step))
        .ok_or(InvocationError::UnknownVariableData)?;
    let mut compute_requested_amount = casm! { [ap] = requested_steps, ap++; };
    let mut compute_requested_amount_ap_change: usize = 1;
    for token_type in CostTokenType::iter() {
        if *token_type == CostTokenType::Step {
            continue;
        }
        let requested_count = variable_values
            .get(&(builder.idx, *token_type))
            .ok_or(InvocationError::UnknownVariableData)?;
        let translated_builtin_cost =
            builtin_cost.unchecked_apply_known_ap_change(compute_requested_amount_ap_change);
        casm_extend!(
            compute_requested_amount,
            // TODO(lior): Fetch the cost of each builtin separately.
            [ap] = translated_builtin_cost * (*requested_count), ap++;
            [ap] = [ap - 2] + [ap - 1], ap++;
        );
        compute_requested_amount_ap_change += 2;
    }

    let gas_counter_value = try_extract_matches!(
        gas_counter_expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?
    .unchecked_apply_known_ap_change(compute_requested_amount_ap_change);
    let range_check = try_extract_matches!(
        range_check_expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?
    .unchecked_apply_known_ap_change(compute_requested_amount_ap_change);

    // The code up to the success branch.
    let mut before_success_branch = casm! {
        // Non-deterministically check if there is enough gas.
        %{
            memory[ap + 0] = memory[ap - 1] <= memory gas_counter_value
        %}
        jmp rel 0 if [ap + 0] != 0, ap++;

        // In this case amount > gas_counter_value, so amount - gas_counter_value - 1 >= 0.
        [ap - 2] = [ap + 0] + (gas_counter_value.unchecked_apply_known_ap_change(1)), ap++;
        [ap + 0] = [ap - 1] + (-1), ap++;
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];

        jmp rel 0; // Fixed in relocations.
    };
    patch_jnz_to_end(&mut before_success_branch, 0);
    let relocation_index =
        compute_requested_amount.instructions.len() + before_success_branch.instructions.len() - 1;
    let success_branch = casm! {
       // Compute the remaining gas and check that it is nonnegative.
       (gas_counter_value.unchecked_apply_known_ap_change(1)) = [ap + 0] + [ap - 2], ap++;
       [ap - 1] = [[range_check.unchecked_apply_known_ap_change(2)]];
    };

    Ok(builder.build(
        chain!(
            compute_requested_amount.instructions,
            before_success_branch.instructions,
            success_branch.instructions
        )
        .collect(),
        vec![RelocationEntry {
            instruction_idx: relocation_index,
            relocation: Relocation::RelativeStatementId(*failure_handle_statement_id),
        }],
        [
            vec![
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(2),
                    b: DerefOrImmediate::from(1),
                })),
                ReferenceExpression::from_cell(CellExpression::Deref(CellRef {
                    register: Register::AP,
                    offset: -1,
                })),
            ]
            .into_iter(),
            vec![
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(3),
                    b: DerefOrImmediate::from(1),
                })),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    gas_counter_value.unchecked_apply_known_ap_change(3),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}
