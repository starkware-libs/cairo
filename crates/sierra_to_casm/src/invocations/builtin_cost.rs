use casm::ap_change::ApplyApChange;
use casm::casm;
use casm::operand::{CellRef, DerefOrImmediate, Register};
use itertools::chain;
use sierra::extensions::builtin_cost::{BuiltinCostConcreteLibFunc, BuiltinGetGasConcreteLibFunc};
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
        BuiltinCostConcreteLibFunc::BuiltinGetGas(libfunc) => {
            build_builtin_get_gas(libfunc, builder)
        }
    }
}

/// Handles the get gas invocation.
fn build_builtin_get_gas(
    libfunc: &BuiltinGetGasConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // TODO(lior): Share code with get_gas().
    let requested_count = builder
        .program_info
        .metadata
        .gas_info
        .variable_values
        .get(&(builder.idx, libfunc.token_type))
        .ok_or(InvocationError::UnknownVariableData)?;
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
    let builtin_cost = try_extract_matches!(
        builtin_cost_expression
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

    // The code up to the success branch.
    let mut before_success_branch = casm! {
        // Compute the requested amount of gas.
        [ap + 0] = builtin_cost * (*requested_count), ap++;
        // Non-deterministically check if there is enough gas.
        %{
            memory[ap + 0] = memory[ap - 1] <=
                memory (gas_counter_value.unchecked_apply_known_ap_change(1))
        %}
        jmp rel 0 if [ap + 0] != 0, ap++;

        // In this case amount > gas_counter_value, so amount - gas_counter_value - 1 >= 0.
        [ap - 2] = [ap + 0] + (gas_counter_value.unchecked_apply_known_ap_change(2)), ap++;
        [ap + 0] = [ap - 1] + (-1), ap++;
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(4)]];

        jmp rel 0; // Fixed in relocations.
    };
    patch_jnz_to_end(&mut before_success_branch, 1);
    let relocation_index = before_success_branch.instructions.len() - 1;
    let success_branch = casm! {
       // Compute the remaining gas and check that it is nonnegative.
       (gas_counter_value.unchecked_apply_known_ap_change(2)) = [ap + 0] + [ap - 2], ap++;
       [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];
    };

    Ok(builder.build(
        chain!(before_success_branch.instructions, success_branch.instructions).collect(),
        vec![RelocationEntry {
            instruction_idx: relocation_index,
            relocation: Relocation::RelativeStatementId(*failure_handle_statement_id),
        }],
        [
            vec![
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: range_check.unchecked_apply_known_ap_change(3),
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
                    a: range_check.unchecked_apply_known_ap_change(4),
                    b: DerefOrImmediate::from(1),
                })),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    gas_counter_value.unchecked_apply_known_ap_change(4),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}
