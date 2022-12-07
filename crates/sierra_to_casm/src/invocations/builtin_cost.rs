use casm::ap_change::ApplyApChange;
use casm::casm;
use casm::operand::{CellRef, DerefOrImmediate, Register};
use itertools::chain;
use sierra::extensions::builtin_cost::{BuiltinCostConcreteLibFunc, BuiltinGetGasConcreteLibFunc};
use sierra::extensions::felt::FeltOperator;
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
    let (range_check_expression, gas_counter_expression, _builtin_cost_expression) =
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

    let failure_handle_statement_id = match builder.invocation.branches.as_slice() {
        [
            BranchInfo { target: BranchTarget::Fallthrough, .. },
            BranchInfo { target: BranchTarget::Statement(statement_id), .. },
        ] => statement_id,
        _ => panic!("malformed invocation"),
    };

    // TODO(lior): Multiply requested_count by a dynamic value.

    // The code up to the success branch.
    let mut before_success_branch = casm! {
        %{ memory[ap + 0] = ((*requested_count - 1) as i128) < memory gas_counter_value %}
        jmp rel 0 if [ap + 0] != 0, ap++;

        // requested_count - 1 >= gas_counter_value => requested_count > gas_counter:
        // TODO(orizi): Make into one command when wider constants are supported.
        [ap + 0] = (gas_counter_value.unchecked_apply_known_ap_change(1)) + (1 - *requested_count as i128), ap++;
        [ap + 0] = [ap - 1] * (-1), ap++;
        [ap - 1] = [[range_check.unchecked_apply_known_ap_change(3)]];

        jmp rel 0; // Fixed in relocations.
    };
    patch_jnz_to_end(&mut before_success_branch, 0);
    let relocation_index = before_success_branch.instructions.len() - 1;
    let success_branch = casm! {
       // requested_count - 1 < gas_counter_value => requested_count <= gas_counter:
       [ap + 0] = (gas_counter_value.unchecked_apply_known_ap_change(1)) + (-requested_count as i128), ap++;
       [ap - 1] = [[range_check.unchecked_apply_known_ap_change(2)]];
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
                    op: FeltOperator::Add,
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
                    op: FeltOperator::Add,
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
