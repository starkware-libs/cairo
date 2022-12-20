use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::{DerefOrImmediate, ResOperand};
use num_bigint::BigInt;
use sierra::extensions::builtin_cost::CostTokenType;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::extensions::gas::GasConcreteLibFunc;
use sierra_ap_change::core_libfunc_ap_change;
use utils::try_extract_matches;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

pub const STEP_COST: i64 = 100;

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &GasConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasConcreteLibFunc::GetGas(_) => build_get_gas(builder),
        GasConcreteLibFunc::RefundGas(_) => build_refund_gas(builder),
    }
}

/// Handles the get gas invocation.
fn build_get_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let requested_count = builder
        .program_info
        .metadata
        .gas_info
        .variable_values
        .get(&(builder.idx, CostTokenType::Step))
        .ok_or(InvocationError::UnknownVariableData)?
        * STEP_COST;
    let (range_check, gas_counter_value) = match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: gas_counter_expression, .. },
        ] => (
            range_check_expression.try_unpack_single()?.to_buffer(1)?,
            gas_counter_expression.try_unpack_single()?.to_deref()?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);

    let mut casm_builder = CasmBuilder::default();
    let range_check = casm_builder.add_var(range_check);
    let gas_counter = casm_builder.add_var(ResOperand::Deref(gas_counter_value));
    let gas_counter_fix =
        casm_builder.add_var(ResOperand::Immediate(BigInt::from(u128::MAX) + 1 - requested_count));
    let requested_count = casm_builder.add_var(ResOperand::Immediate(requested_count.into()));

    casm_build_extend! {casm_builder,
        tempvar has_enough_gas;
        hint TestLessThanOrEqual {lhs: requested_count, rhs: gas_counter} into {dst: has_enough_gas};
        jump HasEnoughGas if has_enough_gas != 0;
        tempvar gas_diff;
        assert gas_diff = gas_counter + gas_counter_fix;
        assert *(range_check++) = gas_diff;
        jump Failure;
        HasEnoughGas:
        tempvar updated_gas;
        assert gas_counter = updated_gas + requested_count;
        assert *(range_check++) = updated_gas;
    };

    let CasmBuildResult { instructions, awaiting_relocations, label_state, fallthrough_state } =
        casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["Failure"].ap_change]
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
                    fallthrough_state.get_adjusted_as_cell_ref(updated_gas),
                )),
            ]
            .into_iter(),
            vec![
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    label_state["Failure"].get_adjusted(range_check),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    label_state["Failure"].get_adjusted_as_cell_ref(gas_counter),
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
        .get(&(builder.idx, CostTokenType::Step))
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
                op: FeltBinaryOperator::Add,
                a: gas_counter_value,
                b: DerefOrImmediate::Immediate(BigInt::from(*requested_count)),
            }))
        }]
        .into_iter(),
    ))
}
