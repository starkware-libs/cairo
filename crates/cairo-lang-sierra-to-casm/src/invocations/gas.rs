use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::operand::DerefOrImmediate;
use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::gas::GasConcreteLibfunc;
use num_bigint::BigInt;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &GasConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasConcreteLibfunc::WithdrawGas(_) => build_withdraw_gas(builder),
        GasConcreteLibfunc::RedepositGas(_) => build_redeposit_gas(builder),
        GasConcreteLibfunc::GetAvailableGas(_) => misc::build_dup(builder),
    }
}

/// Handles the withdraw_gas invocation.
fn build_withdraw_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    let requested_count: i64 = variable_values
        .get(&(builder.idx, CostTokenType::Const))
        .copied()
        .ok_or(InvocationError::UnknownVariableData)?;
    let [range_check, gas_counter] = builder.try_get_single_cells()?;

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref gas_counter;
    };

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar has_enough_gas;
        const requested_count_imm = requested_count;
        hint TestLessThanOrEqual {
            lhs: requested_count_imm,
            rhs: gas_counter
        } into {dst: has_enough_gas};
        jump HasEnoughGas if has_enough_gas != 0;
        const gas_counter_fix = (BigInt::from(u128::MAX) + 1 - requested_count) as BigInt;
        tempvar gas_diff = gas_counter + gas_counter_fix;
        assert gas_diff = *(range_check++);
        jump Failure;
        HasEnoughGas:
        tempvar updated_gas = gas_counter - requested_count_imm;
        assert updated_gas = *(range_check++);
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[updated_gas]], None),
            ("Failure", &[&[range_check], &[gas_counter]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: Some([-requested_count as i32, 0]),
        },
    ))
}

/// Handles the redeposit_gas invocation.
fn build_redeposit_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    let requested_count: i64 = variable_values
        .get(&(builder.idx, CostTokenType::Const))
        .copied()
        .ok_or(InvocationError::UnknownVariableData)?;
    let gas_counter_value = builder.try_get_single_cells::<1>()?[0]
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    Ok(builder.build_only_reference_changes(
        [if requested_count == 0 {
            ReferenceExpression::from_cell(CellExpression::Deref(gas_counter_value))
        } else {
            ReferenceExpression::from_cell(CellExpression::BinOp {
                op: CellOperator::Add,
                a: gas_counter_value,
                b: DerefOrImmediate::Immediate(BigInt::from(requested_count)),
            })
        }]
        .into_iter(),
    ))
}
