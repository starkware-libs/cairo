use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::operand::{CellRef, DerefOrImmediate, Register};
use cairo_lang_sierra::extensions::gas::{CostTokenType, GasConcreteLibfunc};
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use num_bigint::BigInt;

use super::misc::get_pointer_after_program_code;
use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};
use crate::references::ReferenceExpression;
use crate::relocations::InstructionsWithRelocations;

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &GasConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasConcreteLibfunc::WithdrawGas(_) => build_withdraw_gas(builder),
        GasConcreteLibfunc::RedepositGas(_) => build_redeposit_gas(builder),
        GasConcreteLibfunc::GetAvailableGas(_) => misc::build_dup(builder),
        GasConcreteLibfunc::BuiltinWithdrawGas(_) => build_builtin_withdraw_gas(builder),
        GasConcreteLibfunc::GetBuiltinCosts(_) => build_get_builtin_costs(builder),
    }
}

/// Handles the withdraw_gas invocation.
fn build_withdraw_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, gas_counter] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref gas_counter;
    };
    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    validate_all_casm_token_vars_available(variable_values, builder.idx)?;

    // Check if we need to fetch the builtin cost table.
    if CostTokenType::iter_precost().any(|token| variable_values[&(builder.idx, *token)] > 0) {
        let (pre_instructions, cost_builtin_ptr) =
            add_cost_builtin_ptr_fetch_code(&mut casm_builder);
        casm_build_extend!(casm_builder, tempvar cost_builtin = cost_builtin_ptr;);
        return build_withdraw_gas_given_cost_table(
            builder,
            casm_builder,
            [range_check, gas_counter, cost_builtin],
            pre_instructions,
        );
    }
    let requested_count: i64 = variable_values
        .get(&(builder.idx, CostTokenType::Const))
        .copied()
        .ok_or(InvocationError::UnknownVariableData)?;

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

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
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
    let [gas_counter] = builder.try_get_single_cells()?;
    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    validate_all_casm_token_vars_available(variable_values, builder.idx)?;
    let requested_count: i64 = variable_values[&(builder.idx, CostTokenType::Const)];
    // Check if we need to fetch the builtin cost table.
    if CostTokenType::iter_precost().all(|token| variable_values[&(builder.idx, *token)] == 0) {
        let gas_counter_value =
            gas_counter.to_deref().ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

        return Ok(builder.build_only_reference_changes(
            [if requested_count == 0 {
                ReferenceExpression::from_cell(CellExpression::Deref(gas_counter_value))
            } else {
                ReferenceExpression::from_cell(CellExpression::BinOp {
                    op: CellOperator::Add,
                    a: gas_counter_value,
                    b: DerefOrImmediate::Immediate(requested_count.into()),
                })
            }]
            .into_iter(),
        ));
    }
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref gas_counter;
    };
    let (pre_instructions, cost_builtin_ptr) = add_cost_builtin_ptr_fetch_code(&mut casm_builder);
    casm_build_extend! {casm_builder,
        tempvar builtin_cost = cost_builtin_ptr;
    };
    let (_, total_requested_count) =
        add_get_total_requested_count_code(&builder, &mut casm_builder, builtin_cost)?;

    casm_build_extend! {casm_builder,
        let updated_gas = gas_counter + total_requested_count;
    };
    Ok(builder.build_from_casm_builder_ex(
        casm_builder,
        [("Fallthrough", &[&[updated_gas]], None)],
        CostValidationInfo { range_check_info: None, extra_costs: Some([requested_count as i32]) },
        pre_instructions,
    ))
}

/// Handles the withdraw_gas invocation with the builtin costs argument.
fn build_builtin_withdraw_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, gas_counter, builtin_cost] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref gas_counter;
        deref builtin_cost;
    };
    validate_all_casm_token_vars_available(
        &builder.program_info.metadata.gas_info.variable_values,
        builder.idx,
    )?;
    build_withdraw_gas_given_cost_table(
        builder,
        casm_builder,
        [range_check, gas_counter, builtin_cost],
        Default::default(),
    )
}

/// Builds the instructions for the `withdraw_gas` libfunc given the builtin cost table.
fn build_withdraw_gas_given_cost_table(
    builder: CompiledInvocationBuilder<'_>,
    mut casm_builder: CasmBuilder,
    [range_check, gas_counter, builtin_cost]: [Var; 3],
    pre_instructions: InstructionsWithRelocations,
) -> Result<CompiledInvocation, InvocationError> {
    let (requested_count, total_requested_count) =
        add_get_total_requested_count_code(&builder, &mut casm_builder, builtin_cost)?;

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar has_enough_gas;
        hint TestLessThanOrEqual {
            lhs: total_requested_count,
            rhs: gas_counter
        } into {dst: has_enough_gas};
        jump HasEnoughGas if has_enough_gas != 0;
        // In this case amount > gas_counter_value, so amount - gas_counter_value - 1 >= 0.
        tempvar gas_diff = gas_counter - total_requested_count;
        const uint128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
        tempvar fixed_gas_diff = gas_diff + uint128_limit;
        assert fixed_gas_diff = *(range_check++);
        jump Failure;
        HasEnoughGas:
        tempvar updated_gas = gas_counter - total_requested_count;
        assert updated_gas = *(range_check++);
    };
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder_ex(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[updated_gas]], None),
            ("Failure", &[&[range_check], &[gas_counter]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: Some([-requested_count as i32, 0]),
        },
        pre_instructions,
    ))
}

/// Adds the code for calculating the total requested count of gas to update the counter with.
/// Returns the requested *const* cost and a variable containing the amount to update the
/// gas counter with (total cost).
fn add_get_total_requested_count_code(
    builder: &CompiledInvocationBuilder<'_>,
    casm_builder: &mut CasmBuilder,
    builtin_cost: Var,
) -> Result<(i64, Var), InvocationError> {
    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    let requested_count: i64 = variable_values[&(builder.idx, CostTokenType::Const)];
    let mut total_requested_count =
        casm_builder.add_var(CellExpression::Immediate(BigInt::from(requested_count)));
    for token_type in CostTokenType::iter_precost() {
        let requested_count = variable_values[&(builder.idx, *token_type)];
        if requested_count == 0 {
            continue;
        }
        let offset = token_type.offset_in_builtin_costs();
        // Fetch the cost of a single instance.
        casm_build_extend! {casm_builder,
            tempvar single_cost = builtin_cost[offset];
        };

        // If necessary, multiply by the number of instances.
        let multi_cost = if requested_count != 1 {
            casm_build_extend! {casm_builder,
                const requested_count = requested_count;
                tempvar multi_cost = single_cost * requested_count;
            };
            multi_cost
        } else {
            single_cost
        };
        // Add to the cumulative sum.
        casm_build_extend! {casm_builder,
            tempvar updated_total_requested_count = multi_cost + total_requested_count;
        };
        total_requested_count = updated_total_requested_count;
    }
    Ok((requested_count, total_requested_count))
}

/// Validates that all the cost token variables are available for statement at `idx`.
fn validate_all_casm_token_vars_available(
    variable_values: &OrderedHashMap<(StatementIdx, CostTokenType), i64>,
    idx: StatementIdx,
) -> Result<(), InvocationError> {
    for token in CostTokenType::iter_casm_tokens() {
        if !variable_values.contains_key(&(idx, *token)) {
            return Err(InvocationError::UnknownVariableData);
        }
    }
    Ok(())
}

/// Handles the get_builtin_costs invocation.
fn build_get_builtin_costs(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();
    let (pre_instructions, cost_builtin_ptr) = add_cost_builtin_ptr_fetch_code(&mut casm_builder);
    Ok(builder.build_from_casm_builder_ex(
        casm_builder,
        [("Fallthrough", &[&[cost_builtin_ptr]], None)],
        Default::default(),
        pre_instructions,
    ))
}

/// Adds the code for fetching the builtin cost table.
/// Returns the pre-instructions to be provided to
/// `CompiledInvocationBuilder::build_from_casm_builder_ex` and the variable representing the
/// builtin table pointer.
fn add_cost_builtin_ptr_fetch_code(
    casm_builder: &mut CasmBuilder,
) -> (InstructionsWithRelocations, Var) {
    const COST_TABLE_OFFSET: i32 = 1;
    let (pre_instructions, ap_change) = get_pointer_after_program_code(COST_TABLE_OFFSET);
    casm_builder.increase_ap_change(ap_change);
    (
        pre_instructions,
        casm_builder.add_var(CellExpression::DoubleDeref(
            CellRef { register: Register::AP, offset: (ap_change - 1).into_or_panic() },
            0,
        )),
    )
}
