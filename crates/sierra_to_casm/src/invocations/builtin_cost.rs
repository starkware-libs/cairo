use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use num_bigint::BigInt;
use sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibFunc, BuiltinCostGetGasLibFunc, CostTokenType,
};
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::gas::STEP_COST;
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};
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
    let (range_check, gas_counter, builtin_cost) = match builder.refs {
        [
            ReferenceValue { expression: range_check_expression, .. },
            ReferenceValue { expression: gas_counter_expression, .. },
            ReferenceValue { expression: builtin_cost_expression, .. },
        ] => (
            range_check_expression.try_unpack_single()?.to_buffer(1)?,
            gas_counter_expression.try_unpack_single()?.to_deref()?,
            builtin_cost_expression.try_unpack_single()?.to_deref()?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);

    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    if !CostTokenType::iter().all(|token| variable_values.contains_key(&(builder.idx, *token))) {
        return Err(InvocationError::UnknownVariableData);
    }

    let mut casm_builder = CasmBuilder::default();
    let range_check = casm_builder.add_var(range_check);
    let gas_counter = casm_builder.add_var(ResOperand::Deref(gas_counter));
    let builtin_cost = casm_builder.add_var(ResOperand::Deref(builtin_cost));
    let token_requested_counts = CostTokenType::iter().filter_map(|token_type| {
        if *token_type == CostTokenType::Step {
            return None;
        };
        let requested_count = variable_values[&(builder.idx, *token_type)];
        if requested_count == 0 { None } else { Some((token_type, requested_count)) }
    });

    // The actual number of writes for calculating the requested gas amount.
    let initial_writes: i64 = token_requested_counts
        .clone()
        .map(|(_, requested_count)| if requested_count == 1 { 2 } else { 3 })
        .sum();
    let optimized_out_writes =
        (BuiltinCostGetGasLibFunc::cost_computation_max_steps() as i64) - initial_writes;

    let requested_steps = variable_values[&(builder.idx, CostTokenType::Step)];
    // The number of instructions that we may choose to add `ap++` to, not already taken by the
    // basic flow required allocs.
    let free_incrementing_instruction_count = 1;
    // The cost of this libfunc is computed assuming all the cost types are used (and all are > 1).
    // Since in practice this is rarely the case, refund according to the actual number of steps
    // produced by the libfunc, with an additional cost for `ap += <fix size>` if required.
    let refund_steps: i64 = if optimized_out_writes <= free_incrementing_instruction_count {
        optimized_out_writes
    } else {
        optimized_out_writes - 1
    };
    assert!(
        refund_steps >= 0,
        "Internal compiler error: BuiltinCostGetGasLibFunc::max_cost() is wrong."
    );
    let mut total_requested_count = casm_builder
        .add_var(ResOperand::Immediate(BigInt::from((requested_steps - refund_steps) * STEP_COST)));
    for _ in 0..optimized_out_writes {
        casm_builder.alloc_var();
    }
    if optimized_out_writes > free_incrementing_instruction_count {
        casm_builder.add_ap((optimized_out_writes - free_incrementing_instruction_count) as usize);
    }
    for (token_type, requested_count) in token_requested_counts {
        let offset = token_type.offset_in_builtin_costs();
        // Fetch the cost of a single instance.
        let single_cost_val = casm_builder.double_deref(builtin_cost, offset);

        casm_build_extend! {casm_builder,
            tempvar single_cost;
            assert single_cost = single_cost_val;
        };

        // If necessary, multiply by the number of instances.
        let multi_cost = if requested_count != 1 {
            let requested_count =
                casm_builder.add_var(ResOperand::Immediate(requested_count.into()));
            casm_build_extend! {casm_builder,
                tempvar multi_cost;
                assert multi_cost = single_cost * requested_count;
            };
            multi_cost
        } else {
            single_cost
        };
        // Add to the cumulative sum.
        casm_build_extend! {casm_builder,
            tempvar updated_total_requested_count;
            assert updated_total_requested_count = multi_cost + total_requested_count;
        };
        total_requested_count = updated_total_requested_count;
    }
    let uint128_limit = casm_builder.add_var(ResOperand::Immediate(BigInt::from(u128::MAX) + 1));

    casm_build_extend! {casm_builder,
        tempvar has_enough_gas;
        hint TestLessThanOrEqual {lhs: total_requested_count, rhs: gas_counter} into {dst: has_enough_gas};
        jump HasEnoughGas if has_enough_gas != 0;
        // In this case amount > gas_counter_value, so amount - gas_counter_value - 1 >= 0.
        tempvar gas_diff;
        assert gas_counter = gas_diff + total_requested_count;
        tempvar fixed_gas_diff;
        assert fixed_gas_diff = gas_diff + uint128_limit;
        assert *(range_check++) = fixed_gas_diff;
        jump Failure;
        HasEnoughGas:
        tempvar updated_gas;
        assert gas_counter = updated_gas + total_requested_count;
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
