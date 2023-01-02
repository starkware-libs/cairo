use cairo_casm::builder::CasmBuilder;
use cairo_casm::casm_build_extend;
use cairo_casm::operand::ResOperand;
use cairo_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc, CostTokenType,
};
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::gas::STEP_COST;
use crate::invocations::get_non_fallthrough_statement_id;

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &BuiltinCostConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BuiltinCostConcreteLibfunc::BuiltinGetGas(_) => build_builtin_get_gas(builder),
    }
}

/// Handles the get gas invocation.
fn build_builtin_get_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // TODO(lior): Share code with get_gas().
    let [range_check_expr, gas_counter_expr, builtin_cost_expr] = builder.try_get_refs()?;
    let range_check = range_check_expr.try_unpack_single()?.to_buffer(1)?;
    let gas_counter = gas_counter_expr.try_unpack_single()?.to_deref()?;
    let builtin_cost = builtin_cost_expr.try_unpack_single()?.to_deref()?;

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
        (BuiltinCostGetGasLibfunc::cost_computation_max_steps() as i64) - initial_writes;

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
        "Internal compiler error: BuiltinCostGetGasLibfunc::max_cost() is wrong."
    );
    let mut total_requested_count = casm_builder
        .add_var(ResOperand::Immediate(BigInt::from((requested_steps - refund_steps) * STEP_COST)));
    for _ in 0..optimized_out_writes {
        casm_builder.alloc_var(false);
    }
    if optimized_out_writes > free_incrementing_instruction_count {
        casm_builder.add_ap((optimized_out_writes - free_incrementing_instruction_count) as usize);
    }
    for (token_type, requested_count) in token_requested_counts {
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

    casm_build_extend! {casm_builder,
        tempvar has_enough_gas;
        hint TestLessThanOrEqual {lhs: total_requested_count, rhs: gas_counter} into {dst: has_enough_gas};
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
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[updated_gas]], None),
            ("Failure", &[&[range_check], &[gas_counter]], Some(failure_handle_statement_id)),
        ],
    ))
}
