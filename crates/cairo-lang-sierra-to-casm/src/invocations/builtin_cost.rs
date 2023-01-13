use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc, CostTokenType,
};
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::gas::STEP_COST;
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra gas operations.
pub fn build(
    libfunc: &BuiltinCostConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BuiltinCostConcreteLibfunc::BuiltinGetGas(_) => build_builtin_get_gas(builder),
        BuiltinCostConcreteLibfunc::GetBuiltinCosts(_) => build_get_builtin_costs(builder),
    }
}

/// Handles the get gas invocation.
fn build_builtin_get_gas(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    // TODO(lior): Share code with get_gas().
    let [range_check, gas_counter, builtin_cost] = builder.try_get_single_cells()?;

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);

    let variable_values = &builder.program_info.metadata.gas_info.variable_values;
    if !CostTokenType::iter().all(|token| variable_values.contains_key(&(builder.idx, *token))) {
        return Err(InvocationError::UnknownVariableData);
    }

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref gas_counter;
        deref builtin_cost;
    };

    // The actual number of writes for calculating the requested gas amount.
    let optimized_out_writes = (BuiltinCostGetGasLibfunc::cost_computation_max_steps() as i64)
        - (BuiltinCostGetGasLibfunc::cost_computation_steps(|token_type| {
            variable_values[(builder.idx, token_type)] as usize
        }) as i64);

    let requested_steps = variable_values[(builder.idx, CostTokenType::Step)];
    // The cost of this libfunc is computed assuming all the cost types are used (and all are > 1).
    // Since in practice this is rarely the case, refund according to the actual number of steps
    // produced by the libfunc.
    let refund_steps = optimized_out_writes;
    assert!(
        refund_steps >= 0,
        "Internal compiler error: BuiltinCostGetGasLibfunc::max_cost() is wrong."
    );
    let mut total_requested_count = casm_builder.add_var(CellExpression::Immediate(BigInt::from(
        (requested_steps - refund_steps) * STEP_COST,
    )));
    for token_type in CostTokenType::iter() {
        if *token_type == CostTokenType::Step {
            continue;
        }
        let requested_count = variable_values[(builder.idx, *token_type)];
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

/// Handles the get gas invocation.
fn build_get_builtin_costs(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let code = casm! {
        // The relocation table will point the `call` to the end of the program where there will
        // be a `ret` instruction.
        call rel 0;
        // After calling an empty function, `[ap - 1]` contains the current `pc`.
        // Using the relocations below, the immediate value (`1`) will be changed so that it will
        // compute a pointer to the second cell after the end of the program, which will contain
        // the pointer to the builtin cost array.
        [ap] = [ap - 1] + 1, ap++;
    };
    let relocations = vec![
        RelocationEntry { instruction_idx: 0, relocation: Relocation::EndOfProgram },
        RelocationEntry { instruction_idx: 1, relocation: Relocation::EndOfProgram },
    ];
    Ok(builder.build(
        code.instructions,
        relocations,
        [vec![ReferenceExpression::from_cell(CellExpression::DoubleDeref(
            CellRef { register: Register::AP, offset: -1 },
            0,
        ))]
        .into_iter()]
        .into_iter(),
    ))
}
