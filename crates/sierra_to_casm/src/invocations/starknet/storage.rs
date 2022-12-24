use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use num_bigint::BigInt;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{CellExpression, ReferenceExpression};
use crate::relocations::{Relocation, RelocationEntry};

#[cfg(test)]
#[path = "storage_test.rs"]
mod test;

/// Builds instructions for StarkNet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let selector_imm = BigInt::from_bytes_le(num_bigint::Sign::Plus, "storage_read".as_bytes());

    let [expr_system, expr_address] = builder.try_get_refs()?;
    let system = expr_system.try_unpack_single()?.to_buffer(3)?;
    let storage_address = expr_address.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(system);
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert storage_address = *(system++);
        hint SystemCall { system: original_system };
        tempvar read_value = *(system++);
    };

    let CasmBuildResult { instructions, branches: [(state, _)] } =
        casm_builder.build(["Fallthrough"]);
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [sierra_ap_change::ApChange::Known(state.ap_change)]
    );
    Ok(builder.build(
        instructions,
        vec![],
        [vec![
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                state.get_adjusted(system),
            )),
            ReferenceExpression::from_cell(CellExpression::Deref(
                state.get_adjusted_as_cell_ref(read_value),
            )),
        ]
        .into_iter()]
        .into_iter(),
    ))
}

/// Builds instructions for StarkNet write system call.
pub fn build_storage_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_le(num_bigint::Sign::Plus, "storage_write".as_bytes());

    let [expr_gas_builtin, expr_system, expr_address, expr_value] = builder.try_get_refs()?;
    let gas_builtin = expr_gas_builtin.try_unpack_single()?.to_deref()?;
    let system = expr_system.try_unpack_single()?.to_buffer(6)?;
    let storage_address = expr_address.try_unpack_single()?.to_deref()?;
    let value = expr_value.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(system);
    let gas_builtin = casm_builder.add_var(ResOperand::Deref(gas_builtin));
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));
    let value = casm_builder.add_var(ResOperand::Deref(value));
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert storage_address = *(system++);
        assert value = *(system++);
        hint SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        tempvar revert_reason = *(system++);
        let _ignore = *(system++);
        jump Failure if revert_reason != 0;
    };

    let CasmBuildResult {
        instructions,
        branches: [(fallthrough_state, _), (failure_state, awaiting_relocations)],
    } = casm_builder.build(["Fallthrough", "Failure"]);
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, failure_state.ap_change]
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
            // Success branch - return (gas builtin, system)
            vec![
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    fallthrough_state.get_adjusted(updated_gas_builtin),
                )),
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    fallthrough_state.get_adjusted(system),
                )),
            ]
            .into_iter(),
            // Failure branch - return (gas builtin, system, revert_reason)
            vec![
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    failure_state.get_adjusted(updated_gas_builtin),
                )),
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    failure_state.get_adjusted(system),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    failure_state.get_adjusted_as_cell_ref(revert_reason),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}
