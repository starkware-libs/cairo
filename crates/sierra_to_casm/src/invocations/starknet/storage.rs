use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use num_bigint::BigInt;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

#[cfg(test)]
#[path = "storage_test.rs"]
mod test;

/// Builds instructions for StarkNet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let selector_imm = BigInt::from_bytes_le(num_bigint::Sign::Plus, "storage_read".as_bytes());
    let (original_system, storage_address) = match builder.refs {
        [
            ReferenceValue { expression: expr_system, .. },
            ReferenceValue { expression: expr_address, .. },
        ] => (
            expr_system.try_unpack_single()?.to_buffer(3)?,
            expr_address.try_unpack_single()?.to_deref()?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(original_system.clone());
    let original_system = casm_builder.add_var(original_system);
    let selector_imm = casm_builder.add_var(ResOperand::Immediate(selector_imm));
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));
    casm_build_extend! {casm_builder,
        tempvar selector;
        assert selector = selector_imm;
        assert *(system++) = selector;
        assert *(system++) = storage_address;
        hint SystemCall { system: original_system };
        tempvar read_value;
        assert *(system++) = read_value;
    };

    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [sierra_ap_change::ApChange::Known(fallthrough_state.ap_change)]
    );
    Ok(builder.build(
        instructions,
        vec![],
        [vec![
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                fallthrough_state.get_adjusted(system),
            )),
            ReferenceExpression::from_cell(CellExpression::Deref(
                fallthrough_state.get_adjusted_as_cell_ref(read_value),
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

    let (gas_builtin, original_system, storage_address, value) = match builder.refs {
        [
            ReferenceValue { expression: expr_gas_builtin, .. },
            ReferenceValue { expression: expr_system, .. },
            ReferenceValue { expression: expr_address, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => (
            expr_gas_builtin.try_unpack_single()?.to_deref()?,
            expr_system.try_unpack_single()?.to_buffer(6)?,
            expr_address.try_unpack_single()?.to_deref()?,
            expr_value.try_unpack_single()?.to_deref()?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 4,
                actual: refs.len(),
            });
        }
    };
    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(original_system.clone());
    let original_system = casm_builder.add_var(original_system);
    let selector_imm = casm_builder.add_var(ResOperand::Immediate(selector_imm));
    let gas_builtin = casm_builder.add_var(ResOperand::Deref(gas_builtin));
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));
    let value = casm_builder.add_var(ResOperand::Deref(value));
    casm_build_extend! {casm_builder,
        tempvar selector;
        assert selector = selector_imm;
        assert *(system++) = selector;
        assert *(system++) = gas_builtin;
        assert *(system++) = storage_address;
        assert *(system++) = value;
        hint SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        tempvar revert_reason;
        assert *(system++) = revert_reason;
        let _ignore = *(system++);
        jump Failure if revert_reason != 0;
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
                    label_state["Failure"].get_adjusted(updated_gas_builtin),
                )),
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    label_state["Failure"].get_adjusted(system),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    label_state["Failure"].get_adjusted_as_cell_ref(revert_reason),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}
