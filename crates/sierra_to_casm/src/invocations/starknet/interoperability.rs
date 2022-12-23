use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use num_bigint::BigInt;
use sierra::extensions::consts::SignatureAndConstConcreteLibFunc;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{CellExpression, ReferenceExpression};
use crate::relocations::{Relocation, RelocationEntry};

#[cfg(test)]
#[path = "interoperability_test.rs"]
mod test;

/// Builds instructions for StarkNet call contract system call.
pub fn build_call_contract(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_le(num_bigint::Sign::Plus, "call_contract".as_bytes());

    let [expr_gas_builtin, expr_system, expr_address, expr_arr] = builder.try_get_refs()?;
    let gas_builtin = expr_gas_builtin.try_unpack_single()?.to_deref()?;
    let system = expr_system.try_unpack_single()?.to_buffer(8)?;
    let contract_address = expr_address.try_unpack_single()?.to_deref()?;
    let [call_data_start, call_data_end] = expr_arr.try_unpack()?;
    let call_data_start = call_data_start.to_deref()?;
    let call_data_end = call_data_end.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(system);
    let gas_builtin = casm_builder.add_var(ResOperand::Deref(gas_builtin));
    let contract_address = casm_builder.add_var(ResOperand::Deref(contract_address));
    let call_data_start = casm_builder.add_var(ResOperand::Deref(call_data_start));
    let call_data_end = casm_builder.add_var(ResOperand::Deref(call_data_end));
    casm_build_extend! {casm_builder,
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        let original_system = system;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert contract_address = *(system++);
        assert call_data_start = *(system++);
        assert call_data_end = *(system++);
        hint SystemCall { system: original_system };

        let updated_gas_builtin = *(system++);
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        tempvar revert_reason = *(system++);
        let res_start = *(system++);
        let res_end = *(system++);
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
            // Success branch - return (gas builtin, system, result_array)
            vec![
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    fallthrough_state.get_adjusted(updated_gas_builtin),
                )),
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    fallthrough_state.get_adjusted(system),
                )),
                ReferenceExpression {
                    cells: vec![
                        CellExpression::from_res_operand(fallthrough_state.get_adjusted(res_start)),
                        CellExpression::from_res_operand(fallthrough_state.get_adjusted(res_end)),
                    ],
                },
            ]
            .into_iter(),
            // Failure branch - return (gas builtin, system, revert_reason, result_array)
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
                ReferenceExpression {
                    cells: vec![
                        CellExpression::from_res_operand(
                            label_state["Failure"].get_adjusted(res_start),
                        ),
                        CellExpression::from_res_operand(
                            label_state["Failure"].get_adjusted(res_end),
                        ),
                    ],
                },
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}

/// Handles the storage_address_const libfunc.
pub fn build_contract_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibFunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = BigInt::from(1) << 251;
    if libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))].into_iter(),
    ))
}
