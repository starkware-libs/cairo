use cairo_casm::builder::CasmBuilder;
use cairo_casm::casm_build_extend;
use cairo_casm::operand::ResOperand;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;

#[cfg(test)]
#[path = "storage_test.rs"]
mod test;

/// Builds instructions for StarkNet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "StorageRead".as_bytes());

    let [expr_gas_builtin, expr_system, expr_address_domain, expr_address] =
        builder.try_get_refs()?;
    let gas_builtin = expr_gas_builtin.try_unpack_single()?.to_deref()?;
    let system = expr_system.try_unpack_single()?.to_buffer(3)?;
    let address_domain = expr_address_domain.try_unpack_single()?.to_deref()?;
    let storage_address = expr_address.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let gas_builtin = casm_builder.add_var(ResOperand::Deref(gas_builtin));
    let system = casm_builder.add_var(system);
    let address_domain = casm_builder.add_var(ResOperand::Deref(address_domain));
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));

    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert address_domain = *(system++);
        assert storage_address = *(system++);
        hint SystemCall { system: original_system };
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        let updated_gas_builtin = *(system++);
        tempvar revert_reason = *(system++);
        let read_value = *(system++);
        jump Failure if revert_reason != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[system], &[read_value]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[revert_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
    ))
}

/// Builds instructions for StarkNet write system call.
pub fn build_storage_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "StorageWrite".as_bytes());

    let [expr_gas_builtin, expr_system, expr_address_domain, expr_address, expr_value] =
        builder.try_get_refs()?;
    let gas_builtin = expr_gas_builtin.try_unpack_single()?.to_deref()?;
    let system = expr_system.try_unpack_single()?.to_buffer(6)?;
    let address_domain = expr_address_domain.try_unpack_single()?.to_deref()?;
    let storage_address = expr_address.try_unpack_single()?.to_deref()?;

    let value = expr_value.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(system);
    let gas_builtin = casm_builder.add_var(ResOperand::Deref(gas_builtin));
    let address_domain = casm_builder.add_var(ResOperand::Deref(address_domain));
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));
    let value = casm_builder.add_var(ResOperand::Deref(value));
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert address_domain = *(system++);
        assert storage_address = *(system++);
        assert value = *(system++);
        hint SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        tempvar revert_reason = *(system++);
        jump Failure if revert_reason != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[system]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[revert_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
    ))
}
