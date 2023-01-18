use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};

/// Builds instructions for StarkNet emit event system call.
pub fn build_emit_event(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "EmitEvent".as_bytes());

    let [expr_gas_builtin, expr_system, expr_keys, expr_data] = builder.try_get_refs()?;
    let gas_builtin = expr_gas_builtin.try_unpack_single()?;
    let system = expr_system.try_unpack_single()?;
    let [keys_start, keys_end] = expr_keys.try_unpack()?;
    let [data_start, data_end] = expr_data.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(7) system;
        deref gas_builtin;
        deref keys_start;
        deref keys_end;
        deref data_start;
        deref data_end;
    };
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert keys_start = *(system++);
        assert keys_end = *(system++);
        assert data_start = *(system++);
        assert data_end = *(system++);
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
