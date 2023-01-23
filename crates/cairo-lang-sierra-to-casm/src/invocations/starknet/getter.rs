use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};

/// Builds instructions for StarkNet getter system call that return a result of size 1.
pub fn build_getter(
    builder: CompiledInvocationBuilder<'_>,
    selector: &str,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, selector.as_bytes());

    let [gas_builtin, system] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(4) system;
        deref gas_builtin;
    };
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        hint SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        tempvar revert_reason = *(system++);
        let failure_system = system;
        jump Failure if revert_reason != 0;
        let info = *(system++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[system], &[info]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[failure_system], &[revert_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
    ))
}
