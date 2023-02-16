use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra_gas::core_libfunc_cost::SYSTEM_CALL_COST;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

/// Builds instructions for Starknet getter system call that return a result of size 1.
pub fn build_getter(
    builder: CompiledInvocationBuilder<'_>,
    selector: &str,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, selector.as_bytes());

    let [gas_builtin, system] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(6) system;
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
        tempvar failure_flag = *(system++);
        // The response in the success case is smaller than in the failure case.
        let response_0 = *(system++);
        let success_final_system = system;
        let revert_reason_end = *(system++);
        jump Failure if failure_flag != 0;

    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            (
                "Fallthrough",
                &[&[updated_gas_builtin], &[success_final_system], &[response_0]],
                None,
            ),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[response_0, revert_reason_end]],
                Some(failure_handle_statement_id),
            ),
        ],
        CostValidationInfo {
            range_check_info: None,
            extra_costs: Some([SYSTEM_CALL_COST, SYSTEM_CALL_COST]),
        },
    ))
}
