use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

pub fn build_start_prank(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [caller_address, target_contract_address] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref caller_address;
        deref target_contract_address;
    };

    casm_build_extend! {casm_builder,
        tempvar err_code;
        hint StartPrank {caller_address: caller_address, target_contract_address: target_contract_address} into {err_code: err_code};
        jump Failure if err_code != 0;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[], None),
            ("Failure", &[&[err_code]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo { range_check_info: None, extra_costs: None },
    ))
}
