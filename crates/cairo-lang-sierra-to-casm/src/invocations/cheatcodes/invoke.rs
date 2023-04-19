use std::ops::Deref;

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

pub fn build_invoke(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let refs = builder.try_get_refs::<3>()?;

    let mut optional_contract_address = None;
    if let [maybe_contract_address] = refs[0].cells.deref() {
        optional_contract_address = Some(maybe_contract_address);
    }
    let contract_address = optional_contract_address.ok_or(InvocationError::InvalidGenericArg)?;

    let mut optional_function_name = None;
    if let [maybe_function_name] = refs[1].cells.deref() {
        optional_function_name = Some(maybe_function_name);
    }
    let function_name = optional_function_name.ok_or(InvocationError::InvalidGenericArg)?;

    let [calldata_start, calldata_end] = refs[2].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref contract_address;
        deref function_name;
        deref calldata_start;
        deref calldata_end;
    };

    casm_build_extend! {casm_builder,
        tempvar panic_data_start;
        tempvar panic_data_end;
        hint Invoke {
            contract_address: contract_address,
            function_name: function_name,
            calldata_start: calldata_start,
            calldata_end: calldata_end
        } into {
            panic_data_start: panic_data_start,
            panic_data_end: panic_data_end
        };
        tempvar failure = panic_data_end - panic_data_start;
        ap += 2;
        jump Failure if failure != 0;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[], None),
            ("Failure", &[&[panic_data_start, panic_data_end]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo { range_check_info: None, extra_costs: None },
    ))
}
