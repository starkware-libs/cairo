use std::ops::Deref;

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

pub fn build_deploy(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let refs = builder.try_get_refs::<3>()?;

    let mut optional_prepared_contract_address = None;
    if let [maybe_contract_address] = refs[0].cells.deref() {
        optional_prepared_contract_address = Some(maybe_contract_address);
    }
    let prepared_contract_address =
        optional_prepared_contract_address.ok_or(InvocationError::InvalidGenericArg)?;

    let mut optional_prepared_class_hash = None;
    if let [maybe_prepared_class_hash] = refs[1].cells.deref() {
        optional_prepared_class_hash = Some(maybe_prepared_class_hash);
    }
    let prepared_class_hash =
        optional_prepared_class_hash.ok_or(InvocationError::InvalidGenericArg)?;

    let [prepared_constructor_calldata_start, prepared_constructor_calldata_end] =
        refs[2].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref prepared_contract_address;
        deref prepared_class_hash;
        deref prepared_constructor_calldata_start;
        deref prepared_constructor_calldata_end;
    };

    casm_build_extend! {
        casm_builder,
        tempvar err_code;
        tempvar deployed_contract_address;
        hint Deploy {
            prepared_contract_address: prepared_contract_address,
            prepared_class_hash: prepared_class_hash,
            prepared_constructor_calldata_start: prepared_constructor_calldata_start,
            prepared_constructor_calldata_end: prepared_constructor_calldata_end
        } into {
            deployed_contract_address: deployed_contract_address,
            err_code: err_code
        };
        ap += 1;
        jump Failure if err_code != 0;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[deployed_contract_address]], None),
            ("Failure", &[&[err_code]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo { range_check_info: None, extra_costs: None },
    ))
}
