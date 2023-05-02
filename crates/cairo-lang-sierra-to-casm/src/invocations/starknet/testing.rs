use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::StarknetHint;
use cairo_lang_sierra::extensions::starknet::testing::TestingConcreteLibfunc;

use crate::invocations::{
    add_input_variables, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError,
};

/// Builds instructions for starknet test setup operations.
pub fn build(
    libfunc: &TestingConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();
    let mut declare_single_value = || -> Result<Var, InvocationError> {
        let [value] = builder.try_get_single_cells()?;
        add_input_variables! {casm_builder, deref value;};
        Ok(value)
    };
    match libfunc {
        TestingConcreteLibfunc::SetBlockNumber(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetBlockNumber {value: value}; };
        }
        TestingConcreteLibfunc::SetBlockTimestamp(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetBlockTimestamp {value: value}; };
        }
        TestingConcreteLibfunc::SetCallerAddress(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetCallerAddress {value: value}; };
        }
        TestingConcreteLibfunc::SetContractAddress(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetContractAddress {value: value}; };
        }
        TestingConcreteLibfunc::SetSequencerAddress(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetSequencerAddress {value: value}; };
        }
        TestingConcreteLibfunc::SetVersion(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetVersion {value: value}; };
        }
        TestingConcreteLibfunc::SetAccountContractAddress(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetAccountContractAddress {value: value}; };
        }
        TestingConcreteLibfunc::SetMaxFee(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetMaxFee {value: value}; };
        }
        TestingConcreteLibfunc::SetTransactionHash(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetTransactionHash {value: value}; };
        }
        TestingConcreteLibfunc::SetChainId(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetChainId {value: value}; };
        }
        TestingConcreteLibfunc::SetNonce(_) => {
            let value = declare_single_value()?;
            casm_build_extend! {casm_builder, hint StarknetHint::SetNonce {value: value}; };
        }
        TestingConcreteLibfunc::SetSignature(_) => {
            let [signature] = builder.try_get_refs()?;
            let [start, end] = signature.try_unpack()?;
            add_input_variables! {casm_builder, deref start; deref end;};
            casm_build_extend! {casm_builder,
                hint StarknetHint::SetSignature { start: start, end: end };
            };
        }
    }
    casm_build_extend! {casm_builder, ap += 0; };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None)],
        CostValidationInfo::default(),
    ))
}
