use cairo_lang_casm::builder::CasmBuilder;
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
    let [value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref value;
    };
    match libfunc {
        TestingConcreteLibfunc::SetBlockNumber(_) => {
            casm_build_extend! {casm_builder, hint StarknetHint::SetBlockNumber {value: value}; };
        }
        TestingConcreteLibfunc::SetBlockTimestamp(_) => {
            casm_build_extend! {casm_builder, hint StarknetHint::SetBlockTimestamp {value: value}; };
        }
        TestingConcreteLibfunc::SetCallerAddress(_) => {
            casm_build_extend! {casm_builder, hint StarknetHint::SetCallerAddress {value: value}; };
        }
        TestingConcreteLibfunc::SetContractAddress(_) => {
            casm_build_extend! {casm_builder, hint StarknetHint::SetContractAddress {value: value}; };
        }
        TestingConcreteLibfunc::SetSequencerAddress(_) => {
            casm_build_extend! {casm_builder, hint StarknetHint::SetSequencerAddress {value: value}; };
        }
    }
    casm_build_extend! {casm_builder, ap += 0; };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None)],
        CostValidationInfo::default(),
    ))
}
