use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::StarknetHint;
use cairo_lang_sierra::extensions::starknet::testing::TestingConcreteLibfunc;
use cairo_lang_utils::bigint::BigIntAsHex;

use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CompiledInvocation,
    CompiledInvocationBuilder, CostValidationInfo, InvocationError,
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
        TestingConcreteLibfunc::PopLog(_) => {
            let address = declare_single_value()?;

            casm_build_extend! {casm_builder,
                tempvar variant;
                tempvar keys_start;
                tempvar keys_end;
                tempvar data_start;
                tempvar data_end;
                hint StarknetHint::PopLog {
                    value: address
                } into {
                    opt_variant: variant, keys_start: keys_start,
                    keys_end: keys_end, data_start: data_start,
                    data_end: data_end
                };
                ap += 5;
                jump None if variant != 0;
            };

            let none_variant_id = get_non_fallthrough_statement_id(&builder);
            return Ok(builder.build_from_casm_builder(
                casm_builder,
                [
                    ("Fallthrough", &[&[keys_start, keys_end], &[data_start, data_end]], None),
                    ("None", &[], Some(none_variant_id)),
                ],
                CostValidationInfo::default(),
            ));
        }
        TestingConcreteLibfunc::Cheatcode(c) => {
            let [input] = builder.try_get_refs()?;
            let [input_start, input_end] = input.try_unpack()?;

            let output_start = casm_builder.alloc_var(false);
            let output_end = casm_builder.alloc_var(false);

            add_input_variables! {casm_builder,
                deref input_start;
                deref input_end;
            }
            casm_builder.add_hint(
                |[input_start, input_end], [output_start, output_end]| StarknetHint::Cheatcode {
                    selector: BigIntAsHex { value: c.selector.clone() },
                    input_start,
                    input_end,
                    output_start,
                    output_end,
                },
                [input_start, input_end],
                [output_start, output_end],
            );
            casm_build_extend! {casm_builder, ap += 2; }
            // casm_build_extend! {casm_builder,
            //     tempvar output_start;
            //     tempvar output_end;
            //     hint StarknetHint::Cheatcode {
            //         selector: selector,
            //         input_start: input_start,
            //         input_end: input_end
            //     } into {
            //         output_start: output_start,
            //         output_end: output_end
            //     };
            //     ap += 2;
            // };

            return Ok(builder.build_from_casm_builder(
                casm_builder,
                [("Fallthrough", &[&[output_start, output_end]], None)],
                CostValidationInfo::default(),
            ));
        }
    }
    casm_build_extend! {casm_builder, ap += 0; };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None)],
        CostValidationInfo::default(),
    ))
}
