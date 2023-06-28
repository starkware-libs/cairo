use cairo_lang_casm::builder::CasmBuilder;
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
    match libfunc {
        TestingConcreteLibfunc::PopLog(_) => {
            let [address] = builder.try_get_single_cells()?;
            add_input_variables! {casm_builder, deref address;};

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
            Ok(builder.build_from_casm_builder(
                casm_builder,
                [
                    ("Fallthrough", &[&[keys_start, keys_end], &[data_start, data_end]], None),
                    ("None", &[], Some(none_variant_id)),
                ],
                CostValidationInfo::default(),
            ))
        }
        TestingConcreteLibfunc::Cheatcode(c) => {
            let [input] = builder.try_get_refs()?;
            let [input_start, input_end] = input.try_unpack()?;

            add_input_variables! {casm_builder,
                deref input_start;
                deref input_end;
            }

            casm_build_extend! {casm_builder,
                tempvar output_start;
                tempvar output_end;
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

            Ok(builder.build_from_casm_builder(
                casm_builder,
                [("Fallthrough", &[&[output_start, output_end]], None)],
                CostValidationInfo::default(),
            ))
        }
    }
}
