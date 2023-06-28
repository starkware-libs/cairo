use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::StarknetHint;
use cairo_lang_sierra::extensions::starknet::testing::TestingConcreteLibfunc;
use cairo_lang_utils::bigint::BigIntAsHex;

use crate::invocations::{
    add_input_variables, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError,
};

/// Builds instructions for starknet test setup operations.
pub fn build(
    libfunc: &TestingConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        TestingConcreteLibfunc::Cheatcode(c) => {
            let mut casm_builder = CasmBuilder::default();
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
