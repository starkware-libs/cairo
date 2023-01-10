use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;

#[cfg(test)]
#[path = "bitwise_test.rs"]
mod test;

/// Builds instructions for Sierra array operations.
pub fn build(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    build_bitwise(builder)
}

/// Handles instruction for appending an element to an array.
fn build_bitwise(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [bitwise, x, y] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
        deref y;
        buffer(4) bitwise;
    };
    casm_build_extend! {casm_builder,
        assert x = *(bitwise++);
        assert y = *(bitwise++);
        let and = *(bitwise++);
        let xor = *(bitwise++);
        let or = *(bitwise++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[bitwise], &[and], &[xor], &[or]], None)],
    ))
}
