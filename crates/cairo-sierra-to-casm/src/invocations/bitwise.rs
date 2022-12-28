use cairo_casm::builder::CasmBuilder;
use cairo_casm::casm_build_extend;
use cairo_casm::operand::ResOperand;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

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
    let [expr_bitwise, expr_x, expr_y] = builder.try_get_refs()?;
    let bitwise = expr_bitwise.try_unpack_single()?.to_buffer(4)?;
    let x = expr_x.try_unpack_single()?.to_deref()?;
    let y = expr_y.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    let bitwise = casm_builder.add_var(bitwise);
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
