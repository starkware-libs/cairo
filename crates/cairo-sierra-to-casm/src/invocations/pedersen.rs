#[cfg(test)]
#[path = "pedersen_test.rs"]
mod test;

use casm::builder::CasmBuilder;
use casm::casm_build_extend;
use casm::operand::ResOperand;
use sierra::extensions::pedersen::PedersenConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &PedersenConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        PedersenConcreteLibFunc::Hash(_) => build_pedersen_hash(builder),
    }
}

/// Handles instruction for appending an element to an array.
fn build_pedersen_hash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_pedersen, expr_x, expr_y] = builder.try_get_refs()?;
    let pedersen = expr_pedersen.try_unpack_single()?.to_buffer(2)?;
    let x = expr_x.try_unpack_single()?.to_deref()?;
    let y = expr_y.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let pedersen = casm_builder.add_var(pedersen);
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    casm_build_extend! {casm_builder,
        assert x = *(pedersen++);
        assert y = *(pedersen++);
        let result = *(pedersen++);
    };
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[pedersen], &[result]], None)]))
}
