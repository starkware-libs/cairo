#[cfg(test)]
#[path = "pedersen_test.rs"]
mod test;

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::pedersen::PedersenConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra pedersen operations.
pub fn build(
    libfunc: &PedersenConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        PedersenConcreteLibfunc::Hash(_) => build_pedersen_hash(builder),
    }
}

/// Handles instruction for computing a pedersen hash on two felts.
fn build_pedersen_hash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_pedersen, expr_x, expr_y] = builder.try_get_refs()?;
    let pedersen = expr_pedersen.try_unpack_single()?;
    let x = expr_x.try_unpack_single()?;
    let y = expr_y.try_unpack_single()?;

    let mut casm_builder = CasmBuilder::default();
    super::add_input_variables! {casm_builder,
        deref x;
        deref y;
        buffer(2) pedersen;
    };
    casm_build_extend! {casm_builder,
        assert x = *(pedersen++);
        assert y = *(pedersen++);
        let result = *(pedersen++);
    };
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[pedersen], &[result]], None)]))
}
