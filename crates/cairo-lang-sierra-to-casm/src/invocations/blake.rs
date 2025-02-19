use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::ExternalHint;
use cairo_lang_sierra::extensions::blake::BlakeConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;

/// Builds instructions for Sierra bool operations.
pub fn build(
    libfunc: &BlakeConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let finalize = match libfunc {
        BlakeConcreteLibfunc::Blake2sCompress(_) => false,
        BlakeConcreteLibfunc::Blake2sFinalize(_) => true,
    };

    build_compress(builder, finalize)
}

/// Handles instructions for boolean AND.
fn build_compress(
    builder: CompiledInvocationBuilder<'_>,
    finalize: bool,
) -> Result<CompiledInvocation, InvocationError> {
    let [state, byte_count, message] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref state;
        deref byte_count;
        deref message;
    };
    casm_build_extend! {casm_builder,
        tempvar output;
        const state_size = 8;
        const finalize = finalize;
        hint AllocConstantSize { size: state_size } into {dst: output};
        hint ExternalHint::Blake2sCompress { state, byte_count, message, output, finalize};
        ap += 1;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[output]], None)],
        Default::default(),
    ))
}
