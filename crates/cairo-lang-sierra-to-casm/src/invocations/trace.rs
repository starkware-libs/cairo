use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::ExternalHint;
use cairo_lang_sierra::extensions::GenericLibfunc;
use cairo_lang_sierra::extensions::trace::TraceLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra trace operations.
pub fn build(
    libfunc: &<TraceLibfunc as GenericLibfunc>::Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();
    casm_build_extend! {casm_builder,
        const flag = libfunc.c.clone();
        hint ExternalHint::AddTrace { flag };
        ap += 0;
    }
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None)],
        Default::default(),
    ))
}
