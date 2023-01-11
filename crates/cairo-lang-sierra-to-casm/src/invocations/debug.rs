use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::debug::DebugConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;

/// Builds Casm instructions for Nullable operations.
pub fn build(
    libfunc: &DebugConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        DebugConcreteLibfunc::Print(_) => build_print(builder),
    }
}

/// Builds Casm instructions for the `print()` libfunc.
fn build_print(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [arr_start, arr_end] = builder.try_get_refs::<1>()?[0].try_unpack()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) arr_start;
        buffer(0) arr_end;
    };
    casm_build_extend! {casm_builder,
        hint DebugPrint {start: arr_start, end: arr_end} into {};
        // Since we can't have hints not carried on actual instructions.
        ap += 0;
    };
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[], None)]))
}
