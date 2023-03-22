#[cfg(test)]
#[path = "output_test.rs"]
mod test;

use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::output::OutputConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;

/// Builds instructions for Sierra output operations.
pub fn build(
    libfunc: &OutputConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        OutputConcreteLibfunc::OutputFelt(_) => build_output(builder),
    }
}

/// Handles instruction for outputing a felt252.
fn build_output(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [output_builtin, x] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
        buffer(1) output_builtin;
    };
    casm_build_extend! {casm_builder,
        assert x = *(output_builtin++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[output_builtin]], None)],
        Default::default(),
    ))
}
