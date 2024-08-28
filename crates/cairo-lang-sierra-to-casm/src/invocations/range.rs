use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::range::IntRangeConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};

/// Builds instructions for `Range` operations.
pub fn build(
    libfunc: &IntRangeConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        IntRangeConcreteLibfunc::PopFront(_) => build_pop_front(builder),
    }
}

/// Libfunc for reducing `[a, b)` to `[a + 1, b)`.
fn build_pop_front(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [start, end] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref start;
        deref end;
    };
    casm_build_extend! {casm_builder,
        const one = 1;
        let new_start = start + one;
        tempvar is_non_empty = end - start;
        jump NonEmpty if is_non_empty != 0;
    };
    let non_empty_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[], None),
            ("NonEmpty", &[&[new_start, end], &[start]], Some(non_empty_handle)),
        ],
        Default::default(),
    ))
}
