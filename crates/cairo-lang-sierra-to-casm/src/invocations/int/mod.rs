use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::int::{IntConstConcreteLibfunc, IntTraits};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

pub mod unsigned;
pub mod unsigned128;
pub mod unsigned256;
pub mod unsigned512;

/// Builds invocations for uint const values.
fn build_const<TIntTraits: IntTraits>(
    libfunc: &IntConstConcreteLibfunc<TIntTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.into()))].into_iter(),
    ))
}
