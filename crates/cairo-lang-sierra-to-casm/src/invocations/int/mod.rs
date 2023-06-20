use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::int::{IntConstConcreteLibfunc, IntTraits};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, CostValidationInfo};
use crate::references::ReferenceExpression;

pub mod signed;
pub mod signed128;
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

/// Handles a small uint wide multiplication.
pub fn build_small_wide_mul(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref_or_immediate b;
    };

    casm_build_extend! {casm_builder,
        let res = a * b;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[res]], None)],
        CostValidationInfo::default(),
    ))
}
