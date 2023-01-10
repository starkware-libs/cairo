use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::ConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibfunc::Into(_) => build_into_box(builder),
        BoxConcreteLibfunc::Unbox(_) => build_unbox(builder),
    }
}

/// Handles instruction for creating a box.
fn build_into_box(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    if builder.program_info.type_sizes[&builder.libfunc.param_signatures()[0].ty] != 1 {
        return Err(InvocationError::NotImplementedStr {
            invocation: builder.invocation.clone(),
            message: "Box<T> is only supported for types of size 1.".into(),
        });
    }

    let operand = builder.try_get_refs::<1>()?[0].try_unpack_single()?;

    let mut casm_builder = CasmBuilder::default();
    super::add_input_variables! {casm_builder, deref operand; };
    casm_build_extend!(casm_builder,
        tempvar addr;
        hint AllocSegment {} into {dst: addr};
        assert operand = addr[0];
    );
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[addr]], None)]))
}

/// Handles instruction for unboxing a box.
fn build_unbox(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let operand = builder.try_get_refs::<1>()?[0]
        .try_unpack_single()?
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::DoubleDeref(operand, 0))].into_iter(),
    ))
}
