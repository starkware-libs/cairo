use cairo_sierra::extensions::boxing::BoxConcreteLibFunc;
use cairo_sierra::extensions::ConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression};

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibFunc::Into(_) => build_into_box(builder),
        BoxConcreteLibFunc::Unbox(_) => build_unbox(builder),
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

    let operand = builder.try_get_refs::<1>()?[0].try_unpack_single()?.to_deref()?;
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::IntoSingleCellRef(operand))].into_iter(),
    ))
}

/// Handles instruction for unboxing a box.
fn build_unbox(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let operand = builder.try_get_refs::<1>()?[0].try_unpack_single()?.to_deref()?;

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::DoubleDeref(operand, 0))].into_iter(),
    ))
}
