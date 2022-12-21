use sierra::extensions::lib_func::SignatureAndTypeConcreteLibFunc;
use sierra::extensions::nullable::NullableConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};

/// Builds Casm instructions for Nullable operations.
pub fn build(
    libfunc: &NullableConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        NullableConcreteLibFunc::Null(_) => build_nullable_null(builder),
        NullableConcreteLibFunc::IntoNullable(libfunc) => {
            build_nullable_into_nullable(builder, libfunc)
        }
    }
}

/// Builds Casm instructions for the `null()` libfunc.
fn build_nullable_null(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    if !builder.refs.is_empty() {
        return Err(InvocationError::WrongNumberOfArguments {
            expected: 0,
            actual: builder.refs.len(),
        });
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression { cells: vec![CellExpression::Immediate(0.into())] }].into_iter(),
    ))
}

/// Builds Casm instructions for the `into_nullable()` libfunc.
fn build_nullable_into_nullable(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndTypeConcreteLibFunc,
) -> Result<CompiledInvocation, InvocationError> {
    // Check that the size of the inner type is nonzero and the argument is a simple deref
    // expression.
    //
    // This guarantees that values are written to the memory address of the Box<T>.
    // It follows that this address cannot be zero, since the Cairo-AIR guarantees that all
    // memory accesses have address >= 1.
    //
    // Therefore, we do not need to explicitly check that the address chosen by `into_box()`
    // is nonzero.

    assert!(
        *builder.program_info.type_sizes.get(&libfunc.ty).expect("Unknown size for type") > 0,
        "Nullable<> cannot be used for types of size 0."
    );

    let value = match builder.refs {
        [ReferenceValue { expression: expr_value, .. }] => {
            expr_value.try_unpack_single()?.to_deref()?
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression { cells: vec![CellExpression::Deref(value)] }].into_iter(),
    ))
}
