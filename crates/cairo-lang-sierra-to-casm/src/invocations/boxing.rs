use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use num_bigint::ToBigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibfunc::Into(_) => build_into_box(builder),
        BoxConcreteLibfunc::Unbox(libfunc) => build_unbox(&libfunc.ty, builder),
    }
}

/// Handles instruction for creating a box.
fn build_into_box(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [operand] = builder.try_get_refs()?;
    let mut casm_builder = CasmBuilder::default();
    let addr = if operand.cells.is_empty() {
        // In cases of a zero-sized variable, we just simulate a non-zero address.
        casm_build_extend!(casm_builder,
            const one = 1;
            tempvar addr = one;
        );
        addr
    } else {
        casm_build_extend!(casm_builder,
            const operand_size = operand.cells.len().to_bigint().unwrap();
            tempvar addr;
            hint AllocConstantSize { size: operand_size } into { dst: addr };
        );
        for (index, cell) in operand.cells.iter().enumerate() {
            add_input_variables!(casm_builder, deref cell;);
            casm_build_extend!(casm_builder, assert cell = addr[index as i16];);
        }
        addr
    };
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[addr]], None)]))
}

/// Handles instruction for unboxing a box.
fn build_unbox(
    ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let size = builder.program_info.type_sizes[ty];
    let operand = builder.try_get_single_cells::<1>()?[0]
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression {
            cells: (0..size)
                .into_iter()
                .map(|idx| CellExpression::DoubleDeref(operand, idx))
                .collect(),
        }]
        .into_iter(),
    ))
}
