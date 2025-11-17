use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use num_bigint::ToBigInt;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;
use crate::invocations::misc::get_fp_based_pointer;
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibfunc::Into(_) => build_into_box(builder),
        BoxConcreteLibfunc::LocalInto(_) => build_local_into_box(builder),
        BoxConcreteLibfunc::Unbox(libfunc) => build_unbox(&libfunc.ty, builder),
        BoxConcreteLibfunc::ForwardSnapshot(_) => build_identity(builder),
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
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[addr]], None)],
        Default::default(),
    ))
}

/// Handles instruction for wrapping a local object of type T into a box.
fn build_local_into_box(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [operand] = builder.try_get_refs()?;

    let base_offset = match &operand.cells[..] {
        [] => 0,
        [CellExpression::Deref(cell), ..] if cell.register == Register::FP => cell.offset.into(),
        _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
    };
    let pre_instructions = get_fp_based_pointer(base_offset);

    let mut casm_builder = CasmBuilder::default();
    casm_builder.increase_ap_change(1);
    let addr =
        casm_builder.add_var(CellExpression::Deref(CellRef { register: Register::AP, offset: 0 }));

    Ok(builder.build_from_casm_builder_ex(
        casm_builder,
        [("Fallthrough", &[&[addr]], None)],
        Default::default(),
        pre_instructions,
    ))
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
            cells: (0..size).map(|idx| CellExpression::DoubleDeref(operand, idx)).collect(),
        }]
        .into_iter(),
    ))
}
