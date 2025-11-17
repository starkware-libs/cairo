use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra_gas::objects::ConstCost;
use num_bigint::ToBigInt;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;
use crate::references::ReferenceExpression;
use crate::relocations::InstructionsWithRelocations;

#[cfg(test)]
#[path = "boxing_test.rs"]
mod test;

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibfunc::Into(_) => build_into_box(builder),
        BoxConcreteLibfunc::IntoReprPtr(_) => build_into_repr_ptr(builder),
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

/// Handles instruction for creating a repr-ptr box.
fn build_into_repr_ptr(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [operand] = builder.try_get_refs()?;

    if operand.cells.is_empty() {
        return build_into_box(builder);
    }

    let CellExpression::Deref(first_cell) = &operand.cells[0] else {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    };
    let base_offset = i32::from(first_cell.offset);

    // call rel 2 pushes PC/FP and jumps to ret.
    // ret returns to PC+1 (the jmp), which jumps over ret and retrieves the FP needed for the ptr.
    // TODO(giladchase): once helper segment is merged, remove the jmp and call that segment
    // instead of rel 2.
    let instructions = casm! {
        call rel 2;
        jmp rel 2;
        ret;
        [ap + 0] = [ap + -2] + base_offset, ap++;
    };

    let pre_instructions = InstructionsWithRelocations {
        instructions: instructions.instructions,
        relocations: vec![],
        cost: ConstCost { steps: 4, ..Default::default() },
    };

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
