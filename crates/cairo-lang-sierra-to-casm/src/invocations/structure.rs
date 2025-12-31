use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::operand::DerefOrImmediate;
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::extensions::structure::{
    ConcreteStructBoxedDeconstructLibfunc, StructConcreteLibfunc,
};
use cairo_lang_utils::casts::IntoOrPanic;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra struct operations.
pub fn build(
    libfunc: &StructConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StructConcreteLibfunc::Construct(_) => {
            let cells = builder
                .refs
                .iter()
                .flat_map(|ref_value| &ref_value.expression.cells)
                .cloned()
                .collect();
            Ok(builder.build_only_reference_changes([ReferenceExpression { cells }].into_iter()))
        }
        StructConcreteLibfunc::Deconstruct(libfunc)
        | StructConcreteLibfunc::SnapshotDeconstruct(libfunc) => {
            let struct_type = &libfunc.param_signatures()[0].ty;
            let cells = &builder.try_get_refs::<1>()?[0].cells;
            if cells.len() != builder.program_info.type_sizes[struct_type] as usize {
                return Err(InvocationError::InvalidReferenceExpressionForArgument);
            }
            let output_types = libfunc.output_types();
            assert_eq!(output_types.len(), 1, "Wrong number of branches configured.");
            let mut offset = 0_usize;
            let mut outputs = vec![];
            for ty in &output_types[0] {
                let size = builder.program_info.type_sizes[ty] as usize;
                outputs
                    .push(ReferenceExpression { cells: cells[offset..(offset + size)].to_vec() });
                offset += size;
            }
            Ok(builder.build_only_reference_changes(outputs.into_iter()))
        }
        StructConcreteLibfunc::BoxedDeconstruct(libfunc) => {
            build_struct_boxed_deconstruct(libfunc, builder)
        }
    }
}

/// Generates CASM instructions for deconstructing a boxed struct into individual boxed members.
///
/// This function takes a boxed struct (stored in a single memory cell containing the address)
/// and creates reference expressions for each member by calculating their memory offsets.
/// The actual CASM for computing these addresses will be generated when store_temp is called.
fn build_struct_boxed_deconstruct(
    libfunc: &ConcreteStructBoxedDeconstructLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [input_ptr] = builder.try_get_single_cells()?;
    let Some((boxed_struct_ptr, orig_offset)) = input_ptr.to_deref_with_offset() else {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    };
    let mut outputs = vec![];
    // TODO: Why i16 and not i32 as returned by `to_deref_with_offset`?
    // TODO: Why panic instead of using `Result::Err`?
    let mut current_offset = orig_offset.into_or_panic::<i16>();
    for member_ty in &libfunc.members {
        // TODO: Refactor to a function that returns `CellExpression::Deref` if the offset is zero.
        //   This may help if there are zero-sized members.
        outputs.push(ReferenceExpression::from_cell(CellExpression::BinOp {
            op: CellOperator::Add,
            a: boxed_struct_ptr,
            b: DerefOrImmediate::Immediate(current_offset.into()),
        }));
        let member_size = *builder
            .program_info
            .type_sizes
            .get(member_ty)
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
        // TODO: Should use `checked_add`?
        current_offset += member_size;
    }

    Ok(builder.build_only_reference_changes(outputs.into_iter()))
}
