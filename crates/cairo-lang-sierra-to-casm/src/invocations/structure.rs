use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::extensions::structure::{
    ConcreteStructBoxedDeconstructLibfunc, StructConcreteLibfunc,
};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra_type_size::TypeSizeMap;

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
    let outputs =
        boxed_members_cell_exprs(builder.program_info.type_sizes, &libfunc.members, input_ptr)
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
    Ok(builder
        .build_only_reference_changes(outputs.into_iter().map(ReferenceExpression::from_cell)))
}

/// Returns a vector of cell expressions representing the memory addresses of the unboxed members of
/// a boxed struct.
///
/// Note: All failures returning `None` in this function should never actually occur assuming the
/// original boxed type containing recursively all other types (through enums or structs), had an
/// `orig_offset` offset of 0, as all internal offsets are bounded by the size of the struct itself,
/// which is bounded by `i16::MAX`.
fn boxed_members_cell_exprs(
    type_sizes: &TypeSizeMap,
    member_tys: &[ConcreteTypeId],
    input_ptr: &CellExpression,
) -> Option<Vec<CellExpression>> {
    let (boxed_struct_ptr, orig_offset) = input_ptr.to_deref_with_offset()?;
    let mut outputs = vec![];
    let mut current_offset: i16 = orig_offset.try_into().ok()?;
    for member_ty in member_tys {
        outputs.push(CellExpression::add_with_const(boxed_struct_ptr, current_offset));
        let member_size = *type_sizes.get(member_ty)?;
        current_offset = current_offset.checked_add(member_size)?;
    }
    Some(outputs)
}
