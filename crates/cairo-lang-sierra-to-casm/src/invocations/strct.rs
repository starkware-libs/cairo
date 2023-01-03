use cairo_lang_sierra::extensions::strct::StructConcreteLibfunc;
use cairo_lang_sierra::extensions::ConcreteLibfunc;

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
        StructConcreteLibfunc::Deconstruct(libfunc) => {
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
    }
}
