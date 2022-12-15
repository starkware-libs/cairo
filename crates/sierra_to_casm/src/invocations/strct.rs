use sierra::extensions::strct::StructConcreteLibFunc;
use sierra::extensions::ConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{ReferenceExpression, ReferenceValue};

/// Builds instructions for Sierra struct operations.
pub fn build(
    libfunc: &StructConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StructConcreteLibFunc::Construct(_) => {
            let cells = builder
                .refs
                .iter()
                .flat_map(|ref_value| &ref_value.expression.cells)
                .cloned()
                .collect();
            Ok(builder.build_only_reference_changes([ReferenceExpression { cells }].into_iter()))
        }
        StructConcreteLibFunc::Deconstruct(libfunc) => {
            let struct_type = &libfunc.param_signatures()[0].ty;
            let cells = match builder.refs {
                [ReferenceValue { expression: ReferenceExpression { cells }, .. }]
                    if cells.len() == builder.program_info.type_sizes[struct_type] as usize =>
                {
                    cells
                }
                [_] => return Err(InvocationError::InvalidReferenceExpressionForArgument),
                refs => {
                    return Err(InvocationError::WrongNumberOfArguments {
                        expected: 1,
                        actual: refs.len(),
                    });
                }
            };
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
