use std::collections::VecDeque;

use cairo_lang_casm::casm;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::extensions::function_call::FunctionCallConcreteLibfunc;
use cairo_lang_sierra::extensions::ConcreteLibfunc;

use super::{
    check_references_on_stack, CompiledInvocation, CompiledInvocationBuilder, InvocationError,
};
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Handles a function call.
pub fn build(
    libfunc: &FunctionCallConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    check_references_on_stack(builder.refs)?;

    let output_types = libfunc.output_types();
    let fallthrough_outputs = &output_types[0];

    let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

    let mut offset = -1;
    for output_type in fallthrough_outputs.iter().rev() {
        let size = builder
            .program_info
            .type_sizes
            .get(output_type)
            .ok_or(InvocationError::UnknownVariableData)?;
        refs.push_front(ReferenceExpression {
            cells: ((offset - size + 1)..(offset + 1))
                .map(|i| CellExpression::Deref(CellRef { register: Register::AP, offset: i }))
                .collect(),
        });
        offset -= size;
    }

    Ok(builder.build(
        casm! { call rel 0; }.instructions,
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(libfunc.function.entry_point),
        }],
        [refs.into_iter()].into_iter(),
    ))
}
