use std::collections::VecDeque;

use casm::ap_change::ApChange;
use casm::casm;
use casm::operand::{CellRef, Register};
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::lib_func::SierraApChange;
use sierra::extensions::ConcreteLibFunc;
use utils::casts::usize_as_i16;

use super::{
    check_references_on_stack, CompiledInvocation, CompiledInvocationBuilder, InvocationError,
};
use crate::references::{CellExpression, ReferenceExpression};
use crate::relocations::{Relocation, RelocationEntry};

/// Handles a function call.
pub fn build(
    libfunc: &FunctionCallConcreteLibFunc,
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
            cells: ((offset - usize_as_i16(*size) + 1)..(offset + 1))
                .map(|i| CellExpression::Deref(CellRef { register: Register::AP, offset: i }))
                .collect(),
        });
        offset -= usize_as_i16(*size);
    }

    let ap_change = match builder.program_info.metadata.function_ap_change.get(&libfunc.function.id)
    {
        // The call uses two stack slots.
        Some(SierraApChange::Known(change)) => ApChange::Known(change + 2),
        _ => ApChange::Unknown,
    };

    Ok(builder.build(
        casm! { call rel 0; }.instructions,
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(libfunc.function.entry_point),
        }],
        [ap_change].into_iter(),
        [refs.into_iter()].into_iter(),
    ))
}
