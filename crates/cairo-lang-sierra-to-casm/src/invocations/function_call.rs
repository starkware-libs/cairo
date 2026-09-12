use std::collections::VecDeque;

use cairo_lang_casm::casm;
use cairo_lang_casm::operand::Register;
use cairo_lang_sierra::extensions::function_call::SignatureAndFunctionConcreteLibfunc;
use cairo_lang_sierra::extensions::ConcreteLibfunc;

use super::{
    check_references_on_stack, CompiledInvocation, CompiledInvocationBuilder, InvocationError,
};
use crate::references::build_deref_reference;
use crate::relocations::{Relocation, RelocationEntry};

/// Handles a function call.
pub fn build(
    libfunc: &SignatureAndFunctionConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    check_references_on_stack(builder.refs)?;

    let output_types = libfunc.output_types();
    let fallthrough_outputs = &output_types[0];

    let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

    let mut offset = -1_i64;
    for output_type in fallthrough_outputs.iter().rev() {
        let size = builder
            .program_info
            .type_sizes
            .get(output_type)
            .ok_or(InvocationError::UnknownVariableData)?;
        refs.push_front(
            build_deref_reference(Register::AP, offset, *size)
                .ok_or(InvocationError::IntegerOverflow)?,
        );
        offset -= i64::from(*size);
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
