use cairo_lang_casm::casm;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::extensions::function_call::SignatureAndFunctionConcreteLibfunc;
use itertools::Itertools;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, check_references_on_stack,
};
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Handles a function call.
pub fn build(
    libfunc: &SignatureAndFunctionConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
    check_args_on_stack: bool,
) -> Result<CompiledInvocation, InvocationError> {
    if check_args_on_stack {
        check_references_on_stack(builder.refs)?;
    }

    let Ok(output_types) = libfunc.output_types().exactly_one() else {
        unreachable!("FunctionCall has only one branch");
    };

    let mut refs = Vec::with_capacity(output_types.len());

    let mut offset = -1;
    for output_type in output_types.rev() {
        let size = builder
            .program_info
            .type_sizes
            .get(output_type)
            .ok_or(InvocationError::UnknownVariableData)?;
        refs.push(ReferenceExpression {
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
        [refs.into_iter().rev()].into_iter(),
    ))
}
