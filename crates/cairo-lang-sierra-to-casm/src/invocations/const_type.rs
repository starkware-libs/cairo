use cairo_lang_casm::casm;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::extensions::const_type::{
    ConstAsBoxConcreteLibfunc, ConstAsImmediateConcreteLibfunc, ConstConcreteLibfunc,
};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra const type operations.
pub fn build(
    libfunc: &ConstConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        ConstConcreteLibfunc::AsBox(libfunc) => build_as_box(libfunc, builder),
        ConstConcreteLibfunc::AsImmediate(libfunc) => build_as_immediate(libfunc, builder),
    }
}

/// Builds instructions for `const_as_box` libfunc.
fn build_as_box(
    libfunc: &ConstAsBoxConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let ctx = casm! {
        // The relocation will point the `call` to the `ret;` instruction that precedes the
        // relevant const.
        call rel 0;
        // The relocation table will add const offset to the `1` below, making it point to the
        // constant value (the `1` is to skip the `ret` instruction).
        // TODO(Gil): Support relocatable CellExpression and return an unstored "[ap - 1] + 1".
        [ap] = [ap - 1] + 1, ap++;
    };
    let relocations = vec![
        RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::SegmentStart(libfunc.segment_id),
        },
        RelocationEntry {
            instruction_idx: 1,
            relocation: Relocation::ConstStart(libfunc.segment_id, libfunc.const_type.clone()),
        },
    ];
    Ok(builder.build(
        ctx.instructions,
        relocations,
        [vec![ReferenceExpression::from_cell(CellExpression::Deref(CellRef {
            register: Register::AP,
            offset: -1,
        }))]
        .into_iter()]
        .into_iter(),
    ))
}

/// Builds instructions for `const_as_immediate` libfunc.
fn build_as_immediate(
    libfunc: &ConstAsImmediateConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let cells = (builder.program_info.const_data_values)(&libfunc.const_type)
        .into_iter()
        .map(CellExpression::Immediate)
        .collect();
    Ok(builder.build_only_reference_changes([ReferenceExpression { cells }].into_iter()))
}
