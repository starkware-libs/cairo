use cairo_lang_casm::ap_change::ApplyApChange;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::hints::CoreHint;
use cairo_lang_casm::instructions::{Blake2sCompressInstruction, Instruction, InstructionBody};
use cairo_lang_casm::operand::{CellRef, Register, ResOperand};
use cairo_lang_sierra::extensions::blake::BlakeConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra bool operations.
pub fn build(
    libfunc: &BlakeConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BlakeConcreteLibfunc::Blake2sCompress(_) => build_compress(builder, false),
        BlakeConcreteLibfunc::Blake2sFinalize(_) => build_compress(builder, true),
    }
}

/// Handles instructions for blake2s compression.
fn build_compress(
    builder: CompiledInvocationBuilder<'_>,
    finalize: bool,
) -> Result<CompiledInvocation, InvocationError> {
    let [Some(state), Some(byte_count), Some(message)] =
        builder.try_get_single_cells()?.map(CellExpression::to_deref)
    else {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    };
    let output = CellRef { register: Register::AP, offset: 0 };
    let hints = vec![
        CoreHint::AllocConstantSize { size: ResOperand::Immediate(8.into()), dst: output }.into(),
    ];
    let compress = Blake2sCompressInstruction { state, byte_count, message, finalize };
    let instruction =
        Instruction { hints, body: InstructionBody::Blake2sCompress(compress), inc_ap: true };
    let fixed_output = ReferenceExpression::from_cell(CellExpression::Deref(
        output.unchecked_apply_known_ap_change(1),
    ));
    Ok(builder.build(vec![instruction], vec![], [[fixed_output].into_iter()].into_iter()))
}
