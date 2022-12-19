use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};

#[cfg(test)]
#[path = "bitwise_test.rs"]
mod test;

/// Builds instructions for Sierra array operations.
pub fn build(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    build_bitwise(builder)
}

/// Handles instruction for appending an element to an array.
fn build_bitwise(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (bitwise, x, y) = match builder.refs {
        [
            ReferenceValue { expression: expr_bitwise, .. },
            ReferenceValue { expression: expr_x, .. },
            ReferenceValue { expression: expr_y, .. },
        ] => (
            expr_bitwise.try_unpack_single()?.to_buffer(4)?,
            expr_x.try_unpack_single()?.to_deref()?,
            expr_y.try_unpack_single()?.to_deref()?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    let original_bitwise = casm_builder.add_var(bitwise.clone());
    let bitwise = casm_builder.add_var(bitwise);
    casm_build_extend! {casm_builder,
        assert *(bitwise++) = x;
        assert *(bitwise++) = y;
        hint Bitwise { ptr: original_bitwise };
        let and = *(bitwise++);
        let or = *(bitwise++);
        let xor = *(bitwise++);
        ap += 0; // Needed because we currently do not support hints as last thing in CASM build.
    };

    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();

    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
    );

    let output_expressions = [vec![
        ReferenceExpression::from_cell(CellExpression::from_res_operand(
            fallthrough_state.get_adjusted(bitwise),
        )),
        ReferenceExpression::from_cell(CellExpression::from_res_operand(
            fallthrough_state.get_adjusted(and),
        )),
        ReferenceExpression::from_cell(CellExpression::from_res_operand(
            fallthrough_state.get_adjusted(or),
        )),
        ReferenceExpression::from_cell(CellExpression::from_res_operand(
            fallthrough_state.get_adjusted(xor),
        )),
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
