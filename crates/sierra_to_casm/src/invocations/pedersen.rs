#[cfg(test)]
#[path = "pedersen_test.rs"]
mod test;

use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use sierra::extensions::pedersen::PedersenConcreteLibFunc;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &PedersenConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        PedersenConcreteLibFunc::Hash(_) => build_pedersen_hash(builder),
    }
}

/// Handles instruction for appending an element to an array.
fn build_pedersen_hash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (original_pedersen, x, y) = match builder.refs {
        [
            ReferenceValue { expression: expr_pedersen, .. },
            ReferenceValue { expression: expr_x, .. },
            ReferenceValue { expression: expr_y, .. },
        ] => (
            expr_pedersen.try_unpack_single()?.to_buffer(2)?,
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
    let pedersen = casm_builder.add_var(original_pedersen.clone());
    let _original_pedersen = casm_builder.add_var(original_pedersen);
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    casm_build_extend! {casm_builder,
        assert *(pedersen++) = x;
        assert *(pedersen++) = y;
        // TODO(orizi): Add pederesen hash hint: `hint Pedersen { ptr: original_pedersen };`.
        let result = *(pedersen++);
    };
    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
    );
    Ok(builder.build(
        instructions,
        vec![],
        [vec![
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                fallthrough_state.get_adjusted(pedersen),
            )),
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                fallthrough_state.get_adjusted(result),
            )),
        ]
        .into_iter()]
        .into_iter(),
    ))
}
