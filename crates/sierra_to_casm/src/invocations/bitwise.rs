use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression};

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
    let [expr_bitwise, expr_x, expr_y] = builder.try_get_refs()?;
    let bitwise = expr_bitwise.try_unpack_single()?.to_buffer(4)?;
    let x = expr_x.try_unpack_single()?.to_deref()?;
    let y = expr_y.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    let bitwise = casm_builder.add_var(bitwise);
    casm_build_extend! {casm_builder,
        assert x = *(bitwise++);
        assert y = *(bitwise++);
        let and = *(bitwise++);
        let xor = *(bitwise++);
        let or = *(bitwise++);
    };

    let CasmBuildResult { instructions, branches: [(state, _)] } =
        casm_builder.build(["Fallthrough"]);

    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [state.ap_change].map(sierra_ap_change::ApChange::Known)
    );

    let output_expressions = [vec![
        ReferenceExpression::from_cell(CellExpression::from_res_operand(
            state.get_adjusted(bitwise),
        )),
        ReferenceExpression::from_cell(CellExpression::from_res_operand(state.get_adjusted(and))),
        ReferenceExpression::from_cell(CellExpression::from_res_operand(state.get_adjusted(xor))),
        ReferenceExpression::from_cell(CellExpression::from_res_operand(state.get_adjusted(or))),
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
