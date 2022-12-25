#[cfg(test)]
#[path = "pedersen_test.rs"]
mod test;

use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::ResOperand;
use sierra::extensions::pedersen::PedersenConcreteLibFunc;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression};

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
    let [expr_pedersen, expr_x, expr_y] = builder.try_get_refs()?;
    let pedersen = expr_pedersen.try_unpack_single()?.to_buffer(2)?;
    let x = expr_x.try_unpack_single()?.to_deref()?;
    let y = expr_y.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let pedersen = casm_builder.add_var(pedersen);
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    casm_build_extend! {casm_builder,
        assert x = *(pedersen++);
        assert y = *(pedersen++);
        let result = *(pedersen++);
    };
    let CasmBuildResult { instructions, branches: [(state, _)] } =
        casm_builder.build(["Fallthrough"]);
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [state.ap_change].map(sierra_ap_change::ApChange::Known)
    );
    Ok(builder.build(
        instructions,
        vec![],
        [vec![
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                state.get_adjusted(pedersen),
            )),
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                state.get_adjusted(result),
            )),
        ]
        .into_iter()]
        .into_iter(),
    ))
}
