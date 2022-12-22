use casm::{builder::{CasmBuilder, CasmBuildResult}, operand::ResOperand, casm_build_extend};

use crate::references::{
    ReferenceValue, ReferenceExpression, CellExpression,
};
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use sierra_ap_change::core_libfunc_ap_change;

pub fn build_roll(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (address, caller_address) = match builder.refs {
        [
            ReferenceValue { expression: expr_address, .. },
            ReferenceValue { expression: expr_caller_address, .. },
        ] => (expr_address.try_unpack_single()?.to_deref()?, expr_caller_address.try_unpack_single()?.to_deref()?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            }); 
        }
    };

    let mut casm_builder = CasmBuilder::default();
    let address = casm_builder.add_var(ResOperand::Deref(address));
    let caller_address = casm_builder.add_var(ResOperand::Deref(caller_address));
    casm_build_extend! {casm_builder,
        tempvar error_reason;
        hint Roll {address: address, caller_address: caller_address} into {dst: error_reason};
        jump Failure if error_reason != 0;
    };

    let CasmBuildResult { 
        instructions,
        awaiting_relocations: _,
        label_state,
        fallthrough_state 
    } = casm_builder.build();

    // assert_eq!(
    //     core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
    //     [sierra_ap_change::ApChange::Known(fallthrough_state.ap_change)]
    // );

    let output_expressions = [
        vec![].into_iter(),
        vec![
            ReferenceExpression::from_cell(CellExpression::Deref(
                label_state["Failure"].get_adjusted_as_cell_ref(error_reason),
            )),
        ].into_iter()
    ].into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
