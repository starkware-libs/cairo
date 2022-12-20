use casm::{casm, builder::{CasmBuilder, CasmBuildResult}, operand::ResOperand, casm_build_extend};
use num_bigint::BigInt;

use crate::references::{
    try_unpack_deref, try_unpack_deref_with_offset, CellExpression, ReferenceExpression,
    ReferenceValue,
};
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

pub fn build_roll(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let selector = BigInt::from_bytes_le(num_bigint::Sign::Plus, "roll".as_bytes());

    let (address, caller_address) = match builder.refs {
        [
            ReferenceValue { expression: expr_address, .. },
            ReferenceValue { expression: expr_caller_address, .. },
        ] => (try_unpack_deref(expr_address)?, try_unpack_deref(expr_caller_address)?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            }); 
        }
    };

    // let instructions = casm! {
    //     [ap + -1] = [[fp + 1] + 0];
    //     %{ roll(address=1, caller_address=2) %}
    //     [ap + -1] = [[fp + 1] + 0];
    // }
    // .instructions;

    let mut casm_builder = CasmBuilder::default();
    let address = casm_builder.add_var(ResOperand::Deref(address));
    let caller_address = casm_builder.add_var(ResOperand::Deref(caller_address));
    casm_build_extend! {casm_builder,
        tempvar cheat_res;
        hint Roll {address: address, caller_address: caller_address} into {dst: cheat_res};
        jump Failure if cheat_res != 0;
    };

    let CasmBuildResult { instructions, awaiting_relocations: _, label_state: _, fallthrough_state: _ } =
        casm_builder.build();

    let output_expressions = [
        vec![].into_iter()
    ].into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
