use cairo_lang_casm::{builder::{CasmBuilder}, operand::ResOperand, casm_build_extend};

use crate::{references::{
    ReferenceValue,
}, invocations::get_non_fallthrough_statement_id};
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

pub fn build_roll(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
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


    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[], None),
            (
                "Failure",
                &[&[error_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
    ))

}
