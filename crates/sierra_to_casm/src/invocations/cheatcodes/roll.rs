use casm::{casm};
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    try_unpack_deref,
    ReferenceValue,
};

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

    let instructions = casm! {
        %{ roll(address=1, caller_address=2) %}
    }
    .instructions;

    let output_expressions = [
        vec![].into_iter()
    ].into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
