use std::vec;

use cairo_lang_casm::casm;
use cairo_lang_casm::hints::{ExternalHint, Hint};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

/// Builds instructions unsafe_panic;
pub fn build(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut ctx = casm! {
        // infinite loop.
        jmp rel 0;
    };

    ctx.instructions[0].hints.push(Hint::External(ExternalHint::UnsafePanic));

    Ok(builder.build(
        ctx.instructions,
        vec![],
        std::iter::empty::<std::vec::IntoIter<ReferenceExpression>>(),
    ))
}
