use std::vec;

use cairo_lang_casm::{casm};

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions unsafe_panic;
pub fn build(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {

     let ctx = casm! {
        // infinite loop.
        jmp rel 0;
    };


    Ok(builder.build(
        ctx.instructions,
        vec![],
        vec![
        vec![]
        .into_iter()]
        .into_iter()
    ))
}
