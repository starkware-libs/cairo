use std::vec;

use cairo_lang_casm::{casm};

use crate::references::ReferenceExpression;

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
        std::iter::empty::<std::vec::IntoIter<ReferenceExpression>>(),
    ))
}
