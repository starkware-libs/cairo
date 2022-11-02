use sierra::extensions::strct::StructConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra struct operations.
pub fn build(
    libfunc: &StructConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StructConcreteLibFunc::Construct(_) | StructConcreteLibFunc::Deconstruct(_) => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
    }
}
