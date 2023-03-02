use super::{build_syscalls, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Starknet getter system call that return a result of size 1.
pub fn build_getter(
    builder: CompiledInvocationBuilder<'_>,
    selector: &str,
) -> Result<CompiledInvocation, InvocationError> {
    build_syscalls(builder, selector, [], [1])
}
