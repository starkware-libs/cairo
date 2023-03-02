use super::{build_syscalls, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Starknet emit event system call.
pub fn build_emit_event(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    build_syscalls(builder, "EmitEvent", [2, 2], [])
}
