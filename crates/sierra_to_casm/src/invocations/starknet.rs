use sierra::extensions::starknet::StarkNetConcreteLibFunc;

use super::storage::build_storage_read;
use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &StarkNetConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarkNetConcreteLibFunc::StorageRead(_) => build_storage_read(builder),
    }
}
