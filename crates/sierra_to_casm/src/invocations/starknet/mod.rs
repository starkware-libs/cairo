use sierra::extensions::starknet::StarkNetConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod storage;
use storage::build_storage_read;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &StarkNetConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarkNetConcreteLibFunc::StorageRead(_) => build_storage_read(builder),
        StarkNetConcreteLibFunc::StorageAddressConst(_) => todo!(),
    }
}
