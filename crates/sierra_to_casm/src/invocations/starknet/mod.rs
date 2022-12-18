use num_bigint::BigInt;
use sierra::extensions::starknet::storage::StorageAddressConstConcreteLibFunc;
use sierra::extensions::starknet::StarkNetConcreteLibFunc;

use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;
use crate::references::{CellExpression, ReferenceExpression};

mod storage;
use storage::{build_storage_read, build_storage_write};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &StarkNetConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarkNetConcreteLibFunc::StorageRead(_) => build_storage_read(builder),
        StarkNetConcreteLibFunc::StorageWrite(_) => build_storage_write(builder),
        StarkNetConcreteLibFunc::StorageAddressConst(libfunc) => {
            build_storage_address_const(builder, libfunc)
        }
    }
}

/// Handles the storage_address_const libfunc.
fn build_storage_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &StorageAddressConstConcreteLibFunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = (BigInt::from(1) << 251) - 256;
    if libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))].into_iter(),
    ))
}
