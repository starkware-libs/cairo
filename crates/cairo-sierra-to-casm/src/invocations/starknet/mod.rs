use cairo_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use cairo_sierra::extensions::starknet::StarkNetConcreteLibfunc;
use num_bigint::BigInt;

use self::interoperability::{build_call_contract, build_contract_address_const};
use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;
use crate::references::{CellExpression, ReferenceExpression};

mod storage;
use storage::{build_storage_read, build_storage_write};

mod interoperability;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &StarkNetConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarkNetConcreteLibfunc::CallContract(_) => build_call_contract(builder),
        StarkNetConcreteLibfunc::ContractAddressConst(libfunc) => {
            build_contract_address_const(builder, libfunc)
        }
        StarkNetConcreteLibfunc::StorageRead(_) => build_storage_read(builder),
        StarkNetConcreteLibfunc::StorageWrite(_) => build_storage_write(builder),
        StarkNetConcreteLibfunc::StorageAddressConst(libfunc) => {
            build_storage_address_const(builder, libfunc)
        }
    }
}

/// Handles the storage_address_const libfunc.
fn build_storage_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = (BigInt::from(1) << 251) - 256;
    if libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))].into_iter(),
    ))
}
