use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;

use self::interoperability::{build_call_contract, build_contract_address_const};
use self::storage::{
    build_storage_address_from_base_and_offset, build_storage_base_address_const,
    build_storage_base_address_from_felt,
};
use super::{misc, CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;

mod storage;
use storage::{build_storage_read, build_storage_write};

mod interoperability;

mod emit_event;
use emit_event::build_emit_event;

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
        StarkNetConcreteLibfunc::StorageBaseAddressConst(libfunc) => {
            build_storage_base_address_const(builder, libfunc)
        }
        StarkNetConcreteLibfunc::StorageBaseAddressFromFelt(_) => {
            build_storage_base_address_from_felt(builder)
        }
        StarkNetConcreteLibfunc::StorageAddressFromBase(_) => misc::build_identity(builder),
        StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => {
            build_storage_address_from_base_and_offset(builder)
        }
        StarkNetConcreteLibfunc::EmitEvent(_) => build_emit_event(builder),
    }
}
