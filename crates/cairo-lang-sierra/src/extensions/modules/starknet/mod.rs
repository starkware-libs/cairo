use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{
    StorageBaseAddressConstLibfunc, StorageBaseAddressType, StorageReadLibfunc, StorageWriteLibfunc,
};

pub mod syscalls;
use syscalls::SystemType;

pub mod getter;

pub mod emit_event;
use emit_event::EmitEventLibfunc;
pub mod testing;

pub mod interoperability;
use interoperability::{CallContractLibfunc, ContractAddressConstLibfunc, ContractAddressType};

use self::getter::{GetExecutionInfoTrait, GetterLibfunc};
use self::interoperability::{ContractAddressToFeltLibfunc, ContractAddressTryFromFeltLibfunc};
use self::storage::{
    StorageAddressFromBaseAndOffsetLibfunc, StorageAddressFromBaseLibfunc, StorageAddressType,
    StorageBaseAddressFromFeltLibfunc,
};
use self::testing::TestingLibfunc;

define_type_hierarchy! {
    pub enum StarkNetType {
        ContractAddress(ContractAddressType),
        StorageBaseAddress(StorageBaseAddressType),
        StorageAddress(StorageAddressType),
        System(SystemType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibfunc {
         CallContract(CallContractLibfunc),
         ContractAddressConst(ContractAddressConstLibfunc),
         ContractAddressTryFromFelt(ContractAddressTryFromFeltLibfunc),
         ContractAddressToFelt(ContractAddressToFeltLibfunc),
         StorageRead(StorageReadLibfunc),
         StorageWrite(StorageWriteLibfunc),
         StorageBaseAddressConst(StorageBaseAddressConstLibfunc),
         StorageBaseAddressFromFelt(StorageBaseAddressFromFeltLibfunc),
         StorageAddressFromBase(StorageAddressFromBaseLibfunc),
         StorageAddressFromBaseAndOffset(StorageAddressFromBaseAndOffsetLibfunc),
         EmitEvent(EmitEventLibfunc),
         GetExecutionInfo(GetterLibfunc<GetExecutionInfoTrait>),
         Testing(TestingLibfunc),
    }, StarkNetConcreteLibfunc
}
