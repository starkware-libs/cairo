use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{
    StorageAddressConstLibFunc, StorageAddressType, StorageReadLibFunc, StorageWriteLibFunc,
};

pub mod syscalls;
use syscalls::SystemType;

pub mod interoperability;
use interoperability::{CallContractLibFunc, ContractAddressConstLibFunc, ContractAddressType};

define_type_hierarchy! {
    pub enum StarkNetType {
        ContractAddress(ContractAddressType),
        StorageAddress(StorageAddressType),
        System(SystemType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibFunc {
         CallContract(CallContractLibFunc),
         ContractAddressConst(ContractAddressConstLibFunc),
         StorageRead(StorageReadLibFunc),
         StorageWrite(StorageWriteLibFunc),
         StorageAddressConst(StorageAddressConstLibFunc),
    }, StarkNetConcreteLibFunc
}
