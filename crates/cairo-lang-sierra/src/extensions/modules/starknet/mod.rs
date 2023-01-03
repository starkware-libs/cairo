use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{
    StorageAddressConstLibfunc, StorageAddressType, StorageReadLibfunc, StorageWriteLibfunc,
};

pub mod syscalls;
use syscalls::SystemType;

pub mod interoperability;
use interoperability::{CallContractLibfunc, ContractAddressConstLibfunc, ContractAddressType};

define_type_hierarchy! {
    pub enum StarkNetType {
        ContractAddress(ContractAddressType),
        StorageAddress(StorageAddressType),
        System(SystemType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibfunc {
         CallContract(CallContractLibfunc),
         ContractAddressConst(ContractAddressConstLibfunc),
         StorageRead(StorageReadLibfunc),
         StorageWrite(StorageWriteLibfunc),
         StorageAddressConst(StorageAddressConstLibfunc),
    }, StarkNetConcreteLibfunc
}
