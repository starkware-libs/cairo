use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{StorageAddressConstLibFunc, StorageAddressType, StorageReadLibFunc};

mod syscalls;
use syscalls::SyscallPtrType;

define_type_hierarchy! {
    pub enum StarkNetType {
        StorageAddress(StorageAddressType),
        SyscallPtr(SyscallPtrType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibFunc {
         StorageRead(StorageReadLibFunc),
         StorageAddressConst(StorageAddressConstLibFunc),
    }, StarkNetConcreteLibFunc
}
