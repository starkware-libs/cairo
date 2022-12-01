use super::storage::{StorageAddressType, StorageRead};
use crate::extensions::syscalls::SyscallPtrType;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum StarkNetType {
        StorageAddress(StorageAddressType),
        SyscallPtr(SyscallPtrType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibFunc {
         StorageRead(StorageRead),
    }, StarkNetConcreteLibFunc
}
