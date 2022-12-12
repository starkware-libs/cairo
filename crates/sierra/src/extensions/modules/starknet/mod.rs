use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{StorageAddressConstLibFunc, StorageAddressType, StorageReadLibFunc};

mod syscalls;
use syscalls::SystemType;

define_type_hierarchy! {
    pub enum StarkNetType {
        StorageAddress(StorageAddressType),
        System(SystemType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibFunc {
         StorageRead(StorageReadLibFunc),
         StorageAddressConst(StorageAddressConstLibFunc),
    }, StarkNetConcreteLibFunc
}
