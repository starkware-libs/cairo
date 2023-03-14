use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{
    StorageAddressToFeltLibfunc, StorageBaseAddressConstLibfunc, StorageBaseAddressType,
    StorageReadLibfunc, StorageWriteLibfunc,
};

pub mod syscalls;
use syscalls::{ReplaceClassLibfunc, SystemType};

pub mod getter;

pub mod emit_event;
use emit_event::EmitEventLibfunc;
pub mod testing;

pub mod interoperability;
use interoperability::{CallContractLibfunc, ContractAddressConstLibfunc, ContractAddressType};

use self::getter::{GetExecutionInfoTrait, GetterLibfunc};
use self::interoperability::{
    ClassHashConstLibfunc, ClassHashToFeltLibfunc, ClassHashTryFromFeltTrait, ClassHashType,
    ContractAddressToFeltLibfunc, ContractAddressTryFromFeltLibfunc, DeployLibfunc,
    LibraryCallL1HandlerLibfunc, LibraryCallLibfunc, SendMessageToL1Libfunc,
};
use self::storage::{
    StorageAddressFromBaseAndOffsetLibfunc, StorageAddressFromBaseLibfunc,
    StorageAddressTryFromFeltTrait, StorageAddressType, StorageBaseAddressFromFeltLibfunc,
};
use self::testing::TestingLibfunc;
use super::try_from_felt::TryFromFeltLibfunc;

define_type_hierarchy! {
    pub enum StarkNetType {
        ClassHash(ClassHashType),
        ContractAddress(ContractAddressType),
        StorageBaseAddress(StorageBaseAddressType),
        StorageAddress(StorageAddressType),
        System(SystemType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibfunc {
         CallContract(CallContractLibfunc),
         ClassHashConst(ClassHashConstLibfunc),
         ClassHashTryFromFelt(TryFromFeltLibfunc<ClassHashTryFromFeltTrait>),
         ClassHashToFelt(ClassHashToFeltLibfunc),
         ContractAddressConst(ContractAddressConstLibfunc),
         ContractAddressTryFromFelt(TryFromFeltLibfunc<ContractAddressTryFromFeltLibfunc>),
         ContractAddressToFelt(ContractAddressToFeltLibfunc),
         StorageRead(StorageReadLibfunc),
         StorageWrite(StorageWriteLibfunc),
         StorageBaseAddressConst(StorageBaseAddressConstLibfunc),
         StorageBaseAddressFromFelt(StorageBaseAddressFromFeltLibfunc),
         StorageAddressFromBase(StorageAddressFromBaseLibfunc),
         StorageAddressFromBaseAndOffset(StorageAddressFromBaseAndOffsetLibfunc),
         StorageAddressToFelt(StorageAddressToFeltLibfunc),
         StorageAddressTryFromFelt(TryFromFeltLibfunc<StorageAddressTryFromFeltTrait>),
         EmitEvent(EmitEventLibfunc),
         GetExecutionInfo(GetterLibfunc<GetExecutionInfoTrait>),
         Deploy(DeployLibfunc),
         LibraryCall(LibraryCallLibfunc),
         LibraryCallL1Handler(LibraryCallL1HandlerLibfunc),
         ReplaceClass(ReplaceClassLibfunc),
         SendMessageToL1(SendMessageToL1Libfunc),
         Testing(TestingLibfunc),
    }, StarkNetConcreteLibfunc
}
