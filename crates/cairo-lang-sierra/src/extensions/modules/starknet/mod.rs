use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::{NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod storage;
use storage::{
    GetBlockHashLibfunc, StorageAddressToFelt252Libfunc, StorageBaseAddressConstLibfunc,
    StorageBaseAddressType, StorageReadLibfunc, StorageWriteLibfunc,
};

pub mod syscalls;
use syscalls::{ReplaceClassLibfunc, SystemType};

pub mod emit_event;
use emit_event::EmitEventLibfunc;

pub mod getter;

pub mod secp256;
use secp256::{Secp256Libfunc, Secp256PointType};
pub mod secp256k1;
pub mod secp256r1;

pub mod testing;

pub mod interoperability;
use interoperability::{CallContractLibfunc, ContractAddressConstLibfunc, ContractAddressType};

use self::getter::{GetExecutionInfoTrait, GetterLibfunc};
use self::interoperability::{
    ClassHashConstLibfunc, ClassHashToFelt252Libfunc, ClassHashTryFromFelt252Trait, ClassHashType,
    ContractAddressToFelt252Libfunc, ContractAddressTryFromFelt252Libfunc, DeployLibfunc,
    LibraryCallLibfunc, SendMessageToL1Libfunc,
};
use self::storage::{
    StorageAddressFromBaseAndOffsetLibfunc, StorageAddressFromBaseLibfunc,
    StorageAddressTryFromFelt252Trait, StorageAddressType, StorageBaseAddressFromFelt252Libfunc,
};
use self::syscalls::KeccakLibfunc;
use self::testing::TestingLibfunc;
use super::array::ArrayType;
use super::felt252::Felt252Type;
use super::int::unsigned::Uint64Type;
use super::snapshot::snapshot_ty;
use super::structure::StructType;
use super::try_from_felt252::TryFromFelt252Libfunc;

define_type_hierarchy! {
    pub enum StarkNetType {
        ClassHash(ClassHashType),
        ContractAddress(ContractAddressType),
        StorageBaseAddress(StorageBaseAddressType),
        StorageAddress(StorageAddressType),
        System(SystemType),
        Secp256Point(Secp256PointType),
    }, StarkNetTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum StarkNetLibfunc {
         CallContract(CallContractLibfunc),
         ClassHashConst(ClassHashConstLibfunc),
         ClassHashTryFromFelt252(TryFromFelt252Libfunc<ClassHashTryFromFelt252Trait>),
         ClassHashToFelt252(ClassHashToFelt252Libfunc),
         ContractAddressConst(ContractAddressConstLibfunc),
         ContractAddressTryFromFelt252(TryFromFelt252Libfunc<ContractAddressTryFromFelt252Libfunc>),
         ContractAddressToFelt252(ContractAddressToFelt252Libfunc),
         StorageRead(StorageReadLibfunc),
         StorageWrite(StorageWriteLibfunc),
         StorageBaseAddressConst(StorageBaseAddressConstLibfunc),
         StorageBaseAddressFromFelt252(StorageBaseAddressFromFelt252Libfunc),
         StorageAddressFromBase(StorageAddressFromBaseLibfunc),
         StorageAddressFromBaseAndOffset(StorageAddressFromBaseAndOffsetLibfunc),
         StorageAddressToFelt252(StorageAddressToFelt252Libfunc),
         StorageAddressTryFromFelt252(TryFromFelt252Libfunc<StorageAddressTryFromFelt252Trait>),
         EmitEvent(EmitEventLibfunc),
         GetBlockHash(GetBlockHashLibfunc),
         GetExecutionInfo(GetterLibfunc<GetExecutionInfoTrait>),
         Deploy(DeployLibfunc),
         Keccak(KeccakLibfunc),
         LibraryCall(LibraryCallLibfunc),
         ReplaceClass(ReplaceClassLibfunc),
         SendMessageToL1(SendMessageToL1Libfunc),
         Testing(TestingLibfunc),
         Secp256(Secp256Libfunc),
    }, StarkNetConcreteLibfunc
}

/// User type for `Span<felt252>`.
fn felt252_span_ty(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_array_ty = context.get_wrapped_concrete_type(
        ArrayType::id(),
        context.get_concrete_type(Felt252Type::id(), &[])?,
    )?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::array::Span::<core::felt252>")),
            GenericArg::Type(snapshot_ty(context, felt252_array_ty)?),
        ],
    )
}

/// User type for `Span<u64>`.
fn u64_span_ty(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let u64_array_ty = context.get_wrapped_concrete_type(
        ArrayType::id(),
        context.get_concrete_type(Uint64Type::id(), &[])?,
    )?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string(
                "core::array::Span::<core::integer::u64>",
            )),
            GenericArg::Type(snapshot_ty(context, u64_array_ty)?),
        ],
    )
}
