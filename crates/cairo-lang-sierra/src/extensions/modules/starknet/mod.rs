use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::{NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;
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

use self::getter::{
    GetBlockNumberTrait, GetBlockTimestampTrait, GetCallerAddressTrait, GetContractAddressTrait,
    GetSequencerAddressTrait, GetTxInfoTrait, GetterLibfunc,
};
use self::interoperability::{ContractAddressToFeltLibfunc, ContractAddressTryFromFeltLibfunc};
use self::storage::{
    StorageAddressFromBaseAndOffsetLibfunc, StorageAddressFromBaseLibfunc, StorageAddressType,
    StorageBaseAddressFromFeltLibfunc,
};
use self::testing::TestingLibfunc;
use super::array::ArrayType;
use super::felt::FeltType;
use super::snapshot::SnapshotType;
use super::structure::StructType;
use super::uint128::Uint128Type;

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
         GetBlockNumber(GetterLibfunc<GetBlockNumberTrait>),
         GetBlockTimestamp(GetterLibfunc<GetBlockTimestampTrait>),
         GetCallerAddress(GetterLibfunc<GetCallerAddressTrait>),
         GetContractAddress(GetterLibfunc<GetContractAddressTrait>),
         GetSequencerAddress(GetterLibfunc<GetSequencerAddressTrait>),
         GetTxInfo(GetterLibfunc<GetTxInfoTrait>),
         Testing(TestingLibfunc),
    }, StarkNetConcreteLibfunc
}

/// Helper for TxInfo type def.
fn get_tx_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    let felt_array_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
    let felt_array_snapshot_ty =
        context.get_wrapped_concrete_type(SnapshotType::id(), felt_array_ty)?;
    let felt_array_span_ty = context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::array::Span::<core::felt>")),
            GenericArg::Type(felt_array_snapshot_ty),
        ],
    )?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::TxInfo")),
            // version
            GenericArg::Type(felt_ty.clone()),
            // account_contract_address
            GenericArg::Type(contract_address_ty),
            // max_fee
            GenericArg::Type(u128_ty),
            // signature
            GenericArg::Type(felt_array_span_ty),
            // transaction_hash
            GenericArg::Type(felt_ty.clone()),
            // chain_id
            GenericArg::Type(felt_ty.clone()),
            // nonce
            GenericArg::Type(felt_ty),
        ],
    )
}
