use std::marker::PhantomData;

use super::interoperability::ContractAddressType;
use super::syscalls::SyscallGenericLibfunc;
use super::{felt252_span_ty, span_ty};
use crate::extensions::boxing::box_ty;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::int::unsigned::{Uint32Type, Uint64Type};
use crate::extensions::int::unsigned128::Uint128Type;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::structure::StructType;
use crate::extensions::{NamedType, NoGenericArgsGenericType, SpecializationError};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;

/// Trait for implementing getters.
pub trait GetterTraits: Default {
    /// The generic libfunc id for the getter libfunc.
    const STR_ID: &'static str;
    /// The simple sierra generic type returned by the getter.
    type InfoType: NoGenericArgsGenericType;
}

/// Same as GetterTraits, but with a function to return the concrete TypeId.
pub trait GetterTraitsEx: Default {
    /// The generic libfunc id for the getter libfunc.
    const STR_ID: &'static str;
    /// The simple sierra generic type returned by the getter.
    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError>;
}

impl<TGetterTraits: GetterTraits> GetterTraitsEx for TGetterTraits {
    const STR_ID: &'static str = TGetterTraits::STR_ID;
    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        context.get_concrete_type(TGetterTraits::InfoType::id(), &[])
    }
}

/// Libfunc for a getter system call.
#[derive(Default)]
pub struct GetterLibfunc<TGetterTraitsEx: GetterTraitsEx> {
    _phantom: PhantomData<TGetterTraitsEx>,
}
impl<TGetterTraitsEx: GetterTraitsEx> SyscallGenericLibfunc for GetterLibfunc<TGetterTraitsEx> {
    const STR_ID: &'static str = TGetterTraitsEx::STR_ID;

    fn input_tys(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![TGetterTraitsEx::info_type_id(context)?])
    }
}

/// Helper for ExecutionInfo type.
fn get_execution_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::ExecutionInfo")),
            // block_info
            GenericArg::Type(box_ty(context, get_block_info_type(context)?)?),
            // tx_info
            GenericArg::Type(box_ty(context, get_tx_info_type(context)?)?),
            // caller_address
            GenericArg::Type(contract_address_ty.clone()),
            // contract_address
            GenericArg::Type(contract_address_ty),
            // entry_point_selector
            GenericArg::Type(felt252_ty),
        ],
    )
}

/// Helper for v2::ExecutionInfo type.
fn get_execution_info_v2_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string(
                "core::starknet::info::v2::ExecutionInfo",
            )),
            // block_info
            GenericArg::Type(box_ty(context, get_block_info_type(context)?)?),
            // tx_info
            GenericArg::Type(box_ty(context, get_tx_info_v2_type(context)?)?),
            // caller_address
            GenericArg::Type(contract_address_ty.clone()),
            // contract_address
            GenericArg::Type(contract_address_ty),
            // entry_point_selector
            GenericArg::Type(felt252_ty),
        ],
    )
}

/// Helper for BlockInfo type.
fn get_block_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    let u64_ty = context.get_concrete_type(Uint64Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::BlockInfo")),
            // block_number
            GenericArg::Type(u64_ty.clone()),
            // block_timestamp
            GenericArg::Type(u64_ty),
            // sequencer_address
            GenericArg::Type(contract_address_ty),
        ],
    )
}

/// Helper for TxInfo type.
fn get_tx_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::TxInfo")),
            // version
            GenericArg::Type(felt252_ty.clone()),
            // account_contract_address
            GenericArg::Type(contract_address_ty),
            // max_fee
            GenericArg::Type(u128_ty),
            // signature
            GenericArg::Type(felt252_span_ty(context)?),
            // transaction_hash
            GenericArg::Type(felt252_ty.clone()),
            // chain_id
            GenericArg::Type(felt252_ty.clone()),
            // nonce
            GenericArg::Type(felt252_ty),
        ],
    )
}

/// Cairo level user type name for `ResourceBounds`.
const RESOURCE_BOUNDS_USER_TYPE_ID: &str = "core::starknet::info::v2::ResourceBounds";

/// User type for `Span<ResourceBounds>`.
fn resource_bounds_span_ty(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    span_ty(context, get_resource_bounds_type(context)?, RESOURCE_BOUNDS_USER_TYPE_ID)
}

/// Helper for ResourceBounds type.
fn get_resource_bounds_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
    let u64_ty = context.get_concrete_type(Uint64Type::id(), &[])?;
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string(RESOURCE_BOUNDS_USER_TYPE_ID)),
            // resource
            GenericArg::Type(felt252_ty),
            // max_amount
            GenericArg::Type(u64_ty),
            // max_price_per_unit
            GenericArg::Type(u128_ty),
        ],
    )
}

/// Helper for v2::TxInfo type.
fn get_tx_info_v2_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
    let felt252_span_ty = felt252_span_ty(context)?;
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    let u32_ty = context.get_concrete_type(Uint32Type::id(), &[])?;
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::v2::TxInfo")),
            // version
            GenericArg::Type(felt252_ty.clone()),
            // account_contract_address
            GenericArg::Type(contract_address_ty),
            // max_fee
            GenericArg::Type(u128_ty.clone()),
            // signature
            GenericArg::Type(felt252_span_ty.clone()),
            // transaction_hash
            GenericArg::Type(felt252_ty.clone()),
            // chain_id
            GenericArg::Type(felt252_ty.clone()),
            // nonce
            GenericArg::Type(felt252_ty),
            // resource_bounds
            GenericArg::Type(resource_bounds_span_ty(context)?),
            // tip
            GenericArg::Type(u128_ty),
            // paymaster_data
            GenericArg::Type(felt252_span_ty.clone()),
            // nonce_data_availability_mode
            GenericArg::Type(u32_ty.clone()),
            // fee_data_availability_mode
            GenericArg::Type(u32_ty),
            // account_deployment_data
            GenericArg::Type(felt252_span_ty),
        ],
    )
}

#[derive(Default)]
pub struct GetExecutionInfoTrait {}
impl GetterTraitsEx for GetExecutionInfoTrait {
    const STR_ID: &'static str = "get_execution_info_syscall";

    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        box_ty(context, get_execution_info_type(context)?)
    }
}

#[derive(Default)]
pub struct GetExecutionInfoV2Trait {}
impl GetterTraitsEx for GetExecutionInfoV2Trait {
    const STR_ID: &'static str = "get_execution_info_v2_syscall";

    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        box_ty(context, get_execution_info_v2_type(context)?)
    }
}
