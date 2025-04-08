use num_bigint::BigInt;

use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::consts::{ConstGenLibfunc, WrapConstGenLibfunc};
use crate::extensions::felt252::Felt252Type;
use crate::extensions::int::unsigned::{Uint8Type, Uint32Type, Uint64Type};
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::try_from_felt252::TryFromFelt252;
use crate::extensions::utils::reinterpret_cast_signature;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for Starknet storage base address, a value in the range [0, 2 ** 251 - 256).
#[derive(Default)]
pub struct StorageBaseAddressType {}
impl NoGenericArgsGenericType for StorageBaseAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("StorageBaseAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// Libfunc for creating a constant storage base address.
#[derive(Default)]
pub struct StorageBaseAddressConstLibfuncWrapped {}
impl ConstGenLibfunc for StorageBaseAddressConstLibfuncWrapped {
    const STR_ID: &'static str = ("storage_base_address_const");
    const GENERIC_TYPE_ID: GenericTypeId = <StorageBaseAddressType as NoGenericArgsGenericType>::ID;

    fn bound() -> BigInt {
        BigInt::from(2).pow(251) - 256
    }
}

pub type StorageBaseAddressConstLibfunc =
    WrapConstGenLibfunc<StorageBaseAddressConstLibfuncWrapped>;

/// Type for Starknet storage base address, a value in the range [0, 2 ** 251).
#[derive(Default)]
pub struct StorageAddressType {}
impl NoGenericArgsGenericType for StorageAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("StorageAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// Libfunc for converting a StorageAddress into a felt252.
#[derive(Default)]
pub struct StorageAddressToFelt252Libfunc {}
impl NoGenericArgsGenericLibfunc for StorageAddressToFelt252Libfunc {
    const STR_ID: &'static str = "storage_address_to_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            context.get_concrete_type(StorageAddressType::id(), &[])?,
            context.get_concrete_type(Felt252Type::id(), &[])?,
        ))
    }
}

/// Libfunc for attempting to convert a felt252 into a storage address.
#[derive(Default)]
pub struct StorageAddressTryFromFelt252Trait;
impl TryFromFelt252 for StorageAddressTryFromFelt252Trait {
    const STR_ID: &'static str = "storage_address_try_from_felt252";
    const GENERIC_TYPE_ID: GenericTypeId = <StorageAddressType as NoGenericArgsGenericType>::ID;
}

/// Libfunc for converting a base address into a storage address.
#[derive(Default)]
pub struct StorageAddressFromBaseLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageAddressFromBaseLibfunc {
    const STR_ID: &'static str = "storage_address_from_base";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            context.get_concrete_type(StorageBaseAddressType::id(), &[])?,
            context.get_concrete_type(StorageAddressType::id(), &[])?,
        ))
    }
}

/// Libfunc for converting a base address and offset into a storage address.
#[derive(Default)]
pub struct StorageAddressFromBaseAndOffsetLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageAddressFromBaseAndOffsetLibfunc {
    const STR_ID: &'static str = "storage_address_from_base_and_offset";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(context.get_concrete_type(StorageBaseAddressType::id(), &[])?),
                ParamSignature::new(context.get_concrete_type(Uint8Type::id(), &[])?)
                    .with_allow_const(),
            ],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(StorageAddressType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for converting a felt252 into a storage base address.
#[derive(Default)]
pub struct StorageBaseAddressFromFelt252Libfunc {}
impl NoGenericArgsGenericLibfunc for StorageBaseAddressFromFelt252Libfunc {
    const STR_ID: &'static str = "storage_base_address_from_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check_ty = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_ty.clone()).with_allow_add_const(),
                ParamSignature::new(context.get_concrete_type(Felt252Type::id(), &[])?),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_ty, 0),
                OutputVarInfo {
                    ty: context.get_concrete_type(StorageBaseAddressType::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for a storage read system call.
#[derive(Default)]
pub struct StorageReadLibfunc {}
impl SyscallGenericLibfunc for StorageReadLibfunc {
    const STR_ID: &'static str = "storage_read_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Address domain.
            context.get_concrete_type(Uint32Type::id(), &[])?,
            // Storage key.
            context.get_concrete_type(StorageAddressType::id(), &[])?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(Felt252Type::id(), &[])?])
    }
}

/// Libfunc for a storage write system call.
#[derive(Default)]
pub struct StorageWriteLibfunc {}
impl SyscallGenericLibfunc for StorageWriteLibfunc {
    const STR_ID: &'static str = "storage_write_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Address domain
            context.get_concrete_type(Uint32Type::id(), &[])?,
            // Storage key
            context.get_concrete_type(StorageAddressType::id(), &[])?,
            // Value
            context.get_concrete_type(Felt252Type::id(), &[])?,
        ])
    }

    fn success_output_tys(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![])
    }
}

/// Libfunc for a get block hash system call.
#[derive(Default)]
pub struct GetBlockHashLibfunc {}
impl SyscallGenericLibfunc for GetBlockHashLibfunc {
    const STR_ID: &'static str = "get_block_hash_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Block number.
            context.get_concrete_type(Uint64Type::id(), &[])?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Block hash.
            context.get_concrete_type(Felt252Type::id(), &[])?,
        ])
    }
}
