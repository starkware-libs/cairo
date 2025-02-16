use num_bigint::BigInt;

use super::felt252_span_ty;
use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::consts::{ConstGenLibfunc, WrapConstGenLibfunc};
use crate::extensions::felt252::Felt252Type;
use crate::extensions::lib_func::{LibfuncSignature, SignatureSpecializationContext};
use crate::extensions::modules::get_bool_type;
use crate::extensions::try_from_felt252::TryFromFelt252;
use crate::extensions::utils::reinterpret_cast_signature;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for Starknet contract address, a value in the range [0, 2 ** 251).
#[derive(Default)]
pub struct ContractAddressType {}
impl NoGenericArgsGenericType for ContractAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("ContractAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// Libfunc for creating a constant storage address.
#[derive(Default)]
pub struct ContractAddressConstLibfuncWrapped {}
impl ConstGenLibfunc for ContractAddressConstLibfuncWrapped {
    const STR_ID: &'static str = "contract_address_const";
    const GENERIC_TYPE_ID: GenericTypeId = <ContractAddressType as NoGenericArgsGenericType>::ID;

    fn bound() -> BigInt {
        BigInt::from(2).pow(251)
    }
}

pub type ContractAddressConstLibfunc = WrapConstGenLibfunc<ContractAddressConstLibfuncWrapped>;

/// Libfunc for attempting to convert a felt252 into a contract address.
#[derive(Default)]
pub struct ContractAddressTryFromFelt252Libfunc;
impl TryFromFelt252 for ContractAddressTryFromFelt252Libfunc {
    const STR_ID: &'static str = "contract_address_try_from_felt252";
    const GENERIC_TYPE_ID: GenericTypeId = <ContractAddressType as NoGenericArgsGenericType>::ID;
}

/// Libfunc for converting a ContractAddress into a felt252.
#[derive(Default)]
pub struct ContractAddressToFelt252Libfunc {}
impl NoGenericArgsGenericLibfunc for ContractAddressToFelt252Libfunc {
    const STR_ID: &'static str = "contract_address_to_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            context.get_concrete_type(ContractAddressType::id(), &[])?,
            context.get_concrete_type(Felt252Type::id(), &[])?,
        ))
    }
}

/// Type for Starknet class hash, a value in the range [0, 2 ** 251).
#[derive(Default)]
pub struct ClassHashType {}
impl NoGenericArgsGenericType for ClassHashType {
    const ID: GenericTypeId = GenericTypeId::new_inline("ClassHash");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// Libfunc for creating a constant storage address.
#[derive(Default)]
pub struct ClassHashConstLibfuncWrapped {}
impl ConstGenLibfunc for ClassHashConstLibfuncWrapped {
    const STR_ID: &'static str = "class_hash_const";
    const GENERIC_TYPE_ID: GenericTypeId = <ClassHashType as NoGenericArgsGenericType>::ID;

    fn bound() -> BigInt {
        BigInt::from(2).pow(251)
    }
}

pub type ClassHashConstLibfunc = WrapConstGenLibfunc<ClassHashConstLibfuncWrapped>;

/// Libfunc for attempting to convert a felt252 into a class hash.
#[derive(Default)]
pub struct ClassHashTryFromFelt252Trait;
impl TryFromFelt252 for ClassHashTryFromFelt252Trait {
    const STR_ID: &'static str = "class_hash_try_from_felt252";
    const GENERIC_TYPE_ID: GenericTypeId = <ClassHashType as NoGenericArgsGenericType>::ID;
}

/// Libfunc for converting a class hash into a felt252.
#[derive(Default)]
pub struct ClassHashToFelt252Libfunc {}
impl NoGenericArgsGenericLibfunc for ClassHashToFelt252Libfunc {
    const STR_ID: &'static str = "class_hash_to_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            context.get_concrete_type(ClassHashType::id(), &[])?,
            context.get_concrete_type(Felt252Type::id(), &[])?,
        ))
    }
}

/// Libfunc for a storage call contract system call.
#[derive(Default)]
pub struct CallContractLibfunc {}
impl SyscallGenericLibfunc for CallContractLibfunc {
    const STR_ID: &'static str = "call_contract_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Address
            context.get_concrete_type(ContractAddressType::id(), &[])?,
            // Entry point selector.
            context.get_concrete_type(Felt252Type::id(), &[])?,
            // Call data
            felt252_span_ty(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![felt252_span_ty(context)?])
    }
}

/// Libfunc for a deploying a declared class system call.
#[derive(Default)]
pub struct DeployLibfunc {}
impl SyscallGenericLibfunc for DeployLibfunc {
    const STR_ID: &'static str = "deploy_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Class hash
            context.get_concrete_type(ClassHashType::id(), &[])?,
            // Contract address salt
            context.get_concrete_type(Felt252Type::id(), &[])?,
            // Constructor call data
            felt252_span_ty(context)?,
            // Deploy from zero
            get_bool_type(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            context.get_concrete_type(ContractAddressType::id(), &[])?,
            felt252_span_ty(context)?,
        ])
    }
}

/// Libfunc for a library call system call.
#[derive(Default)]
pub struct LibraryCallLibfunc {}
impl SyscallGenericLibfunc for LibraryCallLibfunc {
    const STR_ID: &'static str = "library_call_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Class hash
            context.get_concrete_type(ClassHashType::id(), &[])?,
            // Function selector
            context.get_concrete_type(Felt252Type::id(), &[])?,
            // Call data
            felt252_span_ty(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![felt252_span_ty(context)?])
    }
}

/// Libfunc for sending message to l1 system call.
#[derive(Default)]
pub struct SendMessageToL1Libfunc {}
impl SyscallGenericLibfunc for SendMessageToL1Libfunc {
    const STR_ID: &'static str = "send_message_to_l1_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Address
            context.get_concrete_type(Felt252Type::id(), &[])?,
            // Payload
            felt252_span_ty(context)?,
        ])
    }

    fn success_output_tys(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![])
    }
}

/// Libfunc for the `meta_tx_v0` system call.
#[derive(Default)]
pub struct MetaTxV0Libfunc {}
impl SyscallGenericLibfunc for MetaTxV0Libfunc {
    const STR_ID: &'static str = "meta_tx_v0_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let felt252_span_ty = felt252_span_ty(context)?;
        Ok(vec![
            // Address.
            context.get_concrete_type(ContractAddressType::id(), &[])?,
            // Entry point selector.
            context.get_concrete_type(Felt252Type::id(), &[])?,
            // Call data.
            felt252_span_ty.clone(),
            // Signature.
            felt252_span_ty,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![felt252_span_ty(context)?])
    }
}
