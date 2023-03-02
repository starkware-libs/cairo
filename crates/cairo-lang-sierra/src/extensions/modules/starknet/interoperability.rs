use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::array::ArrayType;
use crate::extensions::consts::{ConstGenLibfunc, WrapConstGenLibfunc};
use crate::extensions::felt::FeltType;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::try_from_felt::TryFromFelt;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for Starknet storage address, a value in the range [0, 2 ** 250).
#[derive(Default)]
pub struct ContractAddressType {}
impl NoGenericArgsGenericType for ContractAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("ContractAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// Libfunc for creating a constant storage address.
#[derive(Default)]
pub struct ContractAddressConstLibfuncWrapped {}
impl ConstGenLibfunc for ContractAddressConstLibfuncWrapped {
    const STR_ID: &'static str = "contract_address_const";
    const GENERIC_TYPE_ID: GenericTypeId = <ContractAddressType as NoGenericArgsGenericType>::ID;
}

pub type ContractAddressConstLibfunc = WrapConstGenLibfunc<ContractAddressConstLibfuncWrapped>;

/// Libfunc for attempting to convert a felt into a contract address.
#[derive(Default)]
pub struct ContractAddressTryFromFeltTrait;
impl TryFromFelt for ContractAddressTryFromFeltTrait {
    const STR_ID: &'static str = "contract_address_try_from_felt";
    const GENERIC_TYPE_ID: GenericTypeId = <ContractAddressType as NoGenericArgsGenericType>::ID;
}

/// Libfunc for converting a ContractAddress into a felt.
#[derive(Default)]
pub struct ContractAddressToFeltLibfunc {}
impl NoGenericArgsGenericLibfunc for ContractAddressToFeltLibfunc {
    const STR_ID: &'static str = "contract_address_to_felt";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: context.get_concrete_type(ContractAddressType::id(), &[])?,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(FeltType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
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
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(vec![
            // Address
            context.get_concrete_type(ContractAddressType::id(), &[])?,
            // Entry point selector.
            felt_ty.clone(),
            // Call data
            context.get_wrapped_concrete_type(ArrayType::id(), felt_ty)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_wrapped_concrete_type(
            ArrayType::id(),
            context.get_concrete_type(FeltType::id(), &[])?,
        )?])
    }
}
