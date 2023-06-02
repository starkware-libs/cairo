use std::marker::PhantomData;

use super::interoperability::ContractAddressType;
use super::{felt252_span_ty, ArrayType};
use crate::define_libfunc_hierarchy;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::int::unsigned::Uint64Type;
use crate::extensions::int::unsigned128::Uint128Type;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::structure::StructType;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;
/// Trait for implementing test setters.
pub trait TestSetterTraits: Default {
    /// The generic libfunc id for the setter libfunc.
    const STR_ID: &'static str;
    /// The value type for the setter.
    fn value_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError>;
}

/// Same as GetterTraits, but with a function to return the concrete TypeId.
pub trait BasicTypeTestSetterTraits: Default {
    /// The generic libfunc id for the setter libfunc.
    const STR_ID: &'static str;
    /// The simple sierra generic type as the setter's value.
    type ValueType: NoGenericArgsGenericType;
}

impl<T: BasicTypeTestSetterTraits> TestSetterTraits for T {
    const STR_ID: &'static str = T::STR_ID;

    fn value_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        context.get_concrete_type(T::ValueType::id(), &[])
    }
}

/// Libfunc for a test setter.
#[derive(Default)]
pub struct TestSetterLibfunc<TTestSetterTraits: TestSetterTraits> {
    _phantom: PhantomData<TTestSetterTraits>,
}
impl<TTestSetterTraits: TestSetterTraits> NoGenericArgsGenericLibfunc
    for TestSetterLibfunc<TTestSetterTraits>
{
    const STR_ID: &'static str = TTestSetterTraits::STR_ID;
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![TTestSetterTraits::value_type_id(context)?],
            vec![],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

#[derive(Default)]
pub struct SetBlockNumberTrait {}
impl BasicTypeTestSetterTraits for SetBlockNumberTrait {
    const STR_ID: &'static str = "set_block_number";
    type ValueType = Uint64Type;
}

#[derive(Default)]
pub struct SetBlockTimestampTrait {}
impl BasicTypeTestSetterTraits for SetBlockTimestampTrait {
    const STR_ID: &'static str = "set_block_timestamp";
    type ValueType = Uint64Type;
}

#[derive(Default)]
pub struct SetCallerAddressTrait {}
impl BasicTypeTestSetterTraits for SetCallerAddressTrait {
    const STR_ID: &'static str = "set_caller_address";
    type ValueType = ContractAddressType;
}

#[derive(Default)]
pub struct SetContractAddressTrait {}
impl BasicTypeTestSetterTraits for SetContractAddressTrait {
    const STR_ID: &'static str = "set_contract_address";
    type ValueType = ContractAddressType;
}

#[derive(Default)]
pub struct SetSequencerAddressTrait {}
impl BasicTypeTestSetterTraits for SetSequencerAddressTrait {
    const STR_ID: &'static str = "set_sequencer_address";
    type ValueType = ContractAddressType;
}

#[derive(Default)]
pub struct SetVersionTrait {}
impl BasicTypeTestSetterTraits for SetVersionTrait {
    const STR_ID: &'static str = "set_version";
    type ValueType = Felt252Type;
}

#[derive(Default)]
pub struct SetAccountContractAddressTrait {}
impl BasicTypeTestSetterTraits for SetAccountContractAddressTrait {
    const STR_ID: &'static str = "set_account_contract_address";
    type ValueType = ContractAddressType;
}

#[derive(Default)]
pub struct SetMaxFeeTrait {}
impl BasicTypeTestSetterTraits for SetMaxFeeTrait {
    const STR_ID: &'static str = "set_max_fee";
    type ValueType = Uint128Type;
}

#[derive(Default)]
pub struct SetTransactionHashTrait {}
impl BasicTypeTestSetterTraits for SetTransactionHashTrait {
    const STR_ID: &'static str = "set_transaction_hash";
    type ValueType = Felt252Type;
}

#[derive(Default)]
pub struct SetChainIdTrait {}
impl BasicTypeTestSetterTraits for SetChainIdTrait {
    const STR_ID: &'static str = "set_chain_id";
    type ValueType = Felt252Type;
}

#[derive(Default)]
pub struct SetNonceTrait {}
impl BasicTypeTestSetterTraits for SetNonceTrait {
    const STR_ID: &'static str = "set_nonce";
    type ValueType = Felt252Type;
}

#[derive(Default)]
pub struct SetSignatureTrait {}
impl TestSetterTraits for SetSignatureTrait {
    const STR_ID: &'static str = "set_signature";

    fn value_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        felt252_span_ty(context)
    }
}

/// Trait for implementing test getters.
pub trait TestGetterTraits: Default {
    /// The generic libfunc id for the getter libfunc.
    const STR_ID: &'static str;
    /// The value type for the getter.
    fn return_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<OutputVarInfo, SpecializationError>;
}

/// Libfunc for a test getter.
#[derive(Default)]
pub struct TestGetterLibfunc<TTestGetterTraits: TestGetterTraits> {
    _phantom: PhantomData<TTestGetterTraits>,
}
impl<TTestGetterTraits: TestGetterTraits> NoGenericArgsGenericLibfunc
    for TestGetterLibfunc<TTestGetterTraits>
{
    const STR_ID: &'static str = TTestGetterTraits::STR_ID;
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![TTestGetterTraits::return_type_id(context)?],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

#[derive(Default)]
pub struct PopLogsLibfunc {}

impl NoGenericArgsGenericLibfunc for PopLogsLibfunc {
    const STR_ID: &'static str = "pop_logs";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let log_array_ty =
            context.get_wrapped_concrete_type(ArrayType::id(), get_log_type(context)?)?;

        Ok(LibfuncSignature::new_non_branch(
            vec![context.get_concrete_type(ContractAddressType::id(), &[])?],
            vec![OutputVarInfo {
                ty: log_array_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Helper for Log type def.
fn get_log_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt252_array_ty = context.get_wrapped_concrete_type(
        ArrayType::id(),
        context.get_concrete_type(Felt252Type::id(), &[])?,
    )?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::testing::Log")),
            // keys
            GenericArg::Type(felt252_array_ty.clone()),
            // data
            GenericArg::Type(felt252_array_ty),
        ],
    )
}

define_libfunc_hierarchy! {
    pub enum TestingLibfunc {
         SetBlockNumber(TestSetterLibfunc<SetBlockNumberTrait>),
         SetBlockTimestamp(TestSetterLibfunc<SetBlockTimestampTrait>),
         SetCallerAddress(TestSetterLibfunc<SetCallerAddressTrait>),
         SetContractAddress(TestSetterLibfunc<SetContractAddressTrait>),
         SetSequencerAddress(TestSetterLibfunc<SetSequencerAddressTrait>),
         SetVersion(TestSetterLibfunc<SetVersionTrait>),
         SetAccountContractAddress(TestSetterLibfunc<SetAccountContractAddressTrait>),
         SetMaxFee(TestSetterLibfunc<SetMaxFeeTrait>),
         SetTransactionHash(TestSetterLibfunc<SetTransactionHashTrait>),
         SetChainId(TestSetterLibfunc<SetChainIdTrait>),
         SetNonce(TestSetterLibfunc<SetNonceTrait>),
         SetSignature(TestSetterLibfunc<SetSignatureTrait>),
         PopLogs(PopLogsLibfunc),
    }, TestingConcreteLibfunc
}
