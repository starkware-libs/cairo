use std::marker::PhantomData;

use super::felt252_span_ty;
use super::interoperability::ContractAddressType;
use crate::define_libfunc_hierarchy;
use crate::extensions::int::unsigned::Uint64Type;
use crate::extensions::lib_func::{
    LibfuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, SpecializationError,
};
use crate::ids::ConcreteTypeId;
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
pub struct SetSignatureTrait {}
impl TestSetterTraits for SetSignatureTrait {
    const STR_ID: &'static str = "set_signature";

    fn value_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        felt252_span_ty(context)
    }
}

define_libfunc_hierarchy! {
    pub enum TestingLibfunc {
         SetBlockNumber(TestSetterLibfunc<SetBlockNumberTrait>),
         SetBlockTimestamp(TestSetterLibfunc<SetBlockTimestampTrait>),
         SetCallerAddress(TestSetterLibfunc<SetCallerAddressTrait>),
         SetContractAddress(TestSetterLibfunc<SetContractAddressTrait>),
         SetSequencerAddress(TestSetterLibfunc<SetSequencerAddressTrait>),
         SetSignature(TestSetterLibfunc<SetSignatureTrait>),
    }, TestingConcreteLibfunc
}
