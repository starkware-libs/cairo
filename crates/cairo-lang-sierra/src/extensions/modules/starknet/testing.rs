use std::marker::PhantomData;

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
    /// The simple sierra generic type as the setter's value.
    type ValueType: NoGenericArgsGenericType;
}

/// Same as GetterTraits, but with a function to return the concrete TypeId.
pub trait TestSetterTraitsEx: Default {
    /// The generic libfunc id for the setter libfunc.
    const STR_ID: &'static str;
    /// The value type for the setter.
    fn value_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError>;
}

impl<TTestSetterTraits: TestSetterTraits> TestSetterTraitsEx for TTestSetterTraits {
    const STR_ID: &'static str = TTestSetterTraits::STR_ID;

    fn value_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        context.get_concrete_type(TTestSetterTraits::ValueType::id(), &[])
    }
}

/// Libfunc for a test setter.
#[derive(Default)]
pub struct TestSetterLibfunc<TTestSetterTraitsEx: TestSetterTraitsEx> {
    _phantom: PhantomData<TTestSetterTraitsEx>,
}
impl<TTestSetterTraitsEx: TestSetterTraitsEx> NoGenericArgsGenericLibfunc
    for TestSetterLibfunc<TTestSetterTraitsEx>
{
    const STR_ID: &'static str = TTestSetterTraitsEx::STR_ID;
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![TTestSetterTraitsEx::value_type_id(context)?],
            vec![],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

#[derive(Default)]
pub struct SetBlockNumberTrait {}
impl TestSetterTraits for SetBlockNumberTrait {
    const STR_ID: &'static str = "set_block_number";
    type ValueType = Uint64Type;
}

#[derive(Default)]
pub struct SetBlockTimestampTrait {}
impl TestSetterTraits for SetBlockTimestampTrait {
    const STR_ID: &'static str = "set_block_timestamp";
    type ValueType = Uint64Type;
}

#[derive(Default)]
pub struct SetCallerAddressTrait {}
impl TestSetterTraits for SetCallerAddressTrait {
    const STR_ID: &'static str = "set_caller_address";
    type ValueType = ContractAddressType;
}

#[derive(Default)]
pub struct SetContractAddressTrait {}
impl TestSetterTraits for SetContractAddressTrait {
    const STR_ID: &'static str = "set_contract_address";
    type ValueType = ContractAddressType;
}

#[derive(Default)]
pub struct SetSequencerAddressTrait {}
impl TestSetterTraits for SetSequencerAddressTrait {
    const STR_ID: &'static str = "set_sequencer_address";
    type ValueType = ContractAddressType;
}

define_libfunc_hierarchy! {
    pub enum TestingLibfunc {
         SetBlockNumber(TestSetterLibfunc<SetBlockNumberTrait>),
         SetBlockTimestamp(TestSetterLibfunc<SetBlockTimestampTrait>),
         SetCallerAddress(TestSetterLibfunc<SetCallerAddressTrait>),
         SetContractAddress(TestSetterLibfunc<SetContractAddressTrait>),
         SetSequencerAddress(TestSetterLibfunc<SetSequencerAddressTrait>),
    }, TestingConcreteLibfunc
}
