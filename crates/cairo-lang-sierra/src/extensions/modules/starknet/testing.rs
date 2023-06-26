use std::marker::PhantomData;

use num_bigint::BigInt;

use super::felt252_span_ty;
use super::interoperability::ContractAddressType;
use crate::define_libfunc_hierarchy;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::int::unsigned::Uint64Type;
use crate::extensions::int::unsigned128::Uint128Type;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::ConcreteTypeId;
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

#[derive(Default)]
pub struct PopLogLibfunc {}

impl NoGenericArgsGenericLibfunc for PopLogLibfunc {
    const STR_ID: &'static str = "pop_log";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
        let span_ty = felt252_span_ty(context)?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(contract_address_ty)],
            branch_signatures: vec![
                // Some variant branch.
                BranchSignature {
                    vars: vec![
                        // keys
                        OutputVarInfo {
                            ty: span_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                        // data
                        OutputVarInfo {
                            ty: span_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // None variant branch.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for creating a general cheatcode.
#[derive(Default)]
pub struct CheatcodeLibfunc {}
impl NamedLibfunc for CheatcodeLibfunc {
    type Concrete = CheatcodeConcreteLibfunc;
    const STR_ID: &'static str = "cheatcode";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        if args.len() != 1 {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }

        let span_ty = felt252_span_ty(context)?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // input
                ParamSignature::new(span_ty.clone()),
            ],
            branch_signatures: vec![BranchSignature {
                vars: vec![
                    // output
                    OutputVarInfo {
                        ty: span_ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(selector)] => Ok(CheatcodeConcreteLibfunc {
                selector: selector.clone(),
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            [_] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }
    }
}

pub struct CheatcodeConcreteLibfunc {
    pub selector: BigInt,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for CheatcodeConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

define_libfunc_hierarchy! {
    pub enum TestingLibfunc {
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
         PopLog(PopLogLibfunc),
         Cheatcode(CheatcodeLibfunc),
    }, TestingConcreteLibfunc
}
