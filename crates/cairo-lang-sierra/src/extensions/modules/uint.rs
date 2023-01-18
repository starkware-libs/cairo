use std::marker::PhantomData;

use num_bigint::BigInt;

use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    OverflowingAdd,
    OverflowingSub,
}

/// Trait for implementing unsigned integers.
pub trait UintTraits: Default {
    /// The rust matching type to this type.
    type UintType: TryFrom<BigInt> + Into<BigInt> + Copy;
    /// The generic type id for this type.
    const GENERIC_TYPE_ID: GenericTypeId;
    /// The generic libfunc id for getting a const of this type.
    const CONST: &'static str;
    /// The generic libfunc id for comparing equality.
    const EQUAL: &'static str;
    /// The generic libfunc id for testing if less than.
    const LESS_THAN: &'static str;
    /// The generic libfunc id for testing if less than or equal.
    const LESS_THAN_OR_EQUAL: &'static str;
}

#[derive(Default)]
pub struct UintType<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericType for UintType<TUintTraits> {
    const ID: GenericTypeId = TUintTraits::GENERIC_TYPE_ID;
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// Libfunc for creating a constant unsigned integer.
#[derive(Default)]
pub struct UintConstLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NamedLibfunc for UintConstLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::CONST;
    type Concrete = UintConstConcreteLibfunc<TUintTraits>;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(Self::Concrete {
                c: TUintTraits::UintType::try_from(c.clone())
                    .map_err(|_| SpecializationError::UnsupportedGenericArg)?,
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct UintConstConcreteLibfunc<TUintTraits: UintTraits> {
    pub c: TUintTraits::UintType,
    pub signature: LibfuncSignature,
}
impl<TUintTraits: UintTraits> SignatureBasedConcreteLibfunc
    for UintConstConcreteLibfunc<TUintTraits>
{
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for comparing uints` equality.
#[derive(Default)]
pub struct UintEqualLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintEqualLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::EQUAL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let param_signatures = vec![
            ParamSignature {
                ty: ty.clone(),
                allow_deferred: false,
                allow_add_const: false,
                allow_const: true,
            },
            ParamSignature { ty, allow_deferred: false, allow_add_const: false, allow_const: true },
        ];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}

/// Libfunc for comparing uints.
#[derive(Default)]
pub struct UintLessThanLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintLessThanLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::LESS_THAN;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature {
                ty: range_check_type.clone(),
                allow_deferred: false,
                allow_add_const: true,
                allow_const: false,
            },
            ParamSignature::new(ty.clone()),
            ParamSignature::new(ty),
        ];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: range_check_type.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}

/// Libfunc for comparing u128s.
#[derive(Default)]
pub struct UintLessThanOrEqualLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc
    for UintLessThanOrEqualLibfunc<TUintTraits>
{
    const STR_ID: &'static str = TUintTraits::LESS_THAN_OR_EQUAL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature {
                ty: range_check_type.clone(),
                allow_deferred: false,
                allow_add_const: true,
                allow_const: false,
            },
            ParamSignature::new(ty.clone()),
            ParamSignature::new(ty),
        ];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: range_check_type.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}

#[derive(Default)]
pub struct Uint8Traits;

impl UintTraits for Uint8Traits {
    type UintType = u8;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u8");
    const CONST: &'static str = "u8_const";
    const EQUAL: &'static str = "u8_eq";
    const LESS_THAN: &'static str = "u8_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u8_le";
}

/// Type for u8.
pub type Uint8Type = UintType<Uint8Traits>;

define_libfunc_hierarchy! {
    pub enum Uint8Libfunc {
        Const(UintConstLibfunc<Uint8Traits>),
        LessThan(UintLessThanLibfunc<Uint8Traits>),
        Equal(UintEqualLibfunc<Uint8Traits>),
        LessThanOrEqual(UintLessThanOrEqualLibfunc<Uint8Traits>),
    }, Uint8Concrete
}
