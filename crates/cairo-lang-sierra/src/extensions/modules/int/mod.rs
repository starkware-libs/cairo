use std::marker::PhantomData;

use num_bigint::BigInt;

use super::felt252::Felt252Type;
use super::try_from_felt252::{TryFromFelt252, TryFromFelt252Libfunc};
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

pub mod signed;
pub mod signed128;
pub mod unsigned;
pub mod unsigned128;
pub mod unsigned256;
pub mod unsigned512;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    OverflowingAdd,
    OverflowingSub,
}

pub struct IntOperationConcreteLibfunc {
    pub operator: IntOperator,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for IntOperationConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Trait for implementing integers.
pub trait IntTraits: Default {
    /// The rust matching type to this type.
    type IntType: TryFrom<BigInt> + Into<BigInt> + Copy;
    /// Is the type smaller than 128 bits.
    /// Relevant since some implementations are different due to range check being 128 bits based.
    const IS_SMALL: bool;
    /// The generic type id for this type.
    const GENERIC_TYPE_ID: GenericTypeId;
    /// The generic libfunc id for getting a const of this type.
    const CONST: &'static str;
    /// The generic libfunc id for comparing equality.
    const EQUAL: &'static str;
    /// The generic libfunc id for conversion to felt252.
    const TO_FELT252: &'static str;
    /// The generic libfunc id for conversion from felt252.
    const TRY_FROM_FELT252: &'static str;
}

/// Trait for implementing multiplication for integers.
pub trait IntMulTraits: IntTraits {
    /// The generic libfunc id that multiplies two integers.
    const WIDE_MUL: &'static str;
    /// The generic type id for this type multiplication result.
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId;
}

#[derive(Default)]
pub struct IntType<TIntTraits: IntTraits> {
    _phantom: PhantomData<TIntTraits>,
}
impl<TIntTraits: IntTraits> NoGenericArgsGenericType for IntType<TIntTraits> {
    const ID: GenericTypeId = TIntTraits::GENERIC_TYPE_ID;
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// Libfunc for creating a constant integer.
#[derive(Default)]
pub struct IntConstLibfunc<TIntTraits: IntTraits> {
    _phantom: PhantomData<TIntTraits>,
}
impl<TIntTraits: IntTraits> NamedLibfunc for IntConstLibfunc<TIntTraits> {
    const STR_ID: &'static str = TIntTraits::CONST;
    type Concrete = IntConstConcreteLibfunc<TIntTraits>;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(TIntTraits::GENERIC_TYPE_ID, &[])?,
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
                c: TIntTraits::IntType::try_from(c.clone())
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

pub struct IntConstConcreteLibfunc<TIntTraits: IntTraits> {
    pub c: TIntTraits::IntType,
    pub signature: LibfuncSignature,
}
impl<TIntTraits: IntTraits> SignatureBasedConcreteLibfunc for IntConstConcreteLibfunc<TIntTraits> {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for comparing integers` equality.
#[derive(Default)]
pub struct IntEqualLibfunc<TIntTraits: IntTraits> {
    _phantom: PhantomData<TIntTraits>,
}
impl<TIntTraits: IntTraits> NoGenericArgsGenericLibfunc for IntEqualLibfunc<TIntTraits> {
    const STR_ID: &'static str = TIntTraits::EQUAL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TIntTraits::GENERIC_TYPE_ID, &[])?;
        let param_signatures =
            vec![ParamSignature::new(ty.clone()), ParamSignature::new(ty).with_allow_const()];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}

/// Libfunc for converting an integer into a felt252.
#[derive(Default)]
pub struct IntToFelt252Libfunc<TIntTraits: IntTraits> {
    _phantom: PhantomData<TIntTraits>,
}
impl<TIntTraits: IntTraits> NoGenericArgsGenericLibfunc for IntToFelt252Libfunc<TIntTraits> {
    const STR_ID: &'static str = TIntTraits::TO_FELT252;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: context.get_concrete_type(TIntTraits::GENERIC_TYPE_ID, &[])?,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Felt252Type::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for attempting to convert a felt252 into an integer.
#[derive(Default)]
pub struct IntFromFelt252Trait<TIntTraits: IntTraits> {
    _phantom: PhantomData<TIntTraits>,
}
impl<TIntTraits: IntTraits> TryFromFelt252 for IntFromFelt252Trait<TIntTraits> {
    const STR_ID: &'static str = TIntTraits::TRY_FROM_FELT252;
    const GENERIC_TYPE_ID: GenericTypeId = TIntTraits::GENERIC_TYPE_ID;
}

pub type IntFromFelt252Libfunc<T> = TryFromFelt252Libfunc<IntFromFelt252Trait<T>>;
/// Libfunc for integer wide multiplication.
#[derive(Default)]
pub struct IntWideMulLibfunc<TIntMulTraits: IntMulTraits> {
    _phantom: PhantomData<TIntMulTraits>,
}
impl<TIntMulTraits: IntMulTraits> NoGenericArgsGenericLibfunc for IntWideMulLibfunc<TIntMulTraits> {
    const STR_ID: &'static str = TIntMulTraits::WIDE_MUL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TIntMulTraits::GENERIC_TYPE_ID, &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(ty.clone()), ParamSignature::new(ty).with_allow_const()],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(TIntMulTraits::WIDE_MUL_RES_TYPE_ID, &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
