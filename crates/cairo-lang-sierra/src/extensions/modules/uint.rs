use std::marker::PhantomData;

use num_bigint::BigInt;

use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError,
};
use crate::ids::{GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    OverflowingAdd,
    OverflowingSub,
    OverflowingMul,
    DivMod,
    WideMul,
}

/// Trait for implementing unsigned integers.
pub trait UintTraits: Default {
    /// The rust matching type to this type.
    type UintType: TryFrom<BigInt> + Into<BigInt> + Copy;
    /// The generic type id for this type.
    const GENERIC_TYPE_ID: GenericTypeId;
    /// The generic libfunc id for getting a const of this type.
    const CONST: GenericLibfuncId;
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
    const ID: GenericLibfuncId = TUintTraits::CONST;
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

#[derive(Default)]
pub struct Uint8Traits;

impl UintTraits for Uint8Traits {
    type UintType = u8;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u8");
    const CONST: GenericLibfuncId = GenericLibfuncId::new_inline("u8_const");
}

/// Type for u8.
pub type Uint8Type = UintType<Uint8Traits>;

define_libfunc_hierarchy! {
    pub enum Uint8Libfunc {
        Const(UintConstLibfunc<Uint8Traits>),
    }, Uint8Concrete
}
