use num_bigint::BigInt;

use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibFunc, OutputVarReferenceInfo, SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Trait for implementing a library function that returns a const of a given type.
pub trait ConstGenLibFunc: Default {
    /// The library function id.
    const ID: GenericLibFuncId;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
}

/// Wrapper to prevent implementation collisions for `NamedLibFunc`.
#[derive(Default)]
pub struct WrapConstGenLibFunc<T: ConstGenLibFunc>(T);

impl<T: ConstGenLibFunc> NamedLibFunc for WrapConstGenLibFunc<T> {
    const ID: GenericLibFuncId = <T as ConstGenLibFunc>::ID;
    type Concrete = SignatureAndConstConcreteLibFunc;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(<T as ConstGenLibFunc>::GENERIC_TYPE_ID, &[])?,
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
            [GenericArg::Value(c)] => Ok(SignatureAndConstConcreteLibFunc {
                c: c.clone(),
                signature: <Self as NamedLibFunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

/// Struct providing a ConcreteLibFunc signature and a const.
pub struct SignatureAndConstConcreteLibFunc {
    pub c: BigInt,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for SignatureAndConstConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
