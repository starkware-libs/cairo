use num_bigint::BigInt;
use num_traits::Signed;

use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    args_as_single_value, NamedLibfunc, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Trait for implementing a library function that returns a const of a given type.
pub trait ConstGenLibfunc: Default {
    /// The library function id.
    const STR_ID: &'static str;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
    /// The bound on the value of the type.
    fn bound() -> BigInt;
}

/// Wrapper to prevent implementation collisions for `NamedLibfunc`.
#[derive(Default)]
pub struct WrapConstGenLibfunc<T: ConstGenLibfunc>(T);

impl<T: ConstGenLibfunc> NamedLibfunc for WrapConstGenLibfunc<T> {
    const STR_ID: &'static str = <T as ConstGenLibfunc>::STR_ID;
    type Concrete = SignatureAndConstConcreteLibfunc;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(<T as ConstGenLibfunc>::GENERIC_TYPE_ID, &[])?,
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
        let c = args_as_single_value(args)?;
        if c.is_negative() || c > (<T as ConstGenLibfunc>::bound()) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        Ok(SignatureAndConstConcreteLibfunc {
            c: c.clone(),
            signature: <Self as NamedLibfunc>::specialize_signature(self, context.upcast(), args)?,
        })
    }
}

/// Struct providing a ConcreteLibfunc signature and a const.
pub struct SignatureAndConstConcreteLibfunc {
    pub c: BigInt,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for SignatureAndConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
