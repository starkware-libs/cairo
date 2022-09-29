use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type wrapping a value as reference.
#[derive(Default)]
pub struct RefType {}
impl NamedType for RefType {
    type Concrete = RefConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Ref");
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RefConcreteType { ty: as_single_type(args)? })
    }
}
pub struct RefConcreteType {
    pub ty: ConcreteTypeId,
}
impl ConcreteType for RefConcreteType {}

define_libfunc_hierarchy! {
    pub enum RefLibFunc {
        Take(IntoRefLibFunc),
        Deref(DerefLibFunc),
    }, RefConcreteLibFunc
}

/// LibFunc for wrapping an object of type T into a reference.
#[derive(Default)]
pub struct IntoRefLibFunc {}
impl NamedLibFunc for IntoRefLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("into_ref");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(RefType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred,
            }],
            SierraApChange::NotImplemented,
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(&context, args)? })
    }
}

/// LibFunc for dereferencing a Ref<T> back into a T.
#[derive(Default)]
pub struct DerefLibFunc {}
impl NamedLibFunc for DerefLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("deref");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_wrapped_concrete_type(RefType::id(), ty.clone())?],
            vec![OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::Deferred }],
            SierraApChange::NotImplemented,
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(&context, args)? })
    }
}
