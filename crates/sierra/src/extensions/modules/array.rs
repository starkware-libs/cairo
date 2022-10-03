use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::types::{TypeInfo, TypeSpecializationContext};
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing an array.
#[derive(Default)]
pub struct ArrayType {}
impl NamedType for ArrayType {
    type Concrete = ArrayConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Array");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        let info = context.get_type_info_as_result(ty.clone())?;
        if info.storable {
            Ok(ArrayConcreteType { info: TypeInfo { duplicatable: false, ..info }, ty })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}

pub struct ArrayConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}
impl ConcreteType for ArrayConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum ArrayLibFunc {
        New(ArrayNewLibFunc),
        Push(ArrayPushLibFunc),
        // TODO(orizi): Add length after libfunc result unpacking is supported.
        // TODO(orizi): Add access after enums are supported.
    }, ArrayConcreteLibFunc
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct ArrayNewLibFunc {}
impl NamedLibFunc for ArrayNewLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("array_new");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(ArrayType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred,
            }],
            SierraApChange::NotImplemented,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// LibFunc for pushing a value into the end of an array.
#[derive(Default)]
pub struct ArrayPushLibFunc {}
impl NamedLibFunc for ArrayPushLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("array_push");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        Ok(LibFuncSignature::new_non_branch(
            vec![arr_ty.clone(), ty],
            vec![OutputVarInfo { ty: arr_ty, ref_info: OutputVarReferenceInfo::Deferred }],
            SierraApChange::NotImplemented,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}
