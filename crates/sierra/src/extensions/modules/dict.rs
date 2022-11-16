use super::as_single_type;
use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyConcreteLibFunc, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing a dict.
#[derive(Default)]
pub struct DictType {}
impl NamedType for DictType {
    type Concrete = DictConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Dict");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        let info = context.get_type_info(ty.clone())?;
        if info.storable {
            Ok(DictConcreteType {
                info: TypeInfo {
                    long_id: Self::concrete_type_long_id(args),
                    duplicatable: false,
                    droppable: info.droppable,
                    storable: true,
                    size: 2,
                },
                ty,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}

pub struct DictConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}

impl ConcreteType for DictConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum DictLibFunc {
        New(DictNewLibFunc),
        Read(DictReadLibFunc),
        Write(DictWriteLibFunc),
        // TODO(Gil): Add DictSquash,
    }, DictConcreteLibFunc
}

/// LibFunc for creating a new dict.
#[derive(Default)]
pub struct DictNewLibFunc {}
impl NamedLibFunc for DictNewLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_new");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![felt_ty],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(DictType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known(1),
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

/// LibFunc for writing a new value to the dict.
#[derive(Default)]
pub struct DictWriteLibFunc {}
impl NamedLibFunc for DictWriteLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_write");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let dict_ty = context.get_wrapped_concrete_type(DictType::id(), ty.clone())?;
        Ok(LibFuncSignature::new_non_branch(
            vec![dict_ty.clone(), felt_ty, ty],
            vec![OutputVarInfo {
                ty: dict_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known(1),
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

/// LibFunc for reading a value corresponding to a key, from the dict.
#[derive(Default)]
pub struct DictReadLibFunc {}
impl NamedLibFunc for DictReadLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_read");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let generic_ty = as_single_type(args)?;
        let dict_ty = context.get_wrapped_concrete_type(DictType::id(), generic_ty.clone())?;
        Ok(LibFuncSignature::new_non_branch(
            vec![dict_ty.clone(), generic_ty.clone()],
            vec![
                OutputVarInfo {
                    ty: dict_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: generic_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known(1),
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
