use super::felt::FeltType;
use super::range_check::RangeCheckType;
use super::squashed_dict_felt_to::SquashedDictFeltToType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing a dictionary from a felt to any type of size one.
#[derive(Default)]
pub struct DictFeltToType {}
impl NamedType for DictFeltToType {
    type Concrete = DictFeltToConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("DictFeltTo");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let info = context.get_type_info(ty.clone())?;
        // TODO(Gil): the implementation support values of size 1. Remove when other sizes are
        // supported.
        if info.storable && info.size == 1 {
            Ok(DictFeltToConcreteType {
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

pub struct DictFeltToConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}

impl ConcreteType for DictFeltToConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum DictFeltToLibFunc {
        New(DictFeltToNewLibFunc),
        Read(DictFeltToReadLibFunc),
        Write(DictFeltToWriteLibFunc),
        Squash(DictFeltToSquashLibFunc),
    }, DictFeltToConcreteLibFunc
}

/// LibFunc for creating a new dict_felt_to.
#[derive(Default)]
pub struct DictFeltToNewLibFunc {}
impl SignatureOnlyGenericLibFunc for DictFeltToNewLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_felt_to_new");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![felt_ty],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(DictFeltToType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// LibFunc for writing a new value to a dict_felt_to.
#[derive(Default)]
pub struct DictFeltToWriteLibFunc {}
impl SignatureOnlyGenericLibFunc for DictFeltToWriteLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_felt_to_write");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let dict_ty = context.get_wrapped_concrete_type(DictFeltToType::id(), ty.clone())?;
        Ok(LibFuncSignature::new_non_branch(
            vec![dict_ty.clone(), felt_ty, ty],
            vec![OutputVarInfo {
                ty: dict_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// LibFunc for reading a value corresponding to a key, from a dict_felt_to.
#[derive(Default)]
pub struct DictFeltToReadLibFunc {}
impl SignatureOnlyGenericLibFunc for DictFeltToReadLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_felt_to_read");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(DictFeltToType::id(), generic_ty.clone())?;
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
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// LibFunc for performing a `squash` opertaion on a dict. Returns a pointer to the squashed dict.
#[derive(Default)]
pub struct DictFeltToSquashLibFunc {}
impl SignatureOnlyGenericLibFunc for DictFeltToSquashLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dict_felt_to_squash");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(DictFeltToType::id(), generic_ty.clone())?;
        let squashed_dict_ty =
            context.get_wrapped_concrete_type(SquashedDictFeltToType::id(), generic_ty)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![range_check_type.clone(), dict_ty],
            vec![
                OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: squashed_dict_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Unknown,
        ))
    }
}
