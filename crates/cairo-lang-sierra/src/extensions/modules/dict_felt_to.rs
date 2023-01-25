use super::dict_manager::DictManagerType;
use super::felt::FeltType;
use super::gas::GasBuiltinType;
use super::range_check::RangeCheckType;
use super::squashed_dict_felt_to::SquashedDictFeltToType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
};
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Type representing a dictionary from a felt to types of size one.
#[derive(Default)]
pub struct DictFeltToTypeWrapped {}
impl GenericTypeArgGenericType for DictFeltToTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("DictFeltTo");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // TODO(Gil): the implementation support values of size 1. Remove when other sizes are
        // supported.
        if !wrapped_info.storable
            || !wrapped_info.droppable
            || !wrapped_info.duplicatable
            || wrapped_info.size != 1
        {
            Err(SpecializationError::UnsupportedGenericArg)
        } else {
            Ok(TypeInfo { long_id, duplicatable: false, droppable: false, storable: true, size: 1 })
        }
    }
}
pub type DictFeltToType = GenericTypeArgGenericTypeWrapper<DictFeltToTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum DictFeltToLibfunc {
        New(DictFeltToNewLibfunc),
        Read(DictFeltToReadLibfunc),
        Write(DictFeltToWriteLibfunc),
        Squash(DictFeltToSquashLibfunc),
    }, DictFeltToConcreteLibfunc
}

/// Libfunc for creating a new dict_felt_to.
#[derive(Default)]
pub struct DictFeltToNewLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFeltToNewLibfunc {
    const STR_ID: &'static str = "dict_felt_to_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let dict_manager_ty = context.get_concrete_type(DictManagerType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![dict_manager_ty.clone()],
            vec![
                OutputVarInfo {
                    ty: dict_manager_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: context.get_wrapped_concrete_type(DictFeltToType::id(), ty)?,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for writing a new value to a dict_felt_to.
#[derive(Default)]
pub struct DictFeltToWriteLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFeltToWriteLibfunc {
    const STR_ID: &'static str = "dict_felt_to_write";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let dict_ty = context.get_wrapped_concrete_type(DictFeltToType::id(), ty.clone())?;
        Ok(LibfuncSignature::new_non_branch(
            vec![dict_ty.clone(), felt_ty, ty],
            vec![OutputVarInfo {
                ty: dict_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for reading a value corresponding to a key, from a dict_felt_to.
#[derive(Default)]
pub struct DictFeltToReadLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFeltToReadLibfunc {
    const STR_ID: &'static str = "dict_felt_to_read";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(DictFeltToType::id(), generic_ty.clone())?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![dict_ty.clone(), felt_ty],
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

/// Libfunc for performing a `squash` opertaion on a dict. Returns a pointer to the squashed dict.
#[derive(Default)]
pub struct DictFeltToSquashLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFeltToSquashLibfunc {
    const STR_ID: &'static str = "dict_felt_to_squash";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(DictFeltToType::id(), generic_ty.clone())?;
        let squashed_dict_ty =
            context.get_wrapped_concrete_type(SquashedDictFeltToType::id(), generic_ty)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let dict_manager_ty = context.get_concrete_type(DictManagerType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![
                range_check_type.clone(),
                gas_builtin_type.clone(),
                dict_manager_ty.clone(),
                dict_ty,
            ],
            vec![
                OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                },
                OutputVarInfo {
                    ty: gas_builtin_type,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(1) },
                },
                OutputVarInfo {
                    ty: dict_manager_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(2) },
                },
                OutputVarInfo {
                    ty: squashed_dict_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(3) },
                },
            ],
            SierraApChange::Unknown,
        ))
    }
}
