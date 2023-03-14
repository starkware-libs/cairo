use super::felt252::Felt252Type;
use super::gas::GasBuiltinType;
use super::nullable::NullableType;
use super::range_check::RangeCheckType;
use super::segment_arena::SegmentArenaType;
use super::squashed_dict_felt252_to::SquashedDictFelt252ToType;
use super::uint::Uint8Type;
use super::uint128::Uint128Type;
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

/// Type representing a dictionary from a felt252 to types of size one.
#[derive(Default)]
pub struct DictFelt252ToTypeWrapped {}
impl GenericTypeArgGenericType for DictFelt252ToTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("DictFelt252To");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { long_id: wrapped_long_id, storable, droppable, duplicatable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // List of specific types allowed as dictionary values.
        // TODO(Gil): Check in the higher level compiler and raise proper diagnostic (when we'll
        // have a 'where' equivalent).
        // TODO(Gil): Allow any type of size 1 which implement the 'Default' trait.
        let allowed_types =
            [Felt252Type::id(), Uint128Type::id(), Uint8Type::id(), NullableType::id()];
        if allowed_types.contains(&wrapped_long_id.generic_id)
            && storable
            && droppable
            && duplicatable
        {
            Ok(TypeInfo { long_id, duplicatable: false, droppable: false, storable: true, size: 1 })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type DictFelt252ToType = GenericTypeArgGenericTypeWrapper<DictFelt252ToTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum DictFelt252ToLibfunc {
        New(DictFelt252ToNewLibfunc),
        Read(DictFelt252ToReadLibfunc),
        Write(DictFelt252ToWriteLibfunc),
        Squash(DictFelt252ToSquashLibfunc),
    }, DictFelt252ToConcreteLibfunc
}

/// Libfunc for creating a new dict_felt252_to.
#[derive(Default)]
pub struct DictFelt252ToNewLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFelt252ToNewLibfunc {
    const STR_ID: &'static str = "dict_felt252_to_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let segment_arena_ty = context.get_concrete_type(SegmentArenaType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![segment_arena_ty.clone()],
            vec![
                OutputVarInfo {
                    ty: segment_arena_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
                OutputVarInfo {
                    ty: context.get_wrapped_concrete_type(DictFelt252ToType::id(), ty)?,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for writing a new value to a dict_felt252_to.
#[derive(Default)]
pub struct DictFelt252ToWriteLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFelt252ToWriteLibfunc {
    const STR_ID: &'static str = "dict_felt252_to_write";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let dict_ty = context.get_wrapped_concrete_type(DictFelt252ToType::id(), ty.clone())?;
        Ok(LibfuncSignature::new_non_branch(
            vec![dict_ty.clone(), felt252_ty, ty],
            vec![OutputVarInfo {
                ty: dict_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for reading a value corresponding to a key, from a dict_felt252_to.
#[derive(Default)]
pub struct DictFelt252ToReadLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFelt252ToReadLibfunc {
    const STR_ID: &'static str = "dict_felt252_to_read";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(DictFelt252ToType::id(), generic_ty.clone())?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![dict_ty.clone(), felt252_ty],
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
pub struct DictFelt252ToSquashLibfunc {}
impl SignatureOnlyGenericLibfunc for DictFelt252ToSquashLibfunc {
    const STR_ID: &'static str = "dict_felt252_to_squash";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(DictFelt252ToType::id(), generic_ty.clone())?;
        let squashed_dict_ty =
            context.get_wrapped_concrete_type(SquashedDictFelt252ToType::id(), generic_ty)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let segment_arena_ty = context.get_concrete_type(SegmentArenaType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![
                range_check_type.clone(),
                gas_builtin_type.clone(),
                segment_arena_ty.clone(),
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
                    ty: segment_arena_ty,
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
