use super::felt252::Felt252Type;
use super::gas::GasBuiltinType;
use super::int::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::int::unsigned128::Uint128Type;
use super::nullable::NullableType;
use super::range_check::RangeCheckType;
use super::segment_arena::SegmentArenaType;
use super::squashed_felt252_dict::SquashedFelt252DictType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
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
pub struct Felt252DictTypeWrapped {}
impl GenericTypeArgGenericType for Felt252DictTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Felt252Dict");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { long_id: wrapped_long_id, storable, droppable, duplicatable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // List of specific types allowed as dictionary values.
        // TODO(Gil): Check in the higher level compiler and raise proper diagnostic (when we'll
        // have a 'where' equivalent).
        // TODO(Gil): Allow any type of size 1 which implement the 'Default' trait.
        let allowed_types = [
            Felt252Type::id(),
            Uint8Type::id(),
            Uint16Type::id(),
            Uint32Type::id(),
            Uint64Type::id(),
            Uint128Type::id(),
            NullableType::id(),
        ];
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
pub type Felt252DictType = GenericTypeArgGenericTypeWrapper<Felt252DictTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum Felt252DictLibfunc {
        New(Felt252DictNewLibfunc),
        Read(Felt252DictReadLibfunc),
        Write(Felt252DictWriteLibfunc),
        Squash(Felt252DictSquashLibfunc),
    }, Felt252DictConcreteLibfunc
}

/// Libfunc for creating a new felt252_dict.
#[derive(Default)]
pub struct Felt252DictNewLibfunc {}
impl SignatureOnlyGenericLibfunc for Felt252DictNewLibfunc {
    const STR_ID: &'static str = "felt252_dict_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let segment_arena_ty = context.get_concrete_type(SegmentArenaType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: segment_arena_ty.clone(),
                allow_deferred: false,
                allow_add_const: true,
                allow_const: false,
            }],
            vec![
                OutputVarInfo {
                    ty: segment_arena_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: context.get_wrapped_concrete_type(Felt252DictType::id(), ty)?,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for writing a new value to a felt252_dict.
#[derive(Default)]
pub struct Felt252DictWriteLibfunc {}
impl SignatureOnlyGenericLibfunc for Felt252DictWriteLibfunc {
    const STR_ID: &'static str = "felt252_dict_write";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let dict_ty = context.get_wrapped_concrete_type(Felt252DictType::id(), ty.clone())?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: dict_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(felt252_ty),
                ParamSignature::new(ty),
            ],
            vec![OutputVarInfo {
                ty: dict_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for reading a value corresponding to a key, from a felt252_dict.
#[derive(Default)]
pub struct Felt252DictReadLibfunc {}
impl SignatureOnlyGenericLibfunc for Felt252DictReadLibfunc {
    const STR_ID: &'static str = "felt252_dict_read";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(Felt252DictType::id(), generic_ty.clone())?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: dict_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(felt252_ty),
            ],
            vec![
                OutputVarInfo {
                    ty: dict_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: generic_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for performing a `squash` operation on a dict. Returns a pointer to the squashed dict.
#[derive(Default)]
pub struct Felt252DictSquashLibfunc {}
impl SignatureOnlyGenericLibfunc for Felt252DictSquashLibfunc {
    const STR_ID: &'static str = "felt252_dict_squash";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let generic_ty = args_as_single_type(args)?;
        let dict_ty =
            context.get_wrapped_concrete_type(Felt252DictType::id(), generic_ty.clone())?;
        let squashed_dict_ty =
            context.get_wrapped_concrete_type(SquashedFelt252DictType::id(), generic_ty)?;
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
