use super::enm::EnumType;
use super::felt252::Felt252Type;
use super::gas::GasBuiltinType;
use super::int::unsigned::{Uint8Type, Uint16Type, Uint32Type, Uint64Type};
use super::int::unsigned128::Uint128Type;
use super::nullable::NullableType;
use super::range_check::RangeCheckType;
use super::segment_arena::SegmentArenaType;
use super::squashed_felt252_dict::SquashedFelt252DictType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureAndTypeGenericLibfunc, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    NamedType, OutputVarReferenceInfo, SpecializationError, args_as_single_type,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

/// Type representing a dictionary from a felt252 to types of size one.
///
/// This is currently only bounded for all numeric types, Nullable, and Enum types with 2 or less
/// variants, as this are the types that has proper default as 0, and therefore can be properly used
/// as a value in the dictionary.
#[derive(Default)]
pub struct Felt252DictTypeWrapped {}
impl GenericTypeArgGenericType for Felt252DictTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Felt252Dict");

    fn calc_info(
        &self,
        context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_type_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        specialize_with_dict_value_param(context, long_id, wrapped_type_info)
    }
}

/// Specializes a generic type matching a dictionary value parameter.
/// Called for both `Felt252Dict` and `Felt252DictEntry`.
fn specialize_with_dict_value_param(
    context: &dyn TypeSpecializationContext,
    long_id: ConcreteTypeLongId,
    TypeInfo {
        long_id: ConcreteTypeLongId { generic_id, generic_args },
        storable,
        droppable,
        duplicatable: _,
        zero_sized: _,
    }: TypeInfo,
) -> Result<TypeInfo, SpecializationError> {
    // Checking for specific types allowed as dictionary values.
    // TODO(Gil): Check in the higher level compiler and raise proper diagnostic (when we'll
    // have a 'where' equivalent).
    // TODO(Gil): Allow any type of size 1 which implement the 'Felt252DictValue' trait.
    let allowed = match generic_id {
        id if id == Felt252Type::id() => generic_args.is_empty(),
        id if id == Uint8Type::id() => generic_args.is_empty(),
        id if id == Uint16Type::id() => generic_args.is_empty(),
        id if id == Uint32Type::id() => generic_args.is_empty(),
        id if id == Uint64Type::id() => generic_args.is_empty(),
        id if id == Uint128Type::id() => generic_args.is_empty(),
        id if id == NullableType::id() => generic_args.len() == 1,
        id if id == EnumType::id() => {
            // Checking the enum type is valid.
            !generic_args.is_empty()
            // Zero is not a valid value for enums with 3 or more variants, so they cannot be
            // used in a dict (whose default value is zero).
            // (the additional argument is the user type).
            && generic_args.len() <= 3
                // All contained types must be 0-sized, so the total size will be exactly 1.
                && generic_args.into_iter().skip(1).all(|arg| {
                    let GenericArg::Type(ty) = arg else {
                        return false;
                    };
                    let Ok(info) = context.get_type_info(ty) else {
                        return false;
                    };
                    info.zero_sized
                })
        }
        _ => false,
    };
    if allowed && storable && droppable {
        Ok(TypeInfo {
            long_id,
            duplicatable: false,
            droppable: false,
            storable: true,
            zero_sized: false,
        })
    } else {
        Err(SpecializationError::UnsupportedGenericArg)
    }
}
pub type Felt252DictType = GenericTypeArgGenericTypeWrapper<Felt252DictTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum Felt252DictLibfunc {
        New(Felt252DictNewLibfunc),
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
            vec![ParamSignature::new(segment_arena_ty.clone()).with_allow_add_const()],
            vec![
                OutputVarInfo::new_builtin(segment_arena_ty, 0),
                OutputVarInfo {
                    ty: context.get_wrapped_concrete_type(Felt252DictType::id(), ty)?,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: false },
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
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                },
                OutputVarInfo {
                    ty: gas_builtin_type,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 1 },
                },
                OutputVarInfo {
                    ty: segment_arena_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 2 },
                },
                OutputVarInfo {
                    ty: squashed_dict_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 3 },
                },
            ],
            SierraApChange::Unknown,
        ))
    }
}

/// Type representing an entry access to a felt252_dict.
#[derive(Default)]
pub struct Felt252DictEntryTypeWrapped {}
impl GenericTypeArgGenericType for Felt252DictEntryTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Felt252DictEntry");

    fn calc_info(
        &self,
        context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_type_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        specialize_with_dict_value_param(context, long_id, wrapped_type_info)
    }
}
pub type Felt252DictEntryType = GenericTypeArgGenericTypeWrapper<Felt252DictEntryTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum Felt252DictEntryLibfunc {
        Get(Felt252DictEntryGetLibfunc),
        Finalize(Felt252DictEntryFinalizeLibfunc),
    }, Felt252DictEntryConcreteLibfunc
}

/// Libfunc for creating a new felt252_dict_entry, it owns the dictionary until finalized.
#[derive(Default)]
pub struct Felt252DictEntryGetLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for Felt252DictEntryGetLibfuncWrapped {
    const STR_ID: &'static str = "felt252_dict_entry_get";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let dict_ty = context.get_wrapped_concrete_type(Felt252DictType::id(), ty.clone())?;
        let dict_entry_ty =
            context.get_wrapped_concrete_type(Felt252DictEntryType::id(), ty.clone())?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(dict_ty).with_allow_add_const(),
                // Key.
                ParamSignature::new(felt252_ty),
            ],
            vec![
                OutputVarInfo {
                    ty: dict_entry_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                // Current value.
                OutputVarInfo {
                    ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type Felt252DictEntryGetLibfunc =
    WrapSignatureAndTypeGenericLibfunc<Felt252DictEntryGetLibfuncWrapped>;

/// Libfunc for finalizing a felt252_dict_entry, returns the owned dict.
#[derive(Default)]
pub struct Felt252DictEntryFinalizeLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for Felt252DictEntryFinalizeLibfuncWrapped {
    const STR_ID: &'static str = "felt252_dict_entry_finalize";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let dict_ty = context.get_wrapped_concrete_type(Felt252DictType::id(), ty.clone())?;
        let dict_entry_ty =
            context.get_wrapped_concrete_type(Felt252DictEntryType::id(), ty.clone())?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(dict_entry_ty).with_allow_add_const(),
                // New value.
                ParamSignature::new(ty),
            ],
            vec![OutputVarInfo {
                ty: dict_ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

pub type Felt252DictEntryFinalizeLibfunc =
    WrapSignatureAndTypeGenericLibfunc<Felt252DictEntryFinalizeLibfuncWrapped>;
