use super::array::ArrayType;
use super::felt252::Felt252Type;
use super::structure::StructType;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, SignatureAndTypeGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId, UserTypeId};
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum SquashedFelt252DictLibfunc {
        IntoEntries(SquashedDictIntoEntriesLibfunc),
    }, SquashedFelt252DictConcreteLibfunc
}
/// Type representing a static squashed dictionary from a felt252 to any type of size one.
#[derive(Default)]
pub struct SquashedFelt252DictTypeWrapped {}
impl GenericTypeArgGenericType for SquashedFelt252DictTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("SquashedFelt252Dict");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { zero_sized, storable, droppable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // Note: SquashedFelt252Dict is defined as non-duplicatable even if the inner type is
        // duplicatable to allow libfunc that adds entries to it (treat it similarly to an array).
        // TODO(Gil): the implementation support values of size 1. Remove when other sizes are
        // supported.
        if storable && !zero_sized {
            Ok(TypeInfo {
                long_id,
                storable: true,
                droppable,
                duplicatable: false,
                zero_sized: false,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SquashedFelt252DictType = GenericTypeArgGenericTypeWrapper<SquashedFelt252DictTypeWrapped>;

#[derive(Default)]
pub struct SquashedDictAsEntriesLibfuncWrapped;
impl SignatureAndTypeGenericLibfunc for SquashedDictAsEntriesLibfuncWrapped {
    const STR_ID: &'static str = "squashed_felt252_dict_entries";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let squashed_dict_ty =
            context.get_wrapped_concrete_type(SquashedFelt252DictType::id(), ty.clone())?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let tuple_ty = context.get_concrete_type(
            StructType::id(),
            &[
                GenericArg::UserType(UserTypeId::from_string("Tuple")),
                GenericArg::Type(felt252_ty.clone()),
                GenericArg::Type(ty.clone()),
                GenericArg::Type(ty.clone()),
            ],
        )?;
        let array_ty = context.get_wrapped_concrete_type(ArrayType::id(), tuple_ty)?;
        Ok(reinterpret_cast_signature(squashed_dict_ty, array_ty))
    }
}
pub type SquashedDictIntoEntriesLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SquashedDictAsEntriesLibfuncWrapped>;
