use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;
use num_traits::ToPrimitive;

use super::boxing::box_ty;
use super::enm::EnumType;
use super::structure::StructType;
use super::utils::Range;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

/// Type representing a constant value, hardcoded in the program segment.
#[derive(Default)]
pub struct ConstType {}
impl NamedType for ConstType {
    type Concrete = ConstConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Const");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let mut args_iter = args.iter();
        let first_arg = args_iter.next().ok_or(SpecializationError::WrongNumberOfGenericArgs)?;
        let inner_ty = try_extract_matches!(first_arg, GenericArg::Type)
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        // Extract the rest of the arguments as the inner data.
        let inner_data = args_iter.cloned().collect::<Vec<_>>();
        validate_const_data(context, inner_ty, &inner_data)?;
        let info = TypeInfo {
            long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
            storable: false,
            duplicatable: false,
            droppable: false,
            zero_sized: false,
        };
        Ok(ConstConcreteType { info, inner_ty: inner_ty.clone(), inner_data })
    }
}

pub struct ConstConcreteType {
    pub info: TypeInfo,
    pub inner_ty: ConcreteTypeId,
    /// Should be one of the following:
    /// - A single value, if the inner type is a simple numeric type (e.g., `felt252`, `u32`,
    ///   etc.).
    /// - A list of const types, if the inner type is a struct. The type of each const type must be
    ///   the same as the corresponding struct member type.
    /// - A selector (a single value) followed by a const type, if the inner type is an enum. The
    ///   type of the const type must be the same as the corresponding enum variant type.
    pub inner_data: Vec<GenericArg>,
}

/// Validates that the inner data is valid for the inner type.
fn validate_const_data(
    context: &dyn TypeSpecializationContext,
    inner_ty: &ConcreteTypeId,
    inner_data: &[GenericArg],
) -> Result<(), SpecializationError> {
    let inner_type_info = context.get_type_info(inner_ty.clone())?;
    if inner_type_info.long_id.generic_id == StructType::ID {
        validate_const_struct_data(context, &inner_type_info, inner_data)?;
    } else if inner_type_info.long_id.generic_id == EnumType::ID {
        validate_const_enum_data(context, &inner_type_info, inner_data)?;
    } else {
        let type_range = Range::from_type_info(&inner_type_info)?;
        let [GenericArg::Value(value)] = inner_data else {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        };
        if !(&type_range.lower <= value && value < &type_range.upper) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    }
    Ok(())
}

/// Given a const type representing a struct, validates that the inner data types are compatible
/// with the struct types.
fn validate_const_struct_data(
    context: &dyn TypeSpecializationContext,
    inner_type_info: &TypeInfo,
    inner_data: &[GenericArg],
) -> Result<(), SpecializationError> {
    let mut struct_args_iter = inner_type_info.long_id.generic_args.iter();
    // The first arg of a struct is the struct user type, so skip it.
    struct_args_iter.next();
    if struct_args_iter.len() != inner_data.len() {
        return Err(SpecializationError::WrongNumberOfGenericArgs);
    }
    for (struct_arg, const_arg) in struct_args_iter.zip_eq(inner_data.iter()) {
        // Both the struct_arg and the const_arg must be types.
        let (GenericArg::Type(struct_arg_ty), GenericArg::Type(const_arg_ty)) =
            (struct_arg, const_arg)
        else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        // Validate that the const_arg_ty is a `Const` representing the same type as struct_arg_ty.
        let const_arg_type_info = context.get_type_info(const_arg_ty.clone())?;
        if const_arg_type_info.long_id.generic_id != ConstType::ID {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let Some(GenericArg::Type(const_arg_inner_ty)) =
            const_arg_type_info.long_id.generic_args.first()
        else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };
        // Validate that the const_arg_inner_ty is the same as the corresponding struct_arg_ty.
        if struct_arg_ty != const_arg_inner_ty {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    }
    Ok(())
}

/// Given a const type representing an enum, validates that the inner data is a selector followed by
/// a const type of the corresponding variant.
fn validate_const_enum_data(
    context: &dyn TypeSpecializationContext,
    inner_type_info: &TypeInfo,
    inner_data: &[GenericArg],
) -> Result<(), SpecializationError> {
    // Assert that the inner data is the variant selector (a single value), followed by a const type
    // representing the variant data.
    let [GenericArg::Value(selector), GenericArg::Type(variant_const_type_id)] = inner_data else {
        return Err(SpecializationError::UnsupportedGenericArg);
    };

    let selector: usize =
        selector.try_into().map_err(|_| SpecializationError::UnsupportedGenericArg)?;
    // Extract the variant data type according to the selector.
    let Some(GenericArg::Type(variant_data_ty)) =
        inner_type_info.long_id.generic_args.get(1 + selector)
    else {
        return Err(SpecializationError::UnsupportedGenericArg);
    };

    // Validate that `variant_const_type_id` is a const type.
    let variant_const_info = context.get_type_info(variant_const_type_id.clone())?;
    if variant_const_info.long_id.generic_id != ConstType::ID {
        return Err(SpecializationError::UnsupportedGenericArg);
    }

    // Validate that the type of the const is the same as the corresponding variant data
    // type.
    if variant_const_info.long_id.generic_args.first()
        != Some(&GenericArg::Type(variant_data_ty.clone()))
    {
        return Err(SpecializationError::UnsupportedGenericArg);
    }

    Ok(())
}

impl ConcreteType for ConstConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// A zero-input function that returns a box of the const value according to the generic arg type
/// of the function.
pub struct ConstAsBoxConcreteLibfunc {
    /// The type of the actual saved constant.
    pub const_type: ConcreteTypeId,
    /// The segment to have the constant in.
    pub segment_id: u32,
    pub signature: LibfuncSignature,
}

impl SignatureBasedConcreteLibfunc for ConstAsBoxConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

define_libfunc_hierarchy! {
    pub enum ConstLibfunc {
        AsBox(ConstAsBoxLibfuncWrapped),
    }, ConstConcreteLibfunc
}
#[derive(Default)]
pub struct ConstAsBoxLibfuncWrapped {}
impl ConstAsBoxLibfuncWrapped {
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<ConstAsBoxConcreteLibfunc, SpecializationError> {
        let (ty, segment_id) = match args {
            [GenericArg::Type(ty), GenericArg::Value(value)] => Ok((ty, value)),
            [_, _] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }?;
        let segment_id = segment_id.to_u32().ok_or(SpecializationError::UnsupportedGenericArg)?;
        let type_info = context.get_type_info(ty.clone())?;
        if type_info.long_id.generic_id != ConstType::ID {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let generic_args = context.get_type_info(ty.clone())?.long_id.generic_args;
        let Some(GenericArg::Type(inner_ty)) = generic_args.first() else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };
        let boxed_inner_ty = box_ty(context, inner_ty.clone())?;

        Ok(ConstAsBoxConcreteLibfunc {
            const_type: ty.clone(),
            segment_id,
            signature: LibfuncSignature::new_non_branch(
                vec![],
                vec![OutputVarInfo {
                    ty: boxed_inner_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                }],
                SierraApChange::Known { new_vars_only: false },
            ),
        })
    }
}

impl NamedLibfunc for ConstAsBoxLibfuncWrapped {
    type Concrete = ConstAsBoxConcreteLibfunc;
    const STR_ID: &'static str = "const_as_box";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(self.specialize_concrete_lib_func(context, args)?.signature)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        self.specialize_concrete_lib_func(context.upcast(), args)
    }
}
