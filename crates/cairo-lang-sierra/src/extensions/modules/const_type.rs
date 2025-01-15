use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;
use num_traits::{ToPrimitive, Zero};

use super::boxing::box_ty;
use super::consts::ConstGenLibfunc;
use super::enm::EnumType;
use super::int::unsigned128::Uint128Type;
use super::non_zero::NonZeroType;
use super::starknet::interoperability::{
    ClassHashConstLibfuncWrapped, ClassHashType, ContractAddressConstLibfuncWrapped,
    ContractAddressType,
};
use super::structure::StructType;
use super::utils::Range;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_single_type, extract_type_generic_args,
};
use crate::ids::{ConcreteTypeId, GenericTypeId, UserTypeId};
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
        let (inner_ty, inner_data) = inner_type_and_data(args)?;
        validate_const_data(context, inner_ty, inner_data)?;
        let info = TypeInfo {
            long_id: ConcreteTypeLongId { generic_id: Self::ID, generic_args: args.to_vec() },
            storable: false,
            duplicatable: false,
            droppable: false,
            zero_sized: false,
        };
        Ok(ConstConcreteType { info, inner_ty: inner_ty.clone(), inner_data: inner_data.to_vec() })
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
    let inner_generic_id = &inner_type_info.long_id.generic_id;
    if *inner_generic_id == StructType::ID {
        return validate_const_struct_data(context, &inner_type_info, inner_data);
    } else if *inner_generic_id == EnumType::ID {
        return validate_const_enum_data(context, &inner_type_info, inner_data);
    } else if *inner_generic_id == NonZeroType::ID {
        return validate_const_nz_data(context, &inner_type_info, inner_data);
    }
    let type_range = if *inner_generic_id == ContractAddressType::id() {
        Range::half_open(0, ContractAddressConstLibfuncWrapped::bound())
    } else if *inner_generic_id == ClassHashType::id() {
        Range::half_open(0, ClassHashConstLibfuncWrapped::bound())
    } else {
        Range::from_type_info(&inner_type_info)?
    };
    let [GenericArg::Value(value)] = inner_data else {
        return Err(SpecializationError::WrongNumberOfGenericArgs);
    };
    if !(&type_range.lower <= value && value < &type_range.upper) {
        return Err(SpecializationError::UnsupportedGenericArg);
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
        if extract_const_info(context, const_arg_ty)?.0 != *struct_arg_ty {
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

    // Validate that the type of the const is the same as the corresponding variant data
    // type.
    if extract_const_info(context, variant_const_type_id)?.0 != *variant_data_ty {
        return Err(SpecializationError::UnsupportedGenericArg);
    }
    Ok(())
}

/// Given a const type representing a NonZero, validates that the inner data is a const type
/// matching the inner type, where the given value isn't fully 0.
/// Example usages:
/// `Const<NonZero<u32>, Const<u32, 1>>`
/// `Const<NonZero<u256>, Const<u256, ...>>`
fn validate_const_nz_data(
    context: &dyn TypeSpecializationContext,
    inner_type_info: &TypeInfo,
    inner_data: &[GenericArg],
) -> Result<(), SpecializationError> {
    let wrapped_ty_from_nz = args_as_single_type(&inner_type_info.long_id.generic_args)?;
    let inner_const_type_id = args_as_single_type(inner_data)?;
    let (wrapped_ty_from_const, inner_const_data) =
        extract_const_info(context, &inner_const_type_id)?;
    if wrapped_ty_from_const != wrapped_ty_from_nz {
        return Err(SpecializationError::UnsupportedGenericArg);
    }

    // Check the direct value case.
    if matches!(inner_const_data.as_slice(), [GenericArg::Value(value)] if !value.is_zero()) {
        // Validate that the inner type is a valid integer type, by calling `from_type_info`.
        Range::from_type_info(&context.get_type_info(wrapped_ty_from_nz)?)?;
        return Ok(());
    }
    let struct_generic_args =
        extract_type_generic_args::<StructType>(context, &wrapped_ty_from_nz)?;
    if !matches!(
        struct_generic_args.first(),
        Some(GenericArg::UserType(ut))
        if (inner_const_data.len() == 2 && *ut == UserTypeId::from_string("core::integer::u256"))
        || (inner_const_data.len() == 4 && *ut == UserTypeId::from_string("core::integer::u512"))
    ) {
        return Err(SpecializationError::UnsupportedGenericArg);
    }
    // Validating one of the inner values for the types in non-zero, as well that all the inner
    // types are consts of `u128`.
    let mut is_non_zero = false;
    for const_u128_arg in inner_const_data {
        let const_u128_ty = try_extract_matches!(const_u128_arg, GenericArg::Type)
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let (u128_ty, const_data) = extract_const_info(context, &const_u128_ty)?;
        extract_type_generic_args::<Uint128Type>(context, &u128_ty)?;
        let [GenericArg::Value(const_data)] = const_data.as_slice() else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        is_non_zero |= !const_data.is_zero();
    }

    if is_non_zero { Ok(()) } else { Err(SpecializationError::UnsupportedGenericArg) }
}

/// Validates the type is a `Const` type, and extracts the inner type and the rest of the generic
/// args of `const_ty`.
fn extract_const_info(
    context: &dyn TypeSpecializationContext,
    const_ty: &ConcreteTypeId,
) -> Result<(ConcreteTypeId, <Vec<GenericArg> as IntoIterator>::IntoIter), SpecializationError> {
    let mut generic_args = extract_type_generic_args::<ConstType>(context, const_ty)?.into_iter();
    if let Some(GenericArg::Type(inner_ty)) = generic_args.next() {
        Ok((inner_ty, generic_args))
    } else {
        Err(SpecializationError::UnsupportedGenericArg)
    }
}

/// Given a `args` extracts the first generic arg as type, and returns a slice pointing to the rest.
fn inner_type_and_data(
    args: &[GenericArg],
) -> Result<(&ConcreteTypeId, &[GenericArg]), SpecializationError> {
    let (first_arg, inner_data) =
        args.split_first().ok_or(SpecializationError::WrongNumberOfGenericArgs)?;
    Ok((
        try_extract_matches!(first_arg, GenericArg::Type)
            .ok_or(SpecializationError::UnsupportedGenericArg)?,
        inner_data,
    ))
}

impl ConcreteType for ConstConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum ConstLibfunc {
        AsBox(ConstAsBoxLibfunc),
        AsImmediate(ConstAsImmediateLibfunc),
    }, ConstConcreteLibfunc
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

#[derive(Default)]
pub struct ConstAsBoxLibfunc {}
impl ConstAsBoxLibfunc {
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<ConstAsBoxConcreteLibfunc, SpecializationError> {
        let (const_type, segment_id) = match args {
            [GenericArg::Type(ty), GenericArg::Value(value)] => Ok((ty, value)),
            [_, _] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }?;
        let segment_id = segment_id.to_u32().ok_or(SpecializationError::UnsupportedGenericArg)?;
        let (inner_ty, _) =
            extract_const_info(context.as_type_specialization_context(), const_type)?;
        let boxed_inner_ty = box_ty(context, inner_ty)?;

        Ok(ConstAsBoxConcreteLibfunc {
            const_type: const_type.clone(),
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

impl NamedLibfunc for ConstAsBoxLibfunc {
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

/// Libfunc for returning a compilation time constant as an immediate.
pub struct ConstAsImmediateConcreteLibfunc {
    /// The type of the returned constant.
    pub const_type: ConcreteTypeId,
    pub signature: LibfuncSignature,
}

impl SignatureBasedConcreteLibfunc for ConstAsImmediateConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

#[derive(Default)]
pub struct ConstAsImmediateLibfunc {}
impl ConstAsImmediateLibfunc {
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<ConstAsImmediateConcreteLibfunc, SpecializationError> {
        let const_type = args_as_single_type(args)?;
        let (ty, _) = extract_const_info(context.as_type_specialization_context(), &const_type)?;
        Ok(ConstAsImmediateConcreteLibfunc {
            const_type,
            signature: LibfuncSignature::new_non_branch(
                vec![],
                vec![OutputVarInfo {
                    ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
                }],
                SierraApChange::Known { new_vars_only: true },
            ),
        })
    }
}

impl NamedLibfunc for ConstAsImmediateLibfunc {
    type Concrete = ConstAsImmediateConcreteLibfunc;
    const STR_ID: &'static str = "const_as_immediate";

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
