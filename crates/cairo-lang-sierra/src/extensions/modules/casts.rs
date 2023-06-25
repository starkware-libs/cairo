use super::int::signed::{Sint16Type, Sint32Type, Sint64Type, Sint8Type};
use super::int::signed128::Sint128Type;
use super::int::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::int::unsigned128::Uint128Type;
use super::range_check::RangeCheckType;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    args_as_two_types, NamedLibfunc, NamedType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum CastLibfunc {
        Downcast(DowncastLibfunc),
        Upcast(UpcastLibfunc),
    }, CastConcreteLibfunc
}

/// The type of casting between two integer types.
#[derive(PartialEq, Eq)]
pub struct CastType {
    /// Does the source type have values above the destination type possible values.
    pub overflow_above: bool,
    /// Does the source type have values below the destination type possible values.
    pub overflow_below: bool,
}

pub struct IntTypeInfo {
    pub nbits: usize,
    pub signed: bool,
}
impl IntTypeInfo {
    /// Returns the cast type.
    pub fn cast_type(&self, to: &IntTypeInfo) -> CastType {
        match (self.signed, to.signed) {
            (false, false) => {
                // Unsigned to unsigned.
                // Overflow below is never possible, as the minimum value of both types is 0.
                // Overflow above is possible if `self` has strictly more bits than the `to`.
                // We use `>=` instead of `>` to provide backward compatibility with the case
                // casting from a type to itself. TODO(orizi): Remove this backward compatibility at
                // next major sierra version.
                CastType { overflow_above: self.nbits >= to.nbits, overflow_below: false }
            }
            (true, true) => {
                // Signed to signed.
                // Both overflows are possible if `self` has strictly more bits than `to`.
                let can_overflow = self.nbits > to.nbits;
                CastType { overflow_above: can_overflow, overflow_below: can_overflow }
            }
            (true, false) => {
                // Signed to unsigned.
                // Overflow below is always possible, as the minimum value of `self` is lower than 0
                // and of `to` is 0. Overflow above is possible if the `self` type
                // has 2 bits more than `to` (as i8 to u7 cannot overflow, but i8 to u6 can).
                CastType { overflow_above: self.nbits >= to.nbits + 2, overflow_below: true }
            }
            (false, true) => {
                // Unsigned to signed.
                // Overflow below is never possible, as the minimum value of `self` is 0 and of `to`
                // lower than 0. Overflow above is possible if `self` has more bits
                // than `to` (as u8 to i9 cannot overflow, but u8 to i8 can).
                CastType { overflow_above: self.nbits >= to.nbits, overflow_below: false }
            }
        }
    }

    /// Returns true if this type can participate in downcasts.
    fn downcastable(&self) -> bool {
        // We don't support downcasting larger than 128-bit integers, as this would not reduce the
        // need for range checks.
        self.nbits <= 128
    }
}

/// Returns a number of bits in a concrete integer type.
fn get_int_info(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
) -> Result<IntTypeInfo, SpecializationError> {
    Ok(match context.get_type_info(ty)?.long_id.generic_id {
        id if id == Uint8Type::ID => IntTypeInfo { nbits: 8, signed: false },
        id if id == Sint8Type::ID => IntTypeInfo { nbits: 8, signed: true },
        id if id == Uint16Type::ID => IntTypeInfo { nbits: 16, signed: false },
        id if id == Sint16Type::ID => IntTypeInfo { nbits: 16, signed: true },
        id if id == Uint32Type::ID => IntTypeInfo { nbits: 32, signed: false },
        id if id == Sint32Type::ID => IntTypeInfo { nbits: 32, signed: true },
        id if id == Uint64Type::ID => IntTypeInfo { nbits: 64, signed: false },
        id if id == Sint64Type::ID => IntTypeInfo { nbits: 64, signed: true },
        id if id == Uint128Type::ID => IntTypeInfo { nbits: 128, signed: false },
        id if id == Sint128Type::ID => IntTypeInfo { nbits: 128, signed: true },
        _ => return Err(SpecializationError::UnsupportedGenericArg),
    })
}

/// Libfunc for casting from one type to another where any input value can fit into the destination
/// type. For example, from u8 to u64.
#[derive(Default)]
pub struct UpcastLibfunc {}
impl SignatureOnlyGenericLibfunc for UpcastLibfunc {
    const STR_ID: &'static str = "upcast";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (from_ty, to_ty) = args_as_two_types(args)?;
        let from_info = get_int_info(context, from_ty.clone())?;
        let to_info = get_int_info(context, to_ty.clone())?;
        let cast_type = from_info.cast_type(&to_info);
        if cast_type.overflow_above || cast_type.overflow_below {
            // Finding if the detected possible overflow is not actually possible, but a backward
            // compatibilty based one.
            // TODO(orizi): Remove this check after the backward compatibility is removed at next
            // major sierra version.
            if from_info.signed || to_info.signed || from_info.nbits != to_info.nbits {
                return Err(SpecializationError::UnsupportedGenericArg);
            }
        }

        Ok(reinterpret_cast_signature(from_ty, to_ty))
    }
}

/// A concrete version of the `downcast` libfunc. See [DowncastLibfunc].
pub struct DowncastConcreteLibfunc {
    pub signature: LibfuncSignature,
    pub from_ty: ConcreteTypeId,
    pub from_info: IntTypeInfo,
    pub to_ty: ConcreteTypeId,
    pub to_info: IntTypeInfo,
}
impl SignatureBasedConcreteLibfunc for DowncastConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for casting from one type to another where the input value may not fit into the
/// destination type. For example, from u64 to u8.
#[derive(Default)]
pub struct DowncastLibfunc {}
impl NamedLibfunc for DowncastLibfunc {
    type Concrete = DowncastConcreteLibfunc;
    const STR_ID: &'static str = "downcast";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (from_ty, to_ty) = args_as_two_types(args)?;
        let from_info = get_int_info(context, from_ty.clone())?;
        let to_info = get_int_info(context, to_ty.clone())?;
        if !from_info.downcastable() || !to_info.downcastable() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ParamSignature::new(from_ty),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: to_ty,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![rc_output_info],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (from_ty, to_ty) = args_as_two_types(args)?;
        Ok(DowncastConcreteLibfunc {
            signature: self.specialize_signature(context.upcast(), args)?,
            from_info: get_int_info(context.upcast(), from_ty.clone())?,
            from_ty,
            to_info: get_int_info(context.upcast(), to_ty.clone())?,
            to_ty,
        })
    }
}
