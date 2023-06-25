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

pub struct IntTypeInfo {
    pub nbits: usize,
    pub signed: bool,
}
impl IntTypeInfo {
    /// Does this type fit into the other type.
    fn upcast_valid(&self, to: &IntTypeInfo) -> bool {
        // If both has the same sign support, the destination must be at least as large.
        if self.signed == to.signed {
            self.nbits <= to.nbits
        // If the type is unsigned and the other is signed, the destination must be larger.
        } else if !self.signed {
            self.nbits < to.nbits
        // Signed to unsigned is always invalid.
        } else {
            false
        }
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
        if !from_info.upcast_valid(&to_info) {
            return Err(SpecializationError::UnsupportedGenericArg);
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
        if from_info.upcast_valid(&to_info) {
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
