use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::int::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::int::unsigned128::Uint128Type;
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    SpecializationContext,
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

/// Returns a map from (concrete) integer type to the number of bits in the type.
fn get_type_to_nbits_map(
    context: &dyn SignatureSpecializationContext,
) -> UnorderedHashMap<ConcreteTypeId, usize> {
    vec![
        (Uint8Type::ID, 8),
        (Uint16Type::ID, 16),
        (Uint32Type::ID, 32),
        (Uint64Type::ID, 64),
        (Uint128Type::ID, 128),
    ]
    .into_iter()
    .filter_map(|(generic_type, n_bits)| {
        Some((context.get_concrete_type(generic_type, &[]).ok()?, n_bits))
    })
    .collect()
}

/// Returns the number of bits for the given types.
// TODO(lior): Convert to a generic function that can take arbitrary number of arguments once
//   `try_map` is a stable feature.
fn get_n_bits(
    context: &dyn SignatureSpecializationContext,
    from_type: &ConcreteTypeId,
    to_type: &ConcreteTypeId,
) -> Result<(usize, usize), SpecializationError> {
    let type_to_n_bits = get_type_to_nbits_map(context);
    let from_nbits =
        *type_to_n_bits.get(from_type).ok_or(SpecializationError::UnsupportedGenericArg)?;
    let to_nbits =
        *type_to_n_bits.get(to_type).ok_or(SpecializationError::UnsupportedGenericArg)?;
    Ok((from_nbits, to_nbits))
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
        let (from_nbits, to_nbits) = get_n_bits(context, &from_ty, &to_ty)?;

        let is_valid = from_nbits <= to_nbits;
        if !is_valid {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        Ok(LibfuncSignature::new_non_branch(
            vec![from_ty],
            vec![OutputVarInfo {
                ty: to_ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// A concrete version of the `downcast` libfunc. See [DowncastLibfunc].
pub struct DowncastConcreteLibfunc {
    pub signature: LibfuncSignature,
    pub from_ty: ConcreteTypeId,
    pub from_nbits: usize,
    pub to_ty: ConcreteTypeId,
    pub to_nbits: usize,
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
        let (from_nbits, to_nbits) = get_n_bits(context, &from_ty, &to_ty)?;

        let is_valid = from_nbits >= to_nbits;
        if !is_valid {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(from_ty),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: to_ty,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    }],
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
        let (from_nbits, to_nbits) = get_n_bits(context.upcast(), &from_ty, &to_ty)?;

        Ok(DowncastConcreteLibfunc {
            signature: self.specialize_signature(context.upcast(), args)?,
            from_ty,
            from_nbits,
            to_ty,
            to_nbits,
        })
    }
}
