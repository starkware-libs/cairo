use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::uint::Uint16Type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext,
};
use crate::extensions::uint::{Uint32Type, Uint64Type, Uint8Type};
use crate::extensions::uint128::Uint128Type;
use crate::extensions::{
    args_as_two_types, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum CastLibfunc {
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
