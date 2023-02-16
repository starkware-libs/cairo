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

        let type_to_n_bits: UnorderedHashMap<ConcreteTypeId, usize> = vec![
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
        .collect();

        let from_nbits =
            type_to_n_bits.get(&from_ty).ok_or(SpecializationError::UnsupportedGenericArg)?;
        let to_nbits =
            type_to_n_bits.get(&to_ty).ok_or(SpecializationError::UnsupportedGenericArg)?;

        let is_valid = from_nbits <= to_nbits;
        if !is_valid {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        Ok(LibfuncSignature::new_non_branch(
            vec![from_ty.clone()],
            vec![OutputVarInfo {
                ty: to_ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
