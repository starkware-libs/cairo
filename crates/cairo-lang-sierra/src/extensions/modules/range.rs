use std::ops::Shl;

use cairo_lang_utils::require;
use itertools::Itertools;
use num_bigint::{BigInt, ToBigInt};
use num_traits::{One, Signed};
use starknet_types_core::felt::Felt as Felt252;

use super::non_zero::{nonzero_ty, NonZeroType};
use super::range_check::RangeCheckType;
use super::utils::{reinterpret_cast_signature, Range};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, args_as_two_types, ConcreteType, NamedLibfunc, NamedType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type for Range(x, y) where x <= y.
#[derive(Default)]
pub struct RangeTypeWrapped {}
impl GenericTypeArgGenericType for RangeTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Range");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // TODO: Check inner type.

        assert!(
            wrapped_info.storable
                && wrapped_info.duplicatable
                && wrapped_info.droppable
                && !wrapped_info.zero_sized
        );
        Ok(TypeInfo {
            long_id,
            duplicatable: true,
            droppable: true,
            storable: true,
            zero_sized: false,
        })
    }
}
pub type RangeType = GenericTypeArgGenericTypeWrapper<RangeTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum RangeLibfunc {
        ConsumeFront(RangeConsumeFrontLibfunc),
    }, RangeConcreteLibfunc
}

/// Libfunc that takes the range `[x, y)` and if `x < y`, returns `x` and the range `[x + 1, y)`.
#[derive(Default)]
pub struct RangeConsumeFrontLibfunc {}
impl SignatureOnlyGenericLibfunc for RangeConsumeFrontLibfunc {
    const STR_ID: &'static str = "range_consume_front";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let range_ty = context.get_wrapped_concrete_type(RangeType::id(), ty.clone())?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(range_ty.clone())],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                        },
                        OutputVarInfo {
                            ty: range_ty,
                            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false }, // TODO
                },
                // Failure.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false }, // TODO
                },
            ],
            fallthrough: Some(0),
        })
    }
}
