use super::bounded_int::BoundedIntType;
use super::int::signed::{Sint8Type, Sint16Type, Sint32Type, Sint64Type};
use super::int::signed128::Sint128Type;
use super::int::unsigned::{Uint8Type, Uint16Type, Uint32Type, Uint64Type};
use super::int::unsigned128::Uint128Type;
use super::range_check::RangeCheckType;
use super::utils::Range;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    NamedType, OutputVarReferenceInfo, SpecializationError, args_as_single_type,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

fn check_inner_type(ty_info: &TypeInfo) -> Result<(), SpecializationError> {
    // Note: the implementation assumes the following types are of size 1.
    match (&ty_info.long_id.generic_id, &ty_info.long_id.generic_args[..]) {
        (id, []) if *id == Uint8Type::id() => (),
        (id, []) if *id == Uint16Type::id() => (),
        (id, []) if *id == Uint32Type::id() => (),
        (id, []) if *id == Uint64Type::id() => (),
        (id, []) if *id == Uint128Type::id() => (),
        (id, []) if *id == Sint8Type::id() => (),
        (id, []) if *id == Sint16Type::id() => (),
        (id, []) if *id == Sint32Type::id() => (),
        (id, []) if *id == Sint64Type::id() => (),
        (id, []) if *id == Sint128Type::id() => (),
        (id, [GenericArg::Value(_), GenericArg::Value(_)]) if *id == BoundedIntType::id() => (),
        _ => return Err(SpecializationError::UnsupportedGenericArg),
    };
    Ok(())
}

/// Type for `IntRange(x, y)` where `x <= y`.
#[derive(Default)]
pub struct IntRangeTypeWrapped {}
impl GenericTypeArgGenericType for IntRangeTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("IntRange");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        check_inner_type(&wrapped_info)?;

        // The following assert is a sanity check. It should follow from the fact that
        // `check_inner_type` passed.
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
pub type IntRangeType = GenericTypeArgGenericTypeWrapper<IntRangeTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum IntRangeLibfunc {
        TryNew(IntRangeTryNewLibfunc),
        PopFront(IntRangePopFrontLibfunc),
    }, IntRangeConcreteLibfunc
}

/// Libfunc that constructs the range `[x, y)` if `x <= y`.
/// Otherwise, returns the empty range `[y, y)`.
#[derive(Default)]
pub struct IntRangeTryNewLibfunc {}
impl SignatureOnlyGenericLibfunc for IntRangeTryNewLibfunc {
    const STR_ID: &'static str = "int_range_try_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let range_ty = context.get_wrapped_concrete_type(IntRangeType::id(), ty.clone())?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        if !Range::from_type(context, ty.clone())?.is_small_range() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(ty.clone()),
                ParamSignature::new(ty.clone()),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(range_check_type.clone(), 0),
                        OutputVarInfo {
                            ty: range_ty.clone(),
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(range_check_type, 0),
                        OutputVarInfo {
                            ty: range_ty,
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc that takes the range `[x, y)` and if `x < y`, returns the range `[x + 1, y)` and the
/// value `x`.
#[derive(Default)]
pub struct IntRangePopFrontLibfunc {}
impl SignatureOnlyGenericLibfunc for IntRangePopFrontLibfunc {
    const STR_ID: &'static str = "int_range_pop_front";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let range_ty = context.get_wrapped_concrete_type(IntRangeType::id(), ty.clone())?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(range_ty.clone())],
            branch_signatures: vec![
                // Failure.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
