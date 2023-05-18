use super::snapshot::snapshot_ty;
use super::span::SpanType;
use super::starknet::getter::boxed_ty;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureAndTypeGenericLibfunc, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext, WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing an array.
#[derive(Default)]
pub struct ArrayTypeWrapped {}
impl GenericTypeArgGenericType for ArrayTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Array");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, droppable, size, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable && size > 0 {
            Ok(TypeInfo { long_id, duplicatable: false, droppable, storable: true, size: 2 })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type ArrayType = GenericTypeArgGenericTypeWrapper<ArrayTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum ArrayLibfunc {
        New(ArrayNewLibfunc),
        Append(ArrayAppendLibfunc),
        PopFront(ArrayPopFrontLibfunc),
        SnapshotArrayAsSpan(SnapshotArrayAsSpanLibfunc),
        ToSpan(ArrayToSpanLibfunc),
        // TODO(spapini): Add split() - that results in a span and an array.
    }, ArrayConcreteLibfunc
}

/// Libfunc for creating a new array.
#[derive(Default)]
pub struct ArrayNewLibfunc {}
impl SignatureOnlyGenericLibfunc for ArrayNewLibfunc {
    const STR_ID: &'static str = "array_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(ArrayType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::SimpleDerefs,
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for pushing a value into the end of an array.
#[derive(Default)]
pub struct ArrayAppendLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayAppendLibfuncWrapped {
    const STR_ID: &'static str = "array_append";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(arr_ty.clone()).with_allow_add_const(),
                ParamSignature::new(ty),
            ],
            vec![OutputVarInfo {
                ty: arr_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type ArrayAppendLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayAppendLibfuncWrapped>;

/// Libfunc for popping the first value from the beginning of an array.
#[derive(Default)]
pub struct ArrayPopFrontLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayPopFrontLibfuncWrapped {
    const STR_ID: &'static str = "array_pop_front";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(arr_ty.clone())],
            branch_signatures: vec![
                // Non-empty.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: arr_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: boxed_ty(context, ty)?,
                            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Empty.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type ArrayPopFrontLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayPopFrontLibfuncWrapped>;

/// Libfunc for converting an array (Array<T>) to a span (Span<T>).
#[derive(Default)]
pub struct ArrayToSpanLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayToSpanLibfuncWrapped {
    const STR_ID: &'static str = "array_to_span";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let span_ty = context.get_wrapped_concrete_type(SpanType::id(), ty)?;
        Ok(reinterpret_cast_signature(arr_ty, span_ty))
    }
}
pub type ArrayToSpanLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayToSpanLibfuncWrapped>;

/// Libfunc for converting a snapshot of array (@Array<T>) to a span of snapshots (Span<@T>)).
#[derive(Default)]
pub struct SnapshotSpanToSpanLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SnapshotSpanToSpanLibfuncWrapped {
    const STR_ID: &'static str = "snapshot_array_as_span";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let snapshot_arr_ty = snapshot_ty(context, arr_ty)?;
        let snapshot_ty = snapshot_ty(context, ty)?;
        let span_snapshot_ty = context.get_wrapped_concrete_type(SpanType::id(), snapshot_ty)?;
        Ok(reinterpret_cast_signature(snapshot_arr_ty, span_snapshot_ty))
    }
}
pub type SnapshotArrayAsSpanLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SnapshotSpanToSpanLibfuncWrapped>;
