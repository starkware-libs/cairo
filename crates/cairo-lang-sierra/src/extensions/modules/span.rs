use super::range_check::RangeCheckType;
use super::snapshot::snapshot_ty;
use super::starknet::getter::boxed_ty;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureAndTypeGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{NamedType, OutputVarReferenceInfo, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId};

type SpanIndexType = super::int::unsigned::Uint32Type;

/// Type representing a span.
#[derive(Default)]
pub struct SpanTypeWrapped {}
impl GenericTypeArgGenericType for SpanTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Span");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, duplicatable, droppable, size, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable && size > 0 {
            Ok(TypeInfo { long_id, duplicatable, droppable, storable: true, size: 2 })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SpanType = GenericTypeArgGenericTypeWrapper<SpanTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum SpanLibfunc {
        PopFront(SpanPopFrontLibfunc),
        PopBack(SpanPopBackLibfunc),
        Get(SpanGetLibfunc),
        Slice(SpanSliceLibfunc),
        Len(SpanLenLibfunc),
        SnapshotAsSpan(SnapshotAsSpanLibfunc),
    }, SpanConcreteLibfunc
}

/// Libfunc for getting the length of the span.
#[derive(Default)]
pub struct SpanLenLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanLenLibfuncWrapped {
    const STR_ID: &'static str = "span_len";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = context.get_wrapped_concrete_type(SpanType::id(), ty)?;
        let snapshot_span_ty = snapshot_ty(context, span_ty)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![snapshot_span_ty],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(SpanIndexType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
pub type SpanLenLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanLenLibfuncWrapped>;

/// Libfunc for popping the first value from the beginning of a span.
#[derive(Default)]
pub struct SpanPopFrontLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanPopFrontLibfuncWrapped {
    const STR_ID: &'static str = "span_pop_front";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = context.get_wrapped_concrete_type(SpanType::id(), ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(span_ty.clone())],
            branch_signatures: vec![
                // Non-empty.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: span_ty.clone(),
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
                    vars: vec![OutputVarInfo {
                        ty: span_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type SpanPopFrontLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanPopFrontLibfuncWrapped>;

/// Libfunc for popping the last value from the end of a span.
#[derive(Default)]
pub struct SpanPopBackLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanPopBackLibfuncWrapped {
    const STR_ID: &'static str = "span_pop_back";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = context.get_wrapped_concrete_type(SpanType::id(), ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(span_ty.clone())],
            branch_signatures: vec![
                // Non-empty.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: span_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: boxed_ty(context, ty)?,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Empty.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: span_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type SpanPopBackLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanPopBackLibfuncWrapped>;

/// Libfunc for fetching a value from a specific span index.
#[derive(Default)]
pub struct SpanGetLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanGetLibfuncWrapped {
    const STR_ID: &'static str = "span_get";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = context.get_wrapped_concrete_type(SpanType::id(), ty.clone())?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let index_type = context.get_concrete_type(SpanIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()),
            ParamSignature::new(span_ty),
            ParamSignature::new(index_type),
        ];
        let branch_signatures = vec![
            // First (success) branch returns rc, span and element; failure branch does not return
            // an element.
            BranchSignature {
                vars: vec![
                    OutputVarInfo {
                        ty: range_check_type.clone(),
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty: boxed_ty(context, ty)?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
            BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
        ];
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}
pub type SpanGetLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanGetLibfuncWrapped>;

/// Libfunc for getting a slice of a span.
#[derive(Default)]
pub struct SpanSliceLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanSliceLibfuncWrapped {
    const STR_ID: &'static str = "span_slice";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_type = context.get_wrapped_concrete_type(SpanType::id(), ty)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let index_type = context.get_concrete_type(SpanIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()),
            ParamSignature::new(span_type.clone()),
            // Start
            ParamSignature::new(index_type.clone()),
            // Length
            ParamSignature::new(index_type),
        ];
        let branch_signatures = vec![
            // First (success) branch returns rc, span and the slice snapshot; failure branch does
            // not return an element.
            BranchSignature {
                vars: vec![
                    OutputVarInfo {
                        ty: range_check_type.clone(),
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty: span_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
            BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
        ];
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}
pub type SpanSliceLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanSliceLibfuncWrapped>;

/// Libfunc for converting a span snapshot to a span of snapshots.
#[derive(Default)]
pub struct SnapshotAsSpanLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SnapshotAsSpanLibfuncWrapped {
    const STR_ID: &'static str = "snapshot_span_as_span";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = context.get_wrapped_concrete_type(SpanType::id(), ty.clone())?;
        let snapshot_span_ty = snapshot_ty(context, span_ty)?;
        let snapshot_ty = snapshot_ty(context, ty)?;
        let span_snapshot_ty = context.get_wrapped_concrete_type(SpanType::id(), snapshot_ty)?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(snapshot_span_ty)],
            vec![OutputVarInfo {
                ty: span_snapshot_ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type SnapshotAsSpanLibfunc = WrapSignatureAndTypeGenericLibfunc<SnapshotAsSpanLibfuncWrapped>;
