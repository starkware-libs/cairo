use super::range_check::RangeCheckType;
use super::snapshot::snapshot_ty;
use super::starknet::getter::boxed_ty;
use super::utils::reinterpret_cast_signature;
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
        PopFrontConsume(SpanPopFrontConsumeLibfunc),
        PopBack(SpanPopBackLibfunc),
        PopBackConsume(SpanPopBackConsumeLibfunc),
        Get(SpanGetLibfunc),
        Slice(SpanSliceLibfunc),
        Len(SpanLenLibfunc),
        SnapshotSpanToSpan(SnapshotSpanToSpanLibfunc),
        SpanToSnapshotSpan(SpanToSnapshotSpanLibfunc),
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
        let span_ty = get_span_ty(context, ty)?;
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
        let span_ty = get_span_ty(context, ty.clone())?;
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
}

pub type SpanPopFrontLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanPopFrontLibfuncWrapped>;

/// Libfunc for popping the first value from the beginning of a span.
#[derive(Default)]
pub struct SpanPopFrontConsumeLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanPopFrontConsumeLibfuncWrapped {
    const STR_ID: &'static str = "span_pop_front_consume";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = get_span_ty(context, ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(span_ty.clone())],
            branch_signatures: vec![
                // Non-empty.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: span_ty,
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

pub type SpanPopFrontConsumeLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SpanPopFrontConsumeLibfuncWrapped>;

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
        let span_ty = get_span_ty(context, ty.clone())?;
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
}
pub type SpanPopBackLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanPopBackLibfuncWrapped>;

/// Libfunc for popping the last value from the end of a span.
#[derive(Default)]
pub struct SpanPopBackConsumeLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanPopBackConsumeLibfuncWrapped {
    const STR_ID: &'static str = "span_pop_back_consume";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = get_span_ty(context, ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(span_ty.clone())],
            branch_signatures: vec![
                // Non-empty.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: span_ty,
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
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type SpanPopBackConsumeLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SpanPopBackConsumeLibfuncWrapped>;

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
        let snapshot_ty = snapshot_ty(context, ty)?;
        let span_ty = get_span_ty(context, snapshot_ty.clone())?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let index_type = context.get_concrete_type(SpanIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
            ParamSignature::new(span_ty),
            ParamSignature::new(index_type),
        ];
        let branch_signatures = vec![
            // Success.
            BranchSignature {
                vars: vec![
                    OutputVarInfo {
                        ty: range_check_type.clone(),
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty: boxed_ty(context, snapshot_ty)?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
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
        let snapshot_ty = snapshot_ty(context, ty)?;
        let span_type = get_span_ty(context, snapshot_ty)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let index_type = context.get_concrete_type(SpanIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
            ParamSignature::new(span_type.clone()),
            // Start
            ParamSignature::new(index_type.clone()),
            // Length
            ParamSignature::new(index_type),
        ];
        let branch_signatures = vec![
            // Success.
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
        ];
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}
pub type SpanSliceLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanSliceLibfuncWrapped>;

/// Libfunc for converting a snapshot of span `@Span<T>` to a span of snapshots `Span<@T>`.
#[derive(Default)]
pub struct SnapshotSpanToSpanLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SnapshotSpanToSpanLibfuncWrapped {
    const STR_ID: &'static str = "snapshot_span_to_span";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = get_span_ty(context, ty.clone())?;
        let snapshot_span_ty = snapshot_ty(context, span_ty)?;
        let snapshot_ty = snapshot_ty(context, ty)?;
        let span_snapshot_ty = get_span_ty(context, snapshot_ty)?;
        Ok(reinterpret_cast_signature(snapshot_span_ty, span_snapshot_ty))
    }
}

pub type SnapshotSpanToSpanLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SnapshotSpanToSpanLibfuncWrapped>;

/// Libfunc for converting a snapshot of span `@Span<T>` to a span of snapshots `Span<@T>`.
#[derive(Default)]
pub struct SpanToSnapshotSpanLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for SpanToSnapshotSpanLibfuncWrapped {
    const STR_ID: &'static str = "span_to_snapshot_span";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let span_ty = get_span_ty(context, ty.clone())?;
        let snapshot_span_ty = snapshot_ty(context, span_ty)?;
        let snapshot_ty = snapshot_ty(context, ty)?;
        let span_snapshot_ty = get_span_ty(context, snapshot_ty)?;
        Ok(reinterpret_cast_signature(span_snapshot_ty, snapshot_span_ty))
    }
}

pub type SpanToSnapshotSpanLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SpanToSnapshotSpanLibfuncWrapped>;

/// Wraps a type with a span type.
pub fn get_span_ty(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_wrapped_concrete_type(SpanType::id(), ty)
}
