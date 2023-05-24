use super::range_check::RangeCheckType;
use super::snapshot::snapshot_ty;
use super::span::get_span_ty;
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

type ArrayIndexType = super::int::unsigned::Uint32Type;

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
        PopFrontConsume(ArrayPopFrontConsumeLibfunc),
        Get(ArrayGetLibfunc),
        Slice(ArraySliceLibfunc),
        Len(ArrayLenLibfunc),
        SnapshotPopFront(ArraySnapshotPopFrontLibfunc),
        SnapshotPopBack(ArraySnapshotPopBackLibfunc),
        ToSpan(ArrayToSpanLibfunc),
        SnapshotToSpan(SnapshotArrayToSpanLibfunc),
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

/// Libfunc for getting the length of the array.
#[derive(Default)]
pub struct ArrayLenLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayLenLibfuncWrapped {
    const STR_ID: &'static str = "array_len";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![snapshot_ty(context, arr_ty)?],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(ArrayIndexType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
pub type ArrayLenLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayLenLibfuncWrapped>;

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
                            ty: arr_ty.clone(),
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
                        ty: arr_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type ArrayPopFrontLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayPopFrontLibfuncWrapped>;

/// Libfunc for popping the first value from the beginning of an array.
#[derive(Default)]
pub struct ArrayPopFrontConsumeLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayPopFrontConsumeLibfuncWrapped {
    const STR_ID: &'static str = "array_pop_front_consume";

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
pub type ArrayPopFrontConsumeLibfunc =
    WrapSignatureAndTypeGenericLibfunc<ArrayPopFrontConsumeLibfuncWrapped>;

/// Libfunc for fetching a value from a specific array index.
#[derive(Default)]
pub struct ArrayGetLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayGetLibfuncWrapped {
    const STR_ID: &'static str = "array_get";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_type = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let index_type = context.get_concrete_type(ArrayIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
            ParamSignature::new(snapshot_ty(context, arr_type)?),
            ParamSignature::new(index_type),
        ];
        let branch_signatures = vec![
            // First (success) branch returns rc, array and element; failure branch does not return
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
                        ty: boxed_ty(context, snapshot_ty(context, ty)?)?,
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
pub type ArrayGetLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayGetLibfuncWrapped>;

/// Libfunc for getting a slice of an array snapshot.
#[derive(Default)]
pub struct ArraySliceLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArraySliceLibfuncWrapped {
    const STR_ID: &'static str = "array_slice";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_snapshot_type =
            snapshot_ty(context, context.get_wrapped_concrete_type(ArrayType::id(), ty)?)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let index_type = context.get_concrete_type(ArrayIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
            ParamSignature::new(arr_snapshot_type.clone()),
            // Start
            ParamSignature::new(index_type.clone()),
            // Length
            ParamSignature::new(index_type),
        ];
        let branch_signatures = vec![
            // First (success) branch returns rc, array and the slice snapshot; failure branch does
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
                        ty: arr_snapshot_type,
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
pub type ArraySliceLibfunc = WrapSignatureAndTypeGenericLibfunc<ArraySliceLibfuncWrapped>;

/// Libfunc for popping the first value from the beginning of an array snapshot.
#[derive(Default)]
pub struct ArraySnapshotPopFrontLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArraySnapshotPopFrontLibfuncWrapped {
    const STR_ID: &'static str = "array_snapshot_pop_front";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let arr_snapshot_ty = snapshot_ty(context, arr_ty)?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(arr_snapshot_ty.clone())],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: arr_snapshot_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: boxed_ty(context, snapshot_ty(context, ty)?)?,
                            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: arr_snapshot_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type ArraySnapshotPopFrontLibfunc =
    WrapSignatureAndTypeGenericLibfunc<ArraySnapshotPopFrontLibfuncWrapped>;

/// Libfunc for popping the last value from the end of an array snapshot.
#[derive(Default)]
pub struct ArraySnapshotPopBackLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArraySnapshotPopBackLibfuncWrapped {
    const STR_ID: &'static str = "array_snapshot_pop_back";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let arr_snapshot_ty = snapshot_ty(context, arr_ty)?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(arr_snapshot_ty.clone())],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: arr_snapshot_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: boxed_ty(context, snapshot_ty(context, ty)?)?,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: arr_snapshot_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type ArraySnapshotPopBackLibfunc =
    WrapSignatureAndTypeGenericLibfunc<ArraySnapshotPopBackLibfuncWrapped>;

/// Libfunc for converting an array (`Array<T>`) to a span (`Span<T>`).
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
        let span_ty = get_span_ty(context, ty)?;
        Ok(reinterpret_cast_signature(arr_ty, span_ty))
    }
}
pub type ArrayToSpanLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayToSpanLibfuncWrapped>;

/// Libfunc for converting a snapshot of array (`@Array<T>`) to a span of snapshots (`Span<@T>`).
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
        let span_snapshot_ty = get_span_ty(context, snapshot_ty)?;
        Ok(reinterpret_cast_signature(snapshot_arr_ty, span_snapshot_ty))
    }
}
pub type SnapshotArrayToSpanLibfunc =
    WrapSignatureAndTypeGenericLibfunc<SnapshotSpanToSpanLibfuncWrapped>;
