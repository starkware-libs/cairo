use super::range_check::RangeCheckType;
use super::snapshot::SnapshotType;
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

type QueueIndexType = super::uint::Uint32Type;

/// Type representing a queue.
#[derive(Default)]
pub struct QueueTypeWrapped {}
impl GenericTypeArgGenericType for QueueTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Queue");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, droppable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable {
            Ok(TypeInfo { long_id, duplicatable: false, droppable, storable: true, size: 2 })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type QueueType = GenericTypeArgGenericTypeWrapper<QueueTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum QueueLibfunc {
        New(QueueNewLibfunc),
        Append(QueueAppendLibfunc),
        PopFront(QueuePopFrontLibfunc),
        Get(QueueGetLibfunc),
        Len(QueueLenLibfunc),
    }, QueueConcreteLibfunc
}

/// Libfunc for creating a new queue.
#[derive(Default)]
pub struct QueueNewLibfunc {}
impl SignatureOnlyGenericLibfunc for QueueNewLibfunc {
    const STR_ID: &'static str = "queue_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(QueueType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: None },
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for getting the length of the queue.
#[derive(Default)]
pub struct QueueLenLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for QueueLenLibfuncWrapped {
    const STR_ID: &'static str = "queue_len";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let queue_ty = context.get_wrapped_concrete_type(QueueType::id(), ty)?;
        let snapshot_ty = context.get_wrapped_concrete_type(SnapshotType::id(), queue_ty)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![snapshot_ty],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(QueueIndexType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type QueueLenLibfunc = WrapSignatureAndTypeGenericLibfunc<QueueLenLibfuncWrapped>;

/// Libfunc for pushing a value into the end of a queue.
#[derive(Default)]
pub struct QueueAppendLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for QueueAppendLibfuncWrapped {
    const STR_ID: &'static str = "queue_append";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let queue_ty = context.get_wrapped_concrete_type(QueueType::id(), ty.clone())?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: queue_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(ty),
            ],
            vec![OutputVarInfo {
                ty: queue_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type QueueAppendLibfunc = WrapSignatureAndTypeGenericLibfunc<QueueAppendLibfuncWrapped>;

/// Libfunc for popping the first value from the begining of a queue.
#[derive(Default)]
pub struct QueuePopFrontLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for QueuePopFrontLibfuncWrapped {
    const STR_ID: &'static str = "queue_pop_front";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let queue_ty = context.get_wrapped_concrete_type(QueueType::id(), ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(queue_ty.clone())],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: queue_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: queue_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type QueuePopFrontLibfunc = WrapSignatureAndTypeGenericLibfunc<QueuePopFrontLibfuncWrapped>;

/// Libfunc for fetching a value from a specific queue index.
#[derive(Default)]
pub struct QueueGetLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for QueueGetLibfuncWrapped {
    const STR_ID: &'static str = "queue_get";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        // Value type must be duplicatable.
        if !context.get_type_info(ty.clone())?.duplicatable {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let queue_type = context.get_wrapped_concrete_type(QueueType::id(), ty.clone())?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let uint128_type = context.get_concrete_type(QueueIndexType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()),
            ParamSignature::new(queue_type.clone()),
            ParamSignature::new(uint128_type),
        ];
        let branch_signatures = vec![
            // First (success) branch returns rc, queue and element; failure branch does not return
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
                        ty: queue_type.clone(),
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                    },
                    OutputVarInfo {
                        ty,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
            BranchSignature {
                vars: vec![
                    OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty: queue_type,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
        ];
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}
pub type QueueGetLibfunc = WrapSignatureAndTypeGenericLibfunc<QueueGetLibfuncWrapped>;
