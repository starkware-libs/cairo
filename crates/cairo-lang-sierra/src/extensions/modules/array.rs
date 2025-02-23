use super::boxing::box_ty;
use super::range_check::RangeCheckType;
use super::snapshot::snapshot_ty;
use super::structure::StructConcreteType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureAndTypeGenericLibfunc, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext, SpecializationContext, WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_single_type,
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
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, droppable, zero_sized, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable && !zero_sized {
            Ok(TypeInfo {
                long_id,
                duplicatable: false,
                droppable,
                storable: true,
                zero_sized: false,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type ArrayType = GenericTypeArgGenericTypeWrapper<ArrayTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum ArrayLibfunc {
        New(ArrayNewLibfunc),
        SpanFromTuple(SpanFromTupleLibfunc),
        TupleFromSpan(TupleFromSpanLibfunc),
        Append(ArrayAppendLibfunc),
        PopFront(ArrayPopFrontLibfunc),
        PopFrontConsume(ArrayPopFrontConsumeLibfunc),
        Get(ArrayGetLibfunc),
        Slice(ArraySliceLibfunc),
        Len(ArrayLenLibfunc),
        SnapshotPopFront(ArraySnapshotPopFrontLibfunc),
        SnapshotPopBack(ArraySnapshotPopBackLibfunc),
        SnapshotMultiPopFront(ArraySnapshotMultiPopFrontLibfunc),
        SnapshotMultiPopBack(ArraySnapshotMultiPopBackLibfunc),
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

/// Libfunc for creating a span from a box of struct of members of the same type.
#[derive(Default)]
pub struct SpanFromTupleLibfuncWrapped;
impl SignatureAndTypeGenericLibfunc for SpanFromTupleLibfuncWrapped {
    const STR_ID: &'static str = "span_from_tuple";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let member_type = validate_tuple_and_fetch_ty(context, &ty)?;

        Ok(LibfuncSignature::new_non_branch(
            vec![box_ty(context, snapshot_ty(context, ty)?)?],
            vec![OutputVarInfo {
                ty: snapshot_ty(
                    context,
                    context.get_wrapped_concrete_type(ArrayType::id(), member_type.clone())?,
                )?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

pub type SpanFromTupleLibfunc = WrapSignatureAndTypeGenericLibfunc<SpanFromTupleLibfuncWrapped>;

/// Libfunc for creating a box of struct of members of the same type from a span.
#[derive(Default)]
pub struct TupleFromSpanLibfuncWrapped;
impl SignatureAndTypeGenericLibfunc for TupleFromSpanLibfuncWrapped {
    const STR_ID: &'static str = "tuple_from_span";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let member_type = validate_tuple_and_fetch_ty(context, &ty)?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(snapshot_ty(
                context,
                context.get_wrapped_concrete_type(ArrayType::id(), member_type)?,
            )?)],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: snapshot_ty(context, box_ty(context, ty)?)?,
                        ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Validates that the given type is a tuple with all members of the same type, and returns the type
/// of the members.
/// Any user type with such members is also considered a tuple.
fn validate_tuple_and_fetch_ty(
    context: &dyn SignatureSpecializationContext,
    ty: &ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    let struct_type = StructConcreteType::try_from_concrete_type(context, ty)?;
    if struct_type.info.zero_sized {
        return Err(SpecializationError::UnsupportedGenericArg);
    }
    let mut members = struct_type.members.into_iter();
    let member_type = members.next().ok_or(SpecializationError::UnsupportedGenericArg)?;
    for member in members {
        if member != member_type {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
    }
    Ok(member_type)
}

pub type TupleFromSpanLibfunc = WrapSignatureAndTypeGenericLibfunc<TupleFromSpanLibfuncWrapped>;

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
                            ty: box_ty(context, ty)?,
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
                            ty: box_ty(context, ty)?,
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
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type, 0);
        let branch_signatures = vec![
            // First (success) branch returns rc, array and element; failure branch does not return
            // an element.
            BranchSignature {
                vars: vec![
                    rc_output_info.clone(),
                    OutputVarInfo {
                        ty: box_ty(context, snapshot_ty(context, ty)?)?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
            BranchSignature {
                vars: vec![rc_output_info],
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
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type, 0);
        let branch_signatures = vec![
            // Success.
            BranchSignature {
                vars: vec![
                    // Range check.
                    rc_output_info.clone(),
                    // Array slice snapshot.
                    OutputVarInfo {
                        ty: arr_snapshot_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
            // Failure - returns only the range check buffer.
            BranchSignature {
                vars: vec![rc_output_info],
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
                            ty: box_ty(context, snapshot_ty(context, ty)?)?,
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
                            ty: box_ty(context, snapshot_ty(context, ty)?)?,
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

/// Libfunc for popping multiple first values from the beginning of an array snapshot.
#[derive(Default)]
pub struct ArraySnapshotMultiPopFrontLibfunc {}
impl NamedLibfunc for ArraySnapshotMultiPopFrontLibfunc {
    const STR_ID: &'static str = "array_snapshot_multi_pop_front";

    type Concrete = ConcreteMultiPopLibfunc;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let popped_ty = args_as_single_type(args)?;
        let ty = validate_tuple_and_fetch_ty(context, &popped_ty)?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let arr_snapshot_ty = snapshot_ty(context, arr_ty)?;
        let range_check_ty = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_ty.clone()).with_allow_add_const(),
                ParamSignature::new(arr_snapshot_ty.clone()),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(range_check_ty.clone(), 0),
                        OutputVarInfo {
                            ty: arr_snapshot_ty.clone(),
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                        OutputVarInfo {
                            ty: snapshot_ty(context, box_ty(context, popped_ty)?)?,
                            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(range_check_ty, 0),
                        OutputVarInfo {
                            ty: arr_snapshot_ty,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
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
        let popped_ty = args_as_single_type(args)?;
        Ok(ConcreteMultiPopLibfunc {
            popped_ty,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// Libfunc for popping the last value from the end of an array snapshot.
#[derive(Default)]
pub struct ArraySnapshotMultiPopBackLibfunc {}
impl NamedLibfunc for ArraySnapshotMultiPopBackLibfunc {
    const STR_ID: &'static str = "array_snapshot_multi_pop_back";

    type Concrete = ConcreteMultiPopLibfunc;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let popped_ty = args_as_single_type(args)?;
        let ty = validate_tuple_and_fetch_ty(context, &popped_ty)?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let arr_snapshot_ty = snapshot_ty(context, arr_ty)?;
        let range_check_ty = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_ty.clone()).with_allow_add_const(),
                ParamSignature::new(arr_snapshot_ty.clone()),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(range_check_ty.clone(), 0),
                        OutputVarInfo {
                            ty: arr_snapshot_ty.clone(),
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                        OutputVarInfo {
                            ty: snapshot_ty(context, box_ty(context, popped_ty)?)?,
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![
                        OutputVarInfo::new_builtin(range_check_ty, 0),
                        OutputVarInfo {
                            ty: arr_snapshot_ty,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
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
        let popped_ty = args_as_single_type(args)?;
        Ok(ConcreteMultiPopLibfunc {
            popped_ty,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// Struct the data for a multi pop action.
pub struct ConcreteMultiPopLibfunc {
    pub popped_ty: ConcreteTypeId,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for ConcreteMultiPopLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
