use super::range_check::RangeCheckType;
use super::uint128::Uint128Type;
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
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if !wrapped_info.storable {
            Err(SpecializationError::UnsupportedGenericArg)
        } else {
            Ok(TypeInfo {
                long_id,
                duplicatable: false,
                droppable: wrapped_info.droppable,
                storable: true,
                size: 2,
            })
        }
    }
}
pub type ArrayType = GenericTypeArgGenericTypeWrapper<ArrayTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum ArrayLibfunc {
        New(ArrayNewLibfunc),
        Append(ArrayAppendLibfunc),
        PopFront(ArrayPopFrontLibfunc),
        At(ArrayAtLibfunc),
        Len(ArrayLenLibfunc),
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
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: None },
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
        let arr_type = context.get_wrapped_concrete_type(ArrayType::id(), ty)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![arr_type.clone()],
            vec![
                OutputVarInfo {
                    ty: arr_type,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
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
                ParamSignature {
                    ty: arr_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
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

/// Libfunc for popping the first value from the begining of an array.
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
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: arr_ty.clone(),
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

/// Libfunc for fetching a value from a specific array index.
#[derive(Default)]
pub struct ArrayAtLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for ArrayAtLibfuncWrapped {
    const STR_ID: &'static str = "array_get";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        // Value type must be duplicatable.
        if !context.get_type_info(ty.clone())?.duplicatable {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let arr_type = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let uint128_type = context.get_concrete_type(Uint128Type::id(), &[])?;
        let param_signatures = vec![
            ParamSignature::new(range_check_type.clone()),
            ParamSignature::new(arr_type.clone()),
            ParamSignature::new(uint128_type),
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
                        ty: arr_type.clone(),
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
                        ty: arr_type,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            },
        ];
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}
pub type ArrayAtLibfunc = WrapSignatureAndTypeGenericLibfunc<ArrayAtLibfuncWrapped>;
