use super::as_single_type;
use super::integer::Uint128Type;
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{ConcreteType, NamedType, OutputVarReferenceInfo, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing an array.
#[derive(Default)]
pub struct ArrayType {}
impl NamedType for ArrayType {
    type Concrete = ArrayConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Array");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        let info = context.get_type_info(ty.clone())?;
        if info.storable {
            Ok(ArrayConcreteType {
                info: TypeInfo {
                    long_id: Self::concrete_type_long_id(args),
                    duplicatable: false,
                    droppable: info.droppable,
                    storable: true,
                    size: 2,
                },
                ty,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}

pub struct ArrayConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}
impl ConcreteType for ArrayConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum ArrayLibFunc {
        New(ArrayNewLibFunc),
        Append(ArrayAppendLibFunc),
        At(ArrayAtLibFunc),
        // TODO(orizi): Add length after libfunc result unpacking is supported.
        // TODO(orizi): Add access after enums are supported.
    }, ArrayConcreteLibFunc
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct ArrayNewLibFunc {}
impl SignatureOnlyGenericLibFunc for ArrayNewLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("array_new");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(ArrayType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known(1),
        ))
    }
}

/// LibFunc for pushing a value into the end of an array.
#[derive(Default)]
pub struct ArrayAppendLibFunc {}
impl SignatureOnlyGenericLibFunc for ArrayAppendLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("array_append");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), ty.clone())?;
        Ok(LibFuncSignature::new_non_branch(
            vec![arr_ty.clone(), ty],
            // TODO(lior): Change `Deferred` into `AddConst` once added.
            vec![OutputVarInfo {
                ty: arr_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known(0),
        ))
    }
}

/// LibFunc for fetching a value from a specific array index.
#[derive(Default)]
pub struct ArrayAtLibFunc {}
impl SignatureOnlyGenericLibFunc for ArrayAtLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("array_at");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        // Value type must be duplicatable.
        let ty = as_single_type(args)?;
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
                ap_change: SierraApChange::Known(5),
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
                ap_change: SierraApChange::Known(3),
            },
        ];
        Ok(LibFuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}
