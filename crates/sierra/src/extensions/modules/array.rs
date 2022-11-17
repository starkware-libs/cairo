use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
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
