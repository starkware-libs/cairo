use super::boxing::BoxType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureAndTypeGenericLibFunc, SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibFunc,
};
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// A type that holds a possibly-null pointer to an object.
///
/// It behaves exactly like `Option<Box<T>>`, except that it only uses 1 memory cell (rather than 2
/// in `Option<Box<T>>`) - the value is 0 if and only if there is no object.
///
/// This type uses the fact that Casm pointers can never be zero.
#[derive(Default)]
pub struct NullableTypeWrapped {}
impl GenericTypeArgGenericType for NullableTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Nullable");
    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if !wrapped_info.storable {
            Err(SpecializationError::UnsupportedGenericArg)
        } else {
            Ok(TypeInfo { long_id, size: 1, ..wrapped_info })
        }
    }
}
pub type NullableType = GenericTypeArgGenericTypeWrapper<NullableTypeWrapped>;

pub struct NullableConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}
impl ConcreteType for NullableConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum NullableLibFunc {
        Null(NullLibFunc),
        IntoNullable(IntoNullableLibFunc),
    }, NullableConcreteLibFunc
}

/// LibFunc for creating a null object of type `Nullable<T>`.
#[derive(Default)]
pub struct NullLibFunc {}
impl SignatureOnlyGenericLibFunc for NullLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("null");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(NullableType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// LibFunc for converting `Box<T>` to `Nullable<T>`.
#[derive(Default)]
pub struct IntoNullableLibFuncWrapped {}
impl SignatureAndTypeGenericLibFunc for IntoNullableLibFuncWrapped {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("into_nullable");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_wrapped_concrete_type(BoxType::id(), ty.clone())?],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(NullableType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type IntoNullableLibFunc = WrapSignatureAndTypeGenericLibFunc<IntoNullableLibFuncWrapped>;
