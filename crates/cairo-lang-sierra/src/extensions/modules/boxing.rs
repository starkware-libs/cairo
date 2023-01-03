use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
};
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type wrapping a value.
#[derive(Default)]
pub struct BoxTypeWrapped {}
impl GenericTypeArgGenericType for BoxTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Box");

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
pub type BoxType = GenericTypeArgGenericTypeWrapper<BoxTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum BoxLibfunc {
        Into(IntoBoxLibfunc),
        Unbox(UnboxLibfunc),
    }, BoxConcreteLibfunc
}

/// Libfunc for wrapping an object of type T into a box.
#[derive(Default)]
pub struct IntoBoxLibfunc {}
impl SignatureOnlyGenericLibfunc for IntoBoxLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("into_box");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(BoxType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for unboxing a `Box<T>` back into a T.
#[derive(Default)]
pub struct UnboxLibfunc {}
impl SignatureOnlyGenericLibfunc for UnboxLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("unbox");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![context.get_wrapped_concrete_type(BoxType::id(), ty.clone())?],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
