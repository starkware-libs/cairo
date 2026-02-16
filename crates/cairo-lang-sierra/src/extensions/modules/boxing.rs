use super::snapshot::snapshot_ty;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureAndTypeGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{NamedType, OutputVarReferenceInfo, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId};

/// Type wrapping a value.
#[derive(Default)]
pub struct BoxTypeWrapped {}
impl GenericTypeArgGenericType for BoxTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Box");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: &TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if wrapped_info.storable {
            Ok(TypeInfo {
                long_id,
                zero_sized: false,
                storable: true,
                droppable: wrapped_info.droppable,
                duplicatable: wrapped_info.duplicatable,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type BoxType = GenericTypeArgGenericTypeWrapper<BoxTypeWrapped>;

define_libfunc_hierarchy! {
    pub enum BoxLibfunc {
        Into(IntoBoxLibfunc),
        LocalInto(LocalIntoBoxLibfunc),
        Unbox(UnboxLibfunc),
        ForwardSnapshot(BoxForwardSnapshotLibfunc),
    }, BoxConcreteLibfunc
}

/// Helper for getting the `Box<T>` type.
pub fn box_ty(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_wrapped_concrete_type(BoxType::id(), ty)
}

/// Libfunc for wrapping an object of type T into a box.
#[derive(Default)]
pub struct IntoBoxLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for IntoBoxLibfuncWrapped {
    const STR_ID: &'static str = "into_box";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty: box_ty(context, ty)?,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type IntoBoxLibfunc = WrapSignatureAndTypeGenericLibfunc<IntoBoxLibfuncWrapped>;

/// Libfunc for wrapping a local object of type T into a box.
#[derive(Default)]
pub struct LocalIntoBoxLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for LocalIntoBoxLibfuncWrapped {
    const STR_ID: &'static str = "local_into_box";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty: box_ty(context, ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
pub type LocalIntoBoxLibfunc = WrapSignatureAndTypeGenericLibfunc<LocalIntoBoxLibfuncWrapped>;

/// Libfunc for unboxing a `Box<T>` back into a T.
#[derive(Default)]
pub struct UnboxLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for UnboxLibfuncWrapped {
    const STR_ID: &'static str = "unbox";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ref_info = if context.get_type_info(&ty)?.zero_sized {
            OutputVarReferenceInfo::ZeroSized
        } else {
            OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic)
        };
        Ok(LibfuncSignature::new_non_branch(
            vec![box_ty(context, ty.clone())?],
            vec![OutputVarInfo { ty, ref_info }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type UnboxLibfunc = WrapSignatureAndTypeGenericLibfunc<UnboxLibfuncWrapped>;

/// Libfunc for converting `@Box<T>` into `Box<@T>`.
#[derive(Default)]
pub struct BoxForwardSnapshotLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for BoxForwardSnapshotLibfuncWrapped {
    const STR_ID: &'static str = "box_forward_snapshot";
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            snapshot_ty(context, box_ty(context, ty.clone())?)?,
            box_ty(context, snapshot_ty(context, ty)?)?,
        ))
    }
}

pub type BoxForwardSnapshotLibfunc =
    WrapSignatureAndTypeGenericLibfunc<BoxForwardSnapshotLibfuncWrapped>;
