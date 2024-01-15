use super::boxing::box_ty;
use super::snapshot::snapshot_ty;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureAndTypeGenericLibfunc, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext, WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, ConcreteType, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
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
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, droppable, duplicatable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable {
            Ok(TypeInfo { long_id, zero_sized: false, storable: true, droppable, duplicatable })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
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

/// Helper for getting the type `Nullable<T>`.
pub fn nullable_ty(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_wrapped_concrete_type(NullableType::id(), ty)
}

define_libfunc_hierarchy! {
    pub enum NullableLibfunc {
        Null(NullLibfunc),
        NullableFromBox(NullableFromBoxLibfunc),
        MatchNullable(MatchNullableLibfunc),
        ForwardSnapshot(NullableForwardSnapshotLibfunc),
    }, NullableConcreteLibfunc
}

/// Libfunc for creating a null object of type `Nullable<T>`.
#[derive(Default)]
pub struct NullLibfunc {}
impl SignatureOnlyGenericLibfunc for NullLibfunc {
    const STR_ID: &'static str = "null";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: nullable_ty(context, ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for converting `Box<T>` to `Nullable<T>`.
#[derive(Default)]
pub struct NullableFromBoxLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for NullableFromBoxLibfuncWrapped {
    const STR_ID: &'static str = "nullable_from_box";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(box_ty(context, ty.clone())?, nullable_ty(context, ty)?))
    }
}
pub type NullableFromBoxLibfunc = WrapSignatureAndTypeGenericLibfunc<NullableFromBoxLibfuncWrapped>;

/// Libfunc for converting `Nullable<T>` to either `Box<T>` or nothing (in the case of `null`).
#[derive(Default)]
pub struct MatchNullableLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for MatchNullableLibfuncWrapped {
    const STR_ID: &'static str = "match_nullable";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(nullable_ty(context, ty.clone())?)],
            branch_signatures: vec![
                // `null`.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // `Box<T>`.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: box_ty(context, ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type MatchNullableLibfunc = WrapSignatureAndTypeGenericLibfunc<MatchNullableLibfuncWrapped>;

/// Libfunc for converting `@Nullable<T>` into `Nullable<@T>`.
#[derive(Default)]
pub struct NullableForwardSnapshotLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for NullableForwardSnapshotLibfuncWrapped {
    const STR_ID: &'static str = "nullable_forward_snapshot";
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            snapshot_ty(context, nullable_ty(context, ty.clone())?)?,
            nullable_ty(context, snapshot_ty(context, ty)?)?,
        ))
    }
}

pub type NullableForwardSnapshotLibfunc =
    WrapSignatureAndTypeGenericLibfunc<NullableForwardSnapshotLibfuncWrapped>;
