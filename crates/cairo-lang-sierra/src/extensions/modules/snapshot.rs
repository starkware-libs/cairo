use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type for a type's snapshot.
#[derive(Default)]
pub struct SnapshotTypeWrapped {}
impl GenericTypeArgGenericType for SnapshotTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Snapshot");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { zero_sized, storable, duplicatable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // Duplicatable types are their own snapshot - as the snapshot itself is useless if we can
        // dup the value already.
        if storable && !duplicatable {
            Ok(TypeInfo {
                long_id,
                zero_sized,
                storable: true,
                droppable: true,
                duplicatable: true,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SnapshotType = GenericTypeArgGenericTypeWrapper<SnapshotTypeWrapped>;

/// Returns the type snapshot for a given type `T`.
/// For duplicatable returns `T` itself, as a regular dup is already a snapshot.
pub fn snapshot_ty(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    if context.get_type_info(ty.clone())?.duplicatable {
        Ok(ty)
    } else {
        context.get_wrapped_concrete_type(SnapshotType::id(), ty)
    }
}

/// Libfunc for taking a snapshot `Snapshot<T>` from a T.
#[derive(Default)]
pub struct SnapshotTakeLibfunc {}
impl SignatureOnlyGenericLibfunc for SnapshotTakeLibfunc {
    const STR_ID: &'static str = "snapshot_take";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![
                OutputVarInfo {
                    ty: ty.clone(),
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty: snapshot_ty(context, ty)?,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
