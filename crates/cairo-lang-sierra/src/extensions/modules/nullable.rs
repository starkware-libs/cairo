use super::boxing::BoxType;
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
    pub enum NullableLibfunc {
        Null(NullLibfunc),
        IntoNullable(IntoNullableLibfunc),
        FromNullable(FromNullableLibfunc),
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
                ty: context.get_wrapped_concrete_type(NullableType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for converting `Box<T>` to `Nullable<T>`.
#[derive(Default)]
pub struct IntoNullableLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for IntoNullableLibfuncWrapped {
    const STR_ID: &'static str = "into_nullable";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: context.get_wrapped_concrete_type(BoxType::id(), ty.clone())?,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(NullableType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type IntoNullableLibfunc = WrapSignatureAndTypeGenericLibfunc<IntoNullableLibfuncWrapped>;

/// Libfunc for converting `Nullable<T>` to either `Box<T>` or nothing (in the case of `null`).
#[derive(Default)]
pub struct FromNullableLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for FromNullableLibfuncWrapped {
    const STR_ID: &'static str = "from_nullable";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(
                context.get_wrapped_concrete_type(NullableType::id(), ty.clone())?,
            )],
            branch_signatures: vec![
                // `null`.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // `Box<T>`.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: context.get_wrapped_concrete_type(BoxType::id(), ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
pub type FromNullableLibfunc = WrapSignatureAndTypeGenericLibfunc<FromNullableLibfuncWrapped>;
