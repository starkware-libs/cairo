use super::uninitialized::UninitializedType;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureAndTypeGenericLibfunc, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
    args_as_single_type,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum MemLibfunc {
        StoreTemp(StoreTempLibfunc),
        StoreLocal(StoreLocalLibfunc),
        FinalizeLocals(FinalizeLocalsLibfunc),
        AllocLocal(AllocLocalLibfunc),
        Rename(RenameLibfunc),
    }, MemConcreteLibfunc
}

/// Libfunc for storing a value into temporary memory.
#[derive(Default)]
pub struct StoreTempLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for StoreTempLibfuncWrapped {
    const STR_ID: &'static str = "store_temp";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let type_info = context.as_type_specialization_context().get_type_info(ty.clone())?;
        if !type_info.storable {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty,
                ref_info: if type_info.zero_sized {
                    OutputVarReferenceInfo::ZeroSized
                } else {
                    OutputVarReferenceInfo::NewTempVar { idx: 0 }
                },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type StoreTempLibfunc = WrapSignatureAndTypeGenericLibfunc<StoreTempLibfuncWrapped>;

/// Libfunc for storing a value into local memory.
#[derive(Default)]
pub struct StoreLocalLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for StoreLocalLibfuncWrapped {
    const STR_ID: &'static str = "store_local";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let uninitialized_type =
            context.get_wrapped_concrete_type(UninitializedType::id(), ty.clone())?;
        let type_info = context.as_type_specialization_context().get_type_info(ty.clone())?;
        if !type_info.storable {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(uninitialized_type),
                ParamSignature {
                    ty: ty.clone(),
                    allow_deferred: true,
                    allow_add_const: true,
                    allow_const: true,
                },
            ],
            vec![OutputVarInfo {
                ty,
                ref_info: if type_info.zero_sized {
                    OutputVarReferenceInfo::ZeroSized
                } else {
                    OutputVarReferenceInfo::NewLocalVar
                },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type StoreLocalLibfunc = WrapSignatureAndTypeGenericLibfunc<StoreLocalLibfuncWrapped>;

/// Libfunc for finalizing the locals for current function.
#[derive(Default)]
pub struct FinalizeLocalsLibfunc {}
impl NoGenericArgsGenericLibfunc for FinalizeLocalsLibfunc {
    const STR_ID: &'static str = "finalize_locals";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for AllocLocalLibfuncWrapped {
    const STR_ID: &'static str = "alloc_local";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let type_info = context.as_type_specialization_context().get_type_info(ty.clone())?;
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(UninitializedType::id(), ty)?,
                ref_info: if type_info.zero_sized {
                    OutputVarReferenceInfo::ZeroSized
                } else {
                    OutputVarReferenceInfo::NewLocalVar
                },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type AllocLocalLibfunc = WrapSignatureAndTypeGenericLibfunc<AllocLocalLibfuncWrapped>;

/// Libfunc for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameLibfunc {}
impl SignatureOnlyGenericLibfunc for RenameLibfunc {
    const STR_ID: &'static str = "rename";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(reinterpret_cast_signature(ty.clone(), ty))
    }
}
