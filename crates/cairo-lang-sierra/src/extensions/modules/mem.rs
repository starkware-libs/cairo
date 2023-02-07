use super::uninitialized::UninitializedType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureAndTypeGenericLibfunc, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::{
    args_as_single_type, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum MemLibfunc {
        StoreTemp(StoreTempLibfunc),
        AlignTemps(AlignTempsLibfunc),
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
        context.as_type_specialization_context().get_type_info(ty.clone())?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type StoreTempLibfunc = WrapSignatureAndTypeGenericLibfunc<StoreTempLibfuncWrapped>;

/// Libfunc for aligning the temporary buffer for flow control merge.
#[derive(Default)]
pub struct AlignTempsLibfuncWrapped {}
impl SignatureAndTypeGenericLibfunc for AlignTempsLibfuncWrapped {
    const STR_ID: &'static str = "align_temps";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        _ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(vec![], vec![], SierraApChange::NotImplemented))
    }
}
pub type AlignTempsLibfunc = WrapSignatureAndTypeGenericLibfunc<AlignTempsLibfuncWrapped>;

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
            vec![OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::NewLocalVar }],
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
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(UninitializedType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::NewLocalVar,
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
        Ok(LibfuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
