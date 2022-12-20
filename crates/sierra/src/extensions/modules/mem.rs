use super::uninitialized::UninitializedType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureAndTypeGenericLibFunc, SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
    WrapSignatureAndTypeGenericLibFunc,
};
use crate::extensions::{
    args_as_single_type, NamedType, NoGenericArgsGenericLibFunc, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum MemLibFunc {
        StoreTemp(StoreTempLibFunc),
        AlignTemps(AlignTempsLibFunc),
        StoreLocal(StoreLocalLibFunc),
        FinalizeLocals(FinalizeLocalsLibFunc),
        AllocLocal(AllocLocalLibFunc),
        Rename(RenameLibFunc),
    }, MemConcreteLibFunc
}

/// LibFunc for storing a value into temporary memory.
#[derive(Default)]
pub struct StoreTempLibFuncWrapped {}
impl SignatureAndTypeGenericLibFunc for StoreTempLibFuncWrapped {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_temp");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibFuncSignature, SpecializationError> {
        context.as_type_specialization_context().get_type_info(ty.clone())?;
        Ok(LibFuncSignature::new_non_branch_ex(
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
pub type StoreTempLibFunc = WrapSignatureAndTypeGenericLibFunc<StoreTempLibFuncWrapped>;

/// LibFunc for aligning the temporary buffer for flow control merge.
#[derive(Default)]
pub struct AlignTempsLibFuncWrapped {}
impl SignatureAndTypeGenericLibFunc for AlignTempsLibFuncWrapped {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("align_temps");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        _ty: ConcreteTypeId,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], SierraApChange::NotImplemented))
    }
}
pub type AlignTempsLibFunc = WrapSignatureAndTypeGenericLibFunc<AlignTempsLibFuncWrapped>;

/// LibFunc for storing a value into local memory.
#[derive(Default)]
pub struct StoreLocalLibFuncWrapped {}
impl SignatureAndTypeGenericLibFunc for StoreLocalLibFuncWrapped {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_local");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let uninitialized_type =
            context.get_wrapped_concrete_type(UninitializedType::id(), ty.clone())?;
        Ok(LibFuncSignature::new_non_branch_ex(
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
pub type StoreLocalLibFunc = WrapSignatureAndTypeGenericLibFunc<StoreLocalLibFuncWrapped>;

/// LibFunc for finalizing the locals for current function.
#[derive(Default)]
pub struct FinalizeLocalsLibFunc {}
impl NoGenericArgsGenericLibFunc for FinalizeLocalsLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("finalize_locals");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// LibFunc for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalLibFuncWrapped {}
impl SignatureAndTypeGenericLibFunc for AllocLocalLibFuncWrapped {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("alloc_local");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(UninitializedType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::NewLocalVar,
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
pub type AllocLocalLibFunc = WrapSignatureAndTypeGenericLibFunc<AllocLocalLibFuncWrapped>;

/// LibFunc for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameLibFunc {}
impl SignatureOnlyGenericLibFunc for RenameLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("rename");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
