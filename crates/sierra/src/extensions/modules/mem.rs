use super::as_single_type;
use super::uninitialized::UninitializedType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange, SignatureOnlyGenericLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibFunc, NamedType, NoGenericArgsGenericLibFunc, OutputVarReferenceInfo,
    SignatureBasedConcreteLibFunc, SpecializationError,
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
pub struct StoreTempLibFunc {}
impl NamedLibFunc for StoreTempLibFunc {
    type Concrete = StoreTempConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_temp");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        let type_size = context.as_type_specialization_context().get_type_info(ty.clone())?.size;
        Ok(LibFuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 } }],
            SierraApChange::Known(type_size),
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(StoreTempConcreteLibFunc {
            ty,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

pub struct StoreTempConcreteLibFunc {
    pub ty: ConcreteTypeId,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for StoreTempConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for aligning the temporary buffer for flow control merge.
#[derive(Default)]
pub struct AlignTempsLibFunc {}
impl NamedLibFunc for AlignTempsLibFunc {
    type Concrete = AlignTempsConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("align_temps");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], SierraApChange::NotImplemented))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcreteLibFunc {
            ty: as_single_type(args)?,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

pub struct AlignTempsConcreteLibFunc {
    pub ty: ConcreteTypeId,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for AlignTempsConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for storing a value into local memory.
#[derive(Default)]
pub struct StoreLocalLibFunc {}
impl NamedLibFunc for StoreLocalLibFunc {
    type Concrete = StoreLocalConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_local");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
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
            SierraApChange::Known(0),
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(StoreLocalConcreteLibFunc {
            ty,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

pub struct StoreLocalConcreteLibFunc {
    pub ty: ConcreteTypeId,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for StoreLocalConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for finalizing the locals for current function.
#[derive(Default)]
pub struct FinalizeLocalsLibFunc {}
impl NoGenericArgsGenericLibFunc for FinalizeLocalsLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("finalize_locals");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], SierraApChange::FinalizeLocals))
    }
}

/// LibFunc for allocating locals for later stores.
pub struct AllocLocalConcreteLibFunc {
    pub ty: ConcreteTypeId,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for AllocLocalConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
#[derive(Default)]
pub struct AllocLocalLibFunc {}
impl NamedLibFunc for AllocLocalLibFunc {
    type Concrete = AllocLocalConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("alloc_local");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_wrapped_concrete_type(UninitializedType::id(), ty)?,
                ref_info: OutputVarReferenceInfo::NewLocalVar,
            }],
            SierraApChange::Known(0),
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(AllocLocalConcreteLibFunc {
            ty,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

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
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known(0),
        ))
    }
}
