use super::as_single_type;
use super::uninitialized::UninitializedType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, SignatureOnlyConcreteLibFunc, SignatureSpecializationContext,
    SpecializationContext,
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
        _context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![ty],
            vec![OutputVarReferenceInfo::NewTempVar { idx: 0 }],
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(StoreTempConcreteLibFunc { ty, signature: self.specialize_signature(&context, args)? })
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
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], vec![]))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcreteLibFunc {
            ty: as_single_type(args)?,
            signature: self.specialize_signature(&context, args)?,
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
        Ok(LibFuncSignature::new_non_branch(
            vec![
                context.get_wrapped_concrete_type(UninitializedType::id(), ty.clone())?,
                ty.clone(),
            ],
            vec![ty],
            vec![OutputVarReferenceInfo::NewLocalVar],
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(StoreLocalConcreteLibFunc { ty, signature: self.specialize_signature(&context, args)? })
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
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("finalize_locals");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], vec![]))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: <Self as NoGenericArgsGenericLibFunc>::specialize_signature(self, &context)?,
        })
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
            vec![context.get_wrapped_concrete_type(UninitializedType::id(), ty)?],
            vec![OutputVarReferenceInfo::NewLocalVar],
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(AllocLocalConcreteLibFunc { ty, signature: self.specialize_signature(&context, args)? })
    }
}

/// LibFunc for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameLibFunc {}
impl NamedLibFunc for RenameLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("rename");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![ty],
            vec![OutputVarReferenceInfo::SameAsParam { param_idx: 0 }],
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(&context, args)? })
    }
}
