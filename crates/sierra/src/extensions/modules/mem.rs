use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, SignatureOnlyConcreteLibFunc, SpecializationContext,
};
use crate::extensions::{
    NamedLibFunc, NoGenericArgsGenericLibFunc, SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum MemLibFunc {
        StoreTemp(StoreTempLibFunc),
        AlignTemps(AlignTempsLibFunc),
        StoreLocal(StoreLocalLibFunc),
        AllocLocals(AllocLocalsLibFunc),
        Rename(RenameLibFunc),
    }, MemConcreteLibFunc
}

/// LibFunc for storing a value into temporary memory.
#[derive(Default)]
pub struct StoreTempLibFunc {}
impl NamedLibFunc for StoreTempLibFunc {
    type Concrete = StoreTempConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_temp");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(StoreTempConcreteLibFunc {
            ty: ty.clone(),
            signature: LibFuncSignature::new_non_branch(vec![ty.clone()], vec![ty]),
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
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcreteLibFunc {
            ty: as_single_type(args)?,
            signature: LibFuncSignature::new_non_branch(vec![], vec![]),
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
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(StoreLocalConcreteLibFunc {
            ty: ty.clone(),
            signature: LibFuncSignature::new_non_branch(vec![ty.clone()], vec![ty]),
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

/// LibFunc for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalsLibFunc {}
impl NoGenericArgsGenericLibFunc for AllocLocalsLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("alloc_locals");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: LibFuncSignature::new_non_branch(vec![], vec![]),
        })
    }
}

/// LibFunc for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameLibFunc {}
impl NamedLibFunc for RenameLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("rename");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(SignatureOnlyConcreteLibFunc {
            signature: LibFuncSignature::new_non_branch(vec![ty.clone()], vec![ty]),
        })
    }
}
