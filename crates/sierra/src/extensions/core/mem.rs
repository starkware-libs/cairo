use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{
    GenericLibFunc, NamedLibFunc, NoGenericArgsGenericLibFunc, NonBranchConcreteLibFunc,
    SpecializationError,
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
        Ok(StoreTempConcreteLibFunc { ty: as_single_type(args)? })
    }
}

pub struct StoreTempConcreteLibFunc {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for StoreTempConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
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
        Ok(AlignTempsConcreteLibFunc { _ty: as_single_type(args)? })
    }
}

pub struct AlignTempsConcreteLibFunc {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for AlignTempsConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
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
        Ok(StoreLocalConcreteLibFunc { ty: as_single_type(args)? })
    }
}

pub struct StoreLocalConcreteLibFunc {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for StoreLocalConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// LibFunc for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalsLibFunc {}
impl NoGenericArgsGenericLibFunc for AllocLocalsLibFunc {
    type Concrete = AllocLocalsConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("alloc_locals");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(AllocLocalsConcreteLibFunc {})
    }
}

pub struct AllocLocalsConcreteLibFunc {}
impl NonBranchConcreteLibFunc for AllocLocalsConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// LibFunc for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameLibFunc {}
impl NamedLibFunc for RenameLibFunc {
    type Concrete = RenameConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("rename");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(RenameConcreteLibFunc { ty: as_single_type(args)? })
    }
}

pub struct RenameConcreteLibFunc {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for RenameConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

pub struct MoveConcreteLibFunc {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for MoveConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}
