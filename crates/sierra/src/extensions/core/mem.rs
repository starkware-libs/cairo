use super::as_single_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::Registries;
use crate::extensions::{
    ConcreteType, GenericLibFunc, NamedLibFunc, NamedType, NoGenericArgsGenericLibFunc,
    NonBranchConcreteLibFunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type for deferred actions.
#[derive(Default)]
pub struct DeferredType {}
impl NamedType for DeferredType {
    type Concrete = DeferredConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Deferred");
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(DeferredConcreteType { ty: as_single_type(args)? })
    }
}
pub struct DeferredConcreteType {
    pub ty: ConcreteTypeId,
}
impl ConcreteType for DeferredConcreteType {}

define_libfunc_hierarchy! {
    pub enum MemLibFunc {
        StoreTemp(StoreTempLibFunc),
        AlignTemps(AlignTempsLibFunc),
        StoreLocal(StoreLocalLibFunc),
        AllocLocals(AllocLocalsLibFunc),
        Rename(RenameLibFunc),
        Move(MoveLibFunc),
    }, MemConcreteLibFunc
}

/// LibFunc for storing a deferred value into temporary memory.
#[derive(Default)]
pub struct StoreTempLibFunc {}
impl NamedLibFunc for StoreTempLibFunc {
    type Concrete = StoreTempConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_temp");
    fn specialize(
        &self,
        _registries: Registries<'_>,
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
        _registries: Registries<'_>,
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

/// LibFunc for storing a deferred value into local memory.
#[derive(Default)]
pub struct StoreLocalLibFunc {}
impl NamedLibFunc for StoreLocalLibFunc {
    type Concrete = StoreLocalConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("store_local");
    fn specialize(
        &self,
        _registries: Registries<'_>,
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
        _registries: Registries<'_>,
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
        _registries: Registries<'_>,
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

/// LibFunc for making a type deferred for later store.
#[derive(Default)]
pub struct MoveLibFunc {}
impl NamedLibFunc for MoveLibFunc {
    type Concrete = MoveConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("move");
    fn specialize(
        &self,
        _registries: Registries<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(MoveConcreteLibFunc { ty: as_single_type(args)? })
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
