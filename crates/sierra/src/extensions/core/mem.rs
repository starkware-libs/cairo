use super::{single_cell_identity, unpack_inputs};
use crate::extension_enum;
use crate::extensions::{
    GenericExtension, InputError, NoGenericArgsGenericExtension, NonBranchConcreteExtension,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericExtensionId};
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

extension_enum! {
    pub enum MemExtension {
        StoreTemp(StoreTempGeneric),
        AlignTemps(AlignTempsGeneric),
        StoreLocal(StoreLocalGeneric),
        AllocLocals(AllocLocalsGeneric),
        Rename(RenameGeneric),
        Move(MoveGeneric)
    }, Memoncrete
}

/// Helper for extracting the type from the template arguments.
fn as_single_type(args: &[GenericArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty.clone()),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

/// Extension for storing a deferred value into temporary memory.
pub struct StoreTempGeneric {}
impl GenericExtension for StoreTempGeneric {
    type Concrete = StoreTempConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("store_temp".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreTempConcrete { _ty: as_single_type(args)? })
    }
}

pub struct StoreTempConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for StoreTempConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        single_cell_identity::<1>(inputs)
    }
}

/// Extension for aligning the temporary buffer for flow control merge.
pub struct AlignTempsGeneric {}
impl GenericExtension for AlignTempsGeneric {
    type Concrete = AlignTempsConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("align_temps".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcrete { _ty: as_single_type(args)? })
    }
}

pub struct AlignTempsConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for AlignTempsConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        unpack_inputs::<0>(inputs)?;
        Ok(vec![])
    }
}

/// Extension for storing a deferred value into local memory.
pub struct StoreLocalGeneric {}
impl GenericExtension for StoreLocalGeneric {
    type Concrete = StoreLocalConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("store_local".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreLocalConcrete { _ty: as_single_type(args)? })
    }
}

pub struct StoreLocalConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for StoreLocalConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        single_cell_identity::<1>(inputs)
    }
}

/// Extension for allocating locals for later stores.
pub struct AllocLocalsGeneric {}
impl NoGenericArgsGenericExtension for AllocLocalsGeneric {
    type Concrete = AllocLocalsConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("alloc_locals".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        AllocLocalsConcrete {}
    }
}

pub struct AllocLocalsConcrete {}
impl NonBranchConcreteExtension for AllocLocalsConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        unpack_inputs::<0>(inputs)?;
        Ok(vec![])
    }
}

/// Extension for renaming an identifier - used to align identities for flow control merge.
pub struct RenameGeneric {}
impl GenericExtension for RenameGeneric {
    type Concrete = RenameConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("rename".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RenameConcrete { _ty: as_single_type(args)? })
    }
}

pub struct RenameConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for RenameConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        single_cell_identity::<1>(inputs)
    }
}

/// Extension for making a type deferred for later store.
pub struct MoveGeneric {}
impl GenericExtension for MoveGeneric {
    type Concrete = MoveConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("move".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(MoveConcrete { _ty: as_single_type(args)? })
    }
}

pub struct MoveConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for MoveConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        single_cell_identity::<1>(inputs)
    }
}
