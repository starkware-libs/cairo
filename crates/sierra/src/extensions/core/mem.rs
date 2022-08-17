use crate::extensions::{
    ConcreteExtension, GenericExtension, NoGenericArgsGenericExtension, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericExtensionId};
use crate::program::GenericArg;
use crate::super_extension;

super_extension! {
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
impl ConcreteExtension for StoreTempConcrete {}

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
impl ConcreteExtension for AlignTempsConcrete {}

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
impl ConcreteExtension for StoreLocalConcrete {}

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
impl ConcreteExtension for AllocLocalsConcrete {}

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
impl ConcreteExtension for RenameConcrete {}

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
impl ConcreteExtension for MoveConcrete {}
