use crate::define_extension_hierarchy;
use crate::extensions::{
    NoGenericArgsGenericExtension, NoRegistryRequiredNamedExtension, NonBranchConcreteExtension,
    SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_extension_hierarchy! {
    pub enum MemExtension {
        StoreTemp(StoreTempGeneric),
        AlignTemps(AlignTempsGeneric),
        StoreLocal(StoreLocalGeneric),
        AllocLocals(AllocLocalsGeneric),
        Rename(RenameGeneric),
        Move(MoveGeneric),
    }, MemConcrete
}

/// Helper for extracting the type from the template arguments.
fn as_single_type(args: &[GenericArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty.clone()),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}

/// Extension for storing a deferred value into temporary memory.
#[derive(Default)]
pub struct StoreTempGeneric {}
impl NoRegistryRequiredNamedExtension for StoreTempGeneric {
    type Concrete = StoreTempConcrete;
    const NAME: &'static str = "store_temp";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreTempConcrete { ty: as_single_type(args)? })
    }
}

pub struct StoreTempConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for StoreTempConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// Extension for aligning the temporary buffer for flow control merge.
#[derive(Default)]
pub struct AlignTempsGeneric {}
impl NoRegistryRequiredNamedExtension for AlignTempsGeneric {
    type Concrete = AlignTempsConcrete;
    const NAME: &'static str = "align_temps";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcrete { _ty: as_single_type(args)? })
    }
}

pub struct AlignTempsConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for AlignTempsConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// Extension for storing a deferred value into local memory.
#[derive(Default)]
pub struct StoreLocalGeneric {}
impl NoRegistryRequiredNamedExtension for StoreLocalGeneric {
    type Concrete = StoreLocalConcrete;
    const NAME: &'static str = "store_local";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreLocalConcrete { ty: as_single_type(args)? })
    }
}

pub struct StoreLocalConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for StoreLocalConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// Extension for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalsGeneric {}
impl NoGenericArgsGenericExtension for AllocLocalsGeneric {
    type Concrete = AllocLocalsConcrete;
    const NAME: &'static str = "alloc_locals";
    fn specialize(&self) -> Self::Concrete {
        AllocLocalsConcrete {}
    }
}

pub struct AllocLocalsConcrete {}
impl NonBranchConcreteExtension for AllocLocalsConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// Extension for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameGeneric {}
impl NoRegistryRequiredNamedExtension for RenameGeneric {
    type Concrete = RenameConcrete;
    const NAME: &'static str = "rename";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RenameConcrete { ty: as_single_type(args)? })
    }
}

pub struct RenameConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for RenameConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// Extension for making a type deferred for later store.
#[derive(Default)]
pub struct MoveGeneric {}
impl NoRegistryRequiredNamedExtension for MoveGeneric {
    type Concrete = MoveConcrete;
    const NAME: &'static str = "move";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(MoveConcrete { ty: as_single_type(args)? })
    }
}

pub struct MoveConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteExtension for MoveConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}
