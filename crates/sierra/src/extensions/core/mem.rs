use crate::define_libcall_hierarchy;
use crate::extensions::{
    GenericLibcall, NamedLibcall, NoGenericArgsGenericLibcall, NonBranchConcreteLibcall,
    SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_libcall_hierarchy! {
    pub enum MemLibcall {
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

/// Libcall for storing a deferred value into temporary memory.
#[derive(Default)]
pub struct StoreTempGeneric {}
impl NamedLibcall for StoreTempGeneric {
    type Concrete = StoreTempConcrete;
    const NAME: &'static str = "store_temp";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreTempConcrete { ty: as_single_type(args)? })
    }
}

pub struct StoreTempConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibcall for StoreTempConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// Libcall for aligning the temporary buffer for flow control merge.
#[derive(Default)]
pub struct AlignTempsGeneric {}
impl NamedLibcall for AlignTempsGeneric {
    type Concrete = AlignTempsConcrete;
    const NAME: &'static str = "align_temps";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcrete { _ty: as_single_type(args)? })
    }
}

pub struct AlignTempsConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteLibcall for AlignTempsConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// Libcall for storing a deferred value into local memory.
#[derive(Default)]
pub struct StoreLocalGeneric {}
impl NamedLibcall for StoreLocalGeneric {
    type Concrete = StoreLocalConcrete;
    const NAME: &'static str = "store_local";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreLocalConcrete { ty: as_single_type(args)? })
    }
}

pub struct StoreLocalConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibcall for StoreLocalConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// Libcall for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalsGeneric {}
impl NoGenericArgsGenericLibcall for AllocLocalsGeneric {
    type Concrete = AllocLocalsConcrete;
    const NAME: &'static str = "alloc_locals";
    fn specialize(&self) -> Self::Concrete {
        AllocLocalsConcrete {}
    }
}

pub struct AllocLocalsConcrete {}
impl NonBranchConcreteLibcall for AllocLocalsConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// Libcall for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameGeneric {}
impl NamedLibcall for RenameGeneric {
    type Concrete = RenameConcrete;
    const NAME: &'static str = "rename";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(RenameConcrete { ty: as_single_type(args)? })
    }
}

pub struct RenameConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibcall for RenameConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// Libcall for making a type deferred for later store.
#[derive(Default)]
pub struct MoveGeneric {}
impl NamedLibcall for MoveGeneric {
    type Concrete = MoveConcrete;
    const NAME: &'static str = "move";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(MoveConcrete { ty: as_single_type(args)? })
    }
}

pub struct MoveConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibcall for MoveConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}
