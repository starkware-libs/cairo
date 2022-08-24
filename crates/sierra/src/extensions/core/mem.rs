use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{
    ConcreteType, GenericLibFunc, NamedLibFunc, NamedType, NoGenericArgsGenericLibFunc,
    NonBranchConcreteLibFunc, SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

/// Type for deferred actions.
#[derive(Default)]
pub struct DeferredGeneric {}
impl NamedType for DeferredGeneric {
    type Concrete = DeferredConcrete;
    const NAME: &'static str = "Deferred";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(DeferredConcrete { ty: as_single_type(args)? })
    }
}
pub struct DeferredConcrete {
    pub ty: ConcreteTypeId,
}
impl ConcreteType for DeferredConcrete {}

define_libfunc_hierarchy! {
pub enum MemLibFunc {
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

/// LibFunc for storing a deferred value into temporary memory.
#[derive(Default)]
pub struct StoreTempGeneric {}
impl NamedLibFunc for StoreTempGeneric {
    type Concrete = StoreTempConcrete;
    const NAME: &'static str = "store_temp";
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreTempConcrete { ty: as_single_type(args)? })
    }
}

pub struct StoreTempConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for StoreTempConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// LibFunc for aligning the temporary buffer for flow control merge.
#[derive(Default)]
pub struct AlignTempsGeneric {}
impl NamedLibFunc for AlignTempsGeneric {
    type Concrete = AlignTempsConcrete;
    const NAME: &'static str = "align_temps";
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(AlignTempsConcrete { _ty: as_single_type(args)? })
    }
}

pub struct AlignTempsConcrete {
    _ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for AlignTempsConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// LibFunc for storing a deferred value into local memory.
#[derive(Default)]
pub struct StoreLocalGeneric {}
impl NamedLibFunc for StoreLocalGeneric {
    type Concrete = StoreLocalConcrete;
    const NAME: &'static str = "store_local";
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(StoreLocalConcrete { ty: as_single_type(args)? })
    }
}

pub struct StoreLocalConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for StoreLocalConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// LibFunc for allocating locals for later stores.
#[derive(Default)]
pub struct AllocLocalsGeneric {}
impl NoGenericArgsGenericLibFunc for AllocLocalsGeneric {
    type Concrete = AllocLocalsConcrete;
    const NAME: &'static str = "alloc_locals";
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(AllocLocalsConcrete {})
    }
}

pub struct AllocLocalsConcrete {}
impl NonBranchConcreteLibFunc for AllocLocalsConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// LibFunc for renaming an identifier - used to align identities for flow control merge.
#[derive(Default)]
pub struct RenameGeneric {}
impl NamedLibFunc for RenameGeneric {
    type Concrete = RenameConcrete;
    const NAME: &'static str = "rename";
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(RenameConcrete { ty: as_single_type(args)? })
    }
}

pub struct RenameConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for RenameConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}

/// LibFunc for making a type deferred for later store.
#[derive(Default)]
pub struct MoveGeneric {}
impl NamedLibFunc for MoveGeneric {
    type Concrete = MoveConcrete;
    const NAME: &'static str = "move";
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(MoveConcrete { ty: as_single_type(args)? })
    }
}

pub struct MoveConcrete {
    ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for MoveConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}
