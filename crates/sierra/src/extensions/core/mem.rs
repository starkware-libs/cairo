use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox,
    NoGenericArgsGenericExtension, SpecializationError,
};
use crate::program::{ConcreteTypeId, GenericArg, GenericExtensionId};

/// Helper for extracting the type from the template arguments.
fn as_single_type(args: &[GenericArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty.clone()),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

/// Extension for storing a deferred value into temporary memory.
struct StoreTempGeneric {}
impl GenericExtension for StoreTempGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreTempConcrete { _ty: as_single_type(args)? }))
    }
}

struct StoreTempConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreTempConcrete {}

/// Extension for aligning the temporary buffer for flow control merge.
struct AlignTempsGeneric {}
impl GenericExtension for AlignTempsGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(AlignTempsConcrete { _ty: as_single_type(args)? }))
    }
}

struct AlignTempsConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for AlignTempsConcrete {}

/// Extension for storing a deferred value into local memory.
struct StoreLocalGeneric {}
impl GenericExtension for StoreLocalGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreLocalConcrete { _ty: as_single_type(args)? }))
    }
}

struct StoreLocalConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreLocalConcrete {}

/// Extension for allocating locals for later stores.
struct AllocLocalsGeneric {}
impl NoGenericArgsGenericExtension for AllocLocalsGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(AllocLocalsConcrete {})
    }
}

struct AllocLocalsConcrete {}
impl ConcreteExtension for AllocLocalsConcrete {}

/// Extension for renaming an identifier - used to align identities for flow control merge.
struct RenameGeneric {}
impl GenericExtension for RenameGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RenameConcrete { _ty: as_single_type(args)? }))
    }
}

struct RenameConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for RenameConcrete {}

/// Extension for making a type deferred for later store.
struct MoveGeneric {}
impl GenericExtension for MoveGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(MoveConcrete { _ty: as_single_type(args)? }))
    }
}

struct MoveConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for MoveConcrete {}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 6] {
    [
        ("store_temp".into(), Box::new(StoreTempGeneric {})),
        ("align_temps".into(), Box::new(AlignTempsGeneric {})),
        ("store_local".into(), Box::new(StoreLocalGeneric {})),
        ("alloc_locals".into(), Box::new(AllocLocalsGeneric {})),
        ("rename".into(), Box::new(RenameGeneric {})),
        ("move".into(), Box::new(MoveGeneric {})),
    ]
}
