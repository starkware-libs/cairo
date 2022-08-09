use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, NoArgsExtension,
    SpecializationError,
};
use crate::program::{ConcreteTypeId, ExtensionId, TemplateArg};

/// Helper for extracting the type from the template arguments.
fn as_single_type(args: &[TemplateArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [TemplateArg::Type(ty)] => Ok(ty.clone()),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

/// Extension for storing a deferred value into temporary memory.
struct StoreTempExtension {}
impl Extension for StoreTempExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreTempConcrete { _ty: as_single_type(args)? }))
    }
}

struct StoreTempConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreTempConcrete {}

/// Extension for aligning the temporary buffer for flow control merge.
struct AlignTempsExtension {}
impl Extension for AlignTempsExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(AlignTempsConcrete { _ty: as_single_type(args)? }))
    }
}

struct AlignTempsConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for AlignTempsConcrete {}

/// Extension for storing a deferred value into local memory.
struct StoreLocalExtension {}
impl Extension for StoreLocalExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreLocalConcrete { _ty: as_single_type(args)? }))
    }
}

struct StoreLocalConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreLocalConcrete {}

/// Extension for allocating locals for later stores.
struct AllocLocalsExtension {}
impl NoArgsExtension for AllocLocalsExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(AllocLocalsConcrete {})
    }
}

struct AllocLocalsConcrete {}
impl ConcreteExtension for AllocLocalsConcrete {}

/// Extension for renaming an identifier - used to align identities for flow control merge.
struct RenameExtension {}
impl Extension for RenameExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(RenameConcrete { _ty: as_single_type(args)? }))
    }
}

struct RenameConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for RenameConcrete {}

/// Extension for making a type deferred for later store.
struct MoveExtension {}
impl Extension for MoveExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(MoveConcrete { _ty: as_single_type(args)? }))
    }
}

struct MoveConcrete {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for MoveConcrete {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 6] {
    [
        ("store_temp".into(), Box::new(StoreTempExtension {})),
        ("align_temps".into(), Box::new(AlignTempsExtension {})),
        ("store_local".into(), Box::new(StoreLocalExtension {})),
        ("alloc_locals".into(), Box::new(AllocLocalsExtension {})),
        ("rename".into(), Box::new(RenameExtension {})),
        ("move".into(), Box::new(MoveExtension {})),
    ]
}
