use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, NoArgsExtension,
    SpecializationError,
};
use crate::program::{ConcreteTypeId, ExtensionId, TemplateArg};

/// Helper for extracting the type from the template arguments.
fn get_type(args: &[TemplateArg]) -> Result<ConcreteTypeId, SpecializationError> {
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
        Ok(Box::new(StoreTemp { _ty: get_type(args)? }))
    }
}

struct StoreTemp {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreTemp {}

/// Extension for aligning the temporary buffer for flow control merge.
struct AlignTempsExtension {}
impl Extension for AlignTempsExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(AlignTemps { _ty: get_type(args)? }))
    }
}

struct AlignTemps {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for AlignTemps {}

/// Extension for storing a deferred value into local memory.
struct StoreLocalExtension {}
impl Extension for StoreLocalExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreLocal { _ty: get_type(args)? }))
    }
}

struct StoreLocal {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreLocal {}

/// Extension for allocating locals for later stores.
struct AllocLocalsExtension {}
impl NoArgsExtension for AllocLocalsExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(AllocLocals {})
    }
}

struct AllocLocals {}
impl ConcreteExtension for AllocLocals {}

/// Extension for renaming an identifier - used to align identities for flow control merge.
struct RenameExtension {}
impl Extension for RenameExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(Rename { _ty: get_type(args)? }))
    }
}

struct Rename {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for Rename {}

/// Extension for making a type deferred for later store.
struct MoveExtension {}
impl Extension for MoveExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(Move { _ty: get_type(args)? }))
    }
}

struct Move {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for Move {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 6] {
    [
        (ExtensionId::Name("store_temp".into()), Box::new(StoreTempExtension {})),
        (ExtensionId::Name("align_temps".into()), Box::new(AlignTempsExtension {})),
        (ExtensionId::Name("store_local".into()), Box::new(StoreLocalExtension {})),
        (ExtensionId::Name("alloc_locals".into()), Box::new(AllocLocalsExtension {})),
        (ExtensionId::Name("rename".into()), Box::new(RenameExtension {})),
        (ExtensionId::Name("move".into()), Box::new(MoveExtension {})),
    ]
}
