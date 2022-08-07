use super::validate_no_args;
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, SpecializationError,
};
use crate::program::{ConcreteTypeId, ExtensionId, TemplateArg};

fn get_type(tmpl_args: &[TemplateArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match tmpl_args {
        [TemplateArg::Type(ty)] => Ok(ty.clone()),
        _ => Err(SpecializationError::UnsupportedTemplateArg),
    }
}

struct StoreTempExtension {}
impl Extension for StoreTempExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreTemp { _ty: get_type(tmpl_args)? }))
    }
}

struct StoreTemp {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreTemp {}

struct AlignTempsExtension {}
impl Extension for AlignTempsExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(AlignTemps { _ty: get_type(tmpl_args)? }))
    }
}

struct AlignTemps {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for AlignTemps {}

struct StoreLocalExtension {}
impl Extension for StoreLocalExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(StoreLocal { _ty: get_type(tmpl_args)? }))
    }
}

struct StoreLocal {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for StoreLocal {}

struct AllocLocalsExtension {}
impl Extension for AllocLocalsExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(AllocLocals {}))
    }
}

struct AllocLocals {}
impl ConcreteExtension for AllocLocals {}

struct RenameExtension {}
impl Extension for RenameExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(Rename { _ty: get_type(tmpl_args)? }))
    }
}

struct Rename {
    _ty: ConcreteTypeId,
}
impl ConcreteExtension for Rename {}

struct MoveExtension {}
impl Extension for MoveExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        Ok(Box::new(Move { _ty: get_type(tmpl_args)? }))
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
