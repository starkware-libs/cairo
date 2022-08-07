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

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 4] {
    [
        (ExtensionId::Name("store_temp".into()), Box::new(StoreTempExtension {})),
        (ExtensionId::Name("store_local".into()), Box::new(StoreLocalExtension {})),
        (ExtensionId::Name("rename".into()), Box::new(RenameExtension {})),
        (ExtensionId::Name("move".into()), Box::new(MoveExtension {})),
    ]
}
