use super::{Specialization, SpecializationBox, SpecializationError, Specializer, SpecializerBox};
use crate::program::{Identifier, TemplateArg, Type};

struct StoreSpecializer {}
impl Specializer for StoreSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        Ok(Box::new(match tmpl_args {
            [TemplateArg::Type(ty)] => Ok(Store { _ty: ty.clone() }),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }?))
    }
}

struct Store {
    _ty: Type,
}
impl Specialization for Store {}

struct RenameSpecializer {}
impl Specializer for RenameSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        Ok(Box::new(match tmpl_args {
            [TemplateArg::Type(ty)] => Ok(Rename { _ty: ty.clone() }),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }?))
    }
}

struct Rename {
    _ty: Type,
}
impl Specialization for Rename {}

struct MoveSpecializer {}
impl Specializer for MoveSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        Ok(Box::new(match tmpl_args {
            [TemplateArg::Type(ty)] => Ok(Move { _ty: ty.clone() }),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }?))
    }
}

struct Move {
    _ty: Type,
}
impl Specialization for Move {}

pub(super) fn extensions() -> [(Identifier, SpecializerBox); 3] {
    [
        (Identifier::Name("store".into()), Box::new(StoreSpecializer {})),
        (Identifier::Name("rename".into()), Box::new(RenameSpecializer {})),
        (Identifier::Name("move".into()), Box::new(MoveSpecializer {})),
    ]
}
