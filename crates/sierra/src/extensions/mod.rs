use std::collections::HashMap;

use thiserror::Error;

use crate::program::{Extension, Identifier, TemplateArg};

mod core;

/// Error option while using extensions.
#[derive(Error, Debug, PartialEq)]
pub enum SpecializationError {
    #[error("Count not find the requested extension")]
    UnsupportedLibCallName,
    #[error("Expected a different number of template arguments")]
    WrongNumberOfTemplateArgs,
    #[error("Provided template arg is unsupported")]
    UnsupportedTemplateArg,
}

/// Error option while using extensions.
#[derive(Error, Debug, PartialEq)]
pub enum ExtensionError {
    #[error("Count not specialize extension")]
    Specialization { extension: Extension, error: SpecializationError },
}

/// Handles extensions usages.
pub struct Extensions {
    specializers: HashMap<Identifier, SpecializerBox>,
}
impl Default for Extensions {
    fn default() -> Self {
        Extensions { specializers: core::all_core_extensions() }
    }
}
impl Extensions {
    pub fn specialize(&self, extension: &Extension) -> Result<SpecializationBox, ExtensionError> {
        self.specializers
            .get(&extension.id)
            .ok_or_else(move || ExtensionError::Specialization {
                extension: extension.clone(),
                error: SpecializationError::UnsupportedLibCallName,
            })?
            .specialize(&extension.tmpl_args)
            .map_err(move |error| ExtensionError::Specialization {
                extension: extension.clone(),
                error,
            })
    }
}

/// Trait for implementing a specialization generator.
trait Specializer {
    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError>;
}

pub trait Specialization {}

type SpecializerBox = Box<dyn Specializer>;
type SpecializationBox = Box<dyn Specialization>;

#[cfg(test)]
mod test;
