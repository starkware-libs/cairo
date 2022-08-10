use std::collections::HashMap;

use thiserror::Error;

use crate::ids::GenericExtensionId;
use crate::program::GenericArg;

mod core;

/// Error option while using extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Count not find the requested extension")]
    UnsupportedLibCallName,
    #[error("Expected a different number of template arguments")]
    WrongNumberOfTemplateArgs,
    #[error("Provided template arg is unsupported")]
    UnsupportedTemplateArg,
}

/// Error option while using extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionError {
    #[error("Count not specialize extension")]
    Specialization { extension_id: GenericExtensionId, error: SpecializationError },
}

/// Handles extensions usages.
pub struct Extensions {
    specializers: HashMap<GenericExtensionId, GenericExtensionBox>,
}
impl Default for Extensions {
    fn default() -> Self {
        Extensions { specializers: core::all_core_extensions() }
    }
}
impl Extensions {
    pub fn specialize(
        &self,
        extension_id: &GenericExtensionId,
        args: &[GenericArg],
    ) -> Result<ConcreteExtensionBox, ExtensionError> {
        self.specializers
            .get(extension_id)
            .ok_or_else(move || ExtensionError::Specialization {
                extension_id: extension_id.clone(),
                error: SpecializationError::UnsupportedLibCallName,
            })?
            .specialize(args)
            .map_err(move |error| ExtensionError::Specialization {
                extension_id: extension_id.clone(),
                error,
            })
    }
}

/// Trait for implementing a specialization generator.
trait GenericExtension {
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError>;
}

/// Trait for implementing a specialization generator with no generic arguments.
trait NoGenericArgsGenericExtension {
    fn specialize(&self) -> ConcreteExtensionBox;
}
impl<T: NoGenericArgsGenericExtension> GenericExtension for T {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        if args.is_empty() {
            Ok(self.specialize())
        } else {
            Err(SpecializationError::WrongNumberOfTemplateArgs)
        }
    }
}

/// Trait for a specialized extension.
pub trait ConcreteExtension {}

type GenericExtensionBox = Box<dyn GenericExtension>;
type ConcreteExtensionBox = Box<dyn ConcreteExtension>;

#[cfg(test)]
mod test;
