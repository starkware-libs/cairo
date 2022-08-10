use std::collections::HashMap;

use thiserror::Error;

use crate::ids::{ConcreteExtensionId,GenericExtensionId};
use crate::program::{ExtensionDeclaration, GenericArg};

mod core;

/// Error option while using extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Count not find the requested extension")]
    UnsupportedLibCallName,
    #[error("Expected a different number of generic arguments")]
    WrongNumberOfGenericArgs,
    #[error("Provided generic arg is unsupported")]
    UnsupportedGenericArg,
    #[error("The concrete id is used more than once")]
    ConcreteIdUsedMoreThanOnce,
}

/// Error option while using extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionError {
    #[error("Count not specialize extension")]
    Specialization { declaration: ExtensionDeclaration, error: SpecializationError },
}

/// Handles extensions usages.
pub struct Extensions {
    specializers: HashMap<GenericExtensionId, GenericExtensionBox>,
    concrete_extensions: HashMap<ConcreteExtensionId, ConcreteExtensionBox>,
}
impl Default for Extensions {
    fn default() -> Self {
        Extensions {
            specializers: core::all_core_extensions(),
            concrete_extensions: HashMap::<ConcreteExtensionId, ConcreteExtensionBox>::new(),
        }
    }
}
impl Extensions {
    /// Adds a specialization of an extension to the set of concrete extensions.
    pub fn specialize(&mut self, declaration: &ExtensionDeclaration) -> Result<(), ExtensionError> {
        let concrete_extension = self
            .specializers
            .get(&declaration.generic_id)
            .ok_or_else(move || ExtensionError::Specialization {
                declaration: declaration.clone(),
                error: SpecializationError::UnsupportedLibCallName,
            })?
            .specialize(&declaration.args)
            .map_err(move |error| ExtensionError::Specialization {
                declaration: declaration.clone(),
                error,
            })?;
        match self.concrete_extensions.insert(declaration.id.clone(), concrete_extension) {
            None => Ok(()),
            Some(_) => Err(ExtensionError::Specialization {
                declaration: declaration.clone(),
                error: SpecializationError::ConcreteIdUsedMoreThanOnce,
            }),
        }
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
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Trait for a specialized extension.
pub trait ConcreteExtension {}

type GenericExtensionBox = Box<dyn GenericExtension>;
type ConcreteExtensionBox = Box<dyn ConcreteExtension>;

#[cfg(test)]
mod test;
