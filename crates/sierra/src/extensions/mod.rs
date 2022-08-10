use std::collections::HashMap;

use thiserror::Error;

use crate::ids::{ConcreteExtensionId, GenericExtensionId};
use crate::program::{
    ConcreteExtensionId, ConcreteTypeId, ExtensionDeclaration, GenericArg, GenericExtensionId,
    GenericTypeId, TypeDeclaration,
};

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
    #[error("Count not specialize type")]
    TypeSpecialization { declaration: TypeDeclaration, error: SpecializationError },
    #[error("Count not specialize extension")]
    ExtensionSpecialization { declaration: ExtensionDeclaration, error: SpecializationError },
}

/// Handles extensions usages.
pub struct Extensions {
    extension_specializers: HashMap<GenericExtensionId, GenericExtensionBox>,
    type_specializers: HashMap<GenericTypeId, GenericTypeBox>,
    concrete_extensions: HashMap<ConcreteExtensionId, ConcreteExtensionBox>,
    concrete_type_info: HashMap<ConcreteTypeId, ConcreteTypeInfo>,
}
impl Default for Extensions {
    fn default() -> Self {
        Extensions {
            extension_specializers: core::all_core_extensions(),
            type_specializers: core::all_core_types(),
            concrete_extensions: HashMap::<ConcreteExtensionId, ConcreteExtensionBox>::new(),
            concrete_type_info: HashMap::<ConcreteTypeId, ConcreteTypeInfo>::new(),
        }
    }
}
impl Extensions {
    /// Adds a specialization of a type to the set of concrete types.
    pub fn specialize_type(&mut self, declaration: &TypeDeclaration) -> Result<(), ExtensionError> {
        let concrete_extension = self
            .type_specializers
            .get(&declaration.generic_id)
            .ok_or_else(move || ExtensionError::TypeSpecialization {
                declaration: declaration.clone(),
                error: SpecializationError::UnsupportedLibCallName,
            })?
            .get_info(&declaration.args)
            .map_err(move |error| ExtensionError::TypeSpecialization {
                declaration: declaration.clone(),
                error,
            })?;
        match self.concrete_type_info.insert(declaration.id.clone(), concrete_extension) {
            None => Ok(()),
            Some(_) => Err(ExtensionError::TypeSpecialization {
                declaration: declaration.clone(),
                error: SpecializationError::ConcreteIdUsedMoreThanOnce,
            }),
        }
    }
    /// Adds a specialization of an extension to the set of concrete extensions.
    pub fn specialize_extension(
        &mut self,
        declaration: &ExtensionDeclaration,
    ) -> Result<(), ExtensionError> {
        let concrete_extension = self
            .extension_specializers
            .get(&declaration.generic_id)
            .ok_or_else(move || ExtensionError::ExtensionSpecialization {
                declaration: declaration.clone(),
                error: SpecializationError::UnsupportedLibCallName,
            })?
            .specialize(&declaration.args)
            .map_err(move |error| ExtensionError::ExtensionSpecialization {
                declaration: declaration.clone(),
                error,
            })?;
        match self.concrete_extensions.insert(declaration.id.clone(), concrete_extension) {
            None => Ok(()),
            Some(_) => Err(ExtensionError::ExtensionSpecialization {
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

/// Trait for implementing a type generator.
trait GenericType {
    /// Returns the type info given generic arguments.
    fn get_info(&self, args: &[GenericArg]) -> Result<ConcreteTypeInfo, SpecializationError>;
}
struct NoGenericArgsGenericType<const SIZE: usize> {}
impl<const SIZE: usize> GenericType for NoGenericArgsGenericType<SIZE> {
    fn get_info(&self, args: &[GenericArg]) -> Result<ConcreteTypeInfo, SpecializationError> {
        if args.is_empty() {
            Ok(ConcreteTypeInfo { size: SIZE })
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// The information for a concrete type.
pub struct ConcreteTypeInfo {
    pub size: usize,
}

type GenericTypeBox = Box<dyn GenericType>;

#[cfg(test)]
mod test;
