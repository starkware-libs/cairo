use std::collections::HashMap;

use casm::instructions::Instruction;
use thiserror::Error;

use crate::ids::{ConcreteExtensionId, GenericExtensionId, GenericTypeId};
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

mod core;

/// Error occurring while specializing extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Count not find the requested id")]
    UnsupportedId,
    #[error("Expected a different number of generic arguments")]
    WrongNumberOfGenericArgs,
    #[error("Provided generic arg is unsupported")]
    UnsupportedGenericArg,
}

/// Error occurring while testing extension inputs.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum InputError {
    #[error("Expected different number of arguments")]
    WrongNumberOfArgs,
    #[error("Expected a different memory layout")]
    MemoryLayoutMismatch,
}

/// Extension related errors.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionError {
    #[error("Could not specialize type")]
    TypeSpecialization { type_id: GenericTypeId, error: SpecializationError },
    #[error("Could not specialize extension")]
    ExtensionSpecialization { extension_id: GenericExtensionId, error: SpecializationError },
    #[error("Requested extension not declared.")]
    UndeclaredExtension { extension_id: ConcreteExtensionId },
    #[error("Encountered unexpected data in extension inputs")]
    Inputs { extension_id: GenericExtensionId, error: InputError },
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
}

/// Handles extensions usages.
pub struct Extensions {
    type_specializers: HashMap<GenericTypeId, GenericTypeBox>,
    extension_specializers: HashMap<GenericExtensionId, GenericExtensionBox>,
}
impl Default for Extensions {
    fn default() -> Self {
        Extensions {
            type_specializers: core::all_core_types(),
            extension_specializers: core::all_core_extensions(),
        }
    }
}
impl Extensions {
    /// Adds a specialization of a type to the set of concrete types.
    pub fn specialize_type(
        &self,
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, ExtensionError> {
        self.type_specializers
            .get(type_id)
            .ok_or_else(move || ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .get_info(args)
            .map_err(move |error| ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error,
            })
    }
    /// Adds a specialization of an extension to the set of concrete extensions.
    pub fn specialize_extension(
        &self,
        extension_id: &GenericExtensionId,
        args: &[GenericArg],
    ) -> Result<ConcreteExtensionBox, ExtensionError> {
        self.extension_specializers
            .get(extension_id)
            .ok_or_else(move || ExtensionError::ExtensionSpecialization {
                extension_id: extension_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(args)
            .map_err(move |error| ExtensionError::ExtensionSpecialization {
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
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Trait for a specialized extension.
pub trait ConcreteExtension {
    fn gen_code(&self) -> Result<Vec<Instruction>, ExtensionError> {
        Err(ExtensionError::NotImplemented)
    }
    /// Given the sets of memory cell values corresponding to inputs to the extension, returns the
    /// sets of memory cell values corresponding to outputs, and the index of the chosen branch.
    fn simulate(&self, inputs: Vec<Vec<MemCell>>)
    -> Result<(Vec<Vec<MemCell>>, usize), InputError>;
}

/// Trait for ConcreteExtension that isn't a branch.
trait NonBranchConcreteExtension {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError>;
}
impl<T: NonBranchConcreteExtension> ConcreteExtension for T {
    fn simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<(Vec<Vec<MemCell>>, usize), InputError> {
        Ok((self.non_branch_simulate(inputs)?, 0))
    }
}

type GenericExtensionBox = Box<dyn GenericExtension>;
pub type ConcreteExtensionBox = Box<dyn ConcreteExtension>;

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

#[derive(Debug, Clone, Eq, PartialEq)]
/// The information for a concrete type.
pub struct ConcreteTypeInfo {
    pub size: usize,
}

type GenericTypeBox = Box<dyn GenericType>;

#[cfg(test)]
mod test;
