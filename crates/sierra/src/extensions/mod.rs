use std::collections::HashMap;

use casm::instructions::Instruction;
use thiserror::Error;

use crate::ids::{ConcreteExtensionId, GenericExtensionId};
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

mod core;

/// Error occurring while specializing extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Could not find the requested extension")]
    UnsupportedLibCallName,
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
    #[error("Could not specialize extension")]
    Specialization { extension_id: GenericExtensionId, error: SpecializationError },
    #[error("Requested extension not declared.")]
    UndeclaredExtension { extension_id: ConcreteExtensionId },
    #[error("Encountered unexpected data in extension inputs")]
    Inputs { extension_id: GenericExtensionId, error: InputError },
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
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

#[cfg(test)]
mod test;
