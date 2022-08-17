mod core;
#[cfg(test)]
mod test;

use thiserror::Error;

pub use self::core::{CoreConcrete, CoreExtension};
use crate::ids::{ConcreteExtensionId, GenericExtensionId};
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

/// Error occurring while specializing extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Count not find the requested extension")]
    UnsupportedLibCallName,
    #[error("Expected a different number of template arguments")]
    WrongNumberOfTemplateArgs,
    #[error("Provided template arg is unsupported")]
    UnsupportedTemplateArg,
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

/// Trait for implementing a specialization generator.
pub trait GenericExtension: Sized {
    type Concrete: ConcreteExtension;

    fn id() -> Option<GenericExtensionId>;
    fn new() -> Option<Self>;
    fn by_id(id: &GenericExtensionId) -> Option<Self> {
        if &Self::id()? == id {
            return Self::new();
        }
        None
    }
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on GenericExtension.
pub trait GenericExtensionEx: GenericExtension {
    fn specialize_by_id(
        extension_id: &GenericExtensionId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericExtension: GenericExtension> GenericExtensionEx for TGenericExtension {
    fn specialize_by_id(
        extension_id: &GenericExtensionId,
        args: &[GenericArg],
    ) -> Result<TGenericExtension::Concrete, ExtensionError> {
        Self::by_id(extension_id)
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

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericExtension: Sized {
    type Concrete: ConcreteExtension;
    fn id() -> Option<GenericExtensionId>;
    fn new() -> Option<Self>;
    fn specialize(&self) -> Self::Concrete;
}
impl<T: NoGenericArgsGenericExtension> GenericExtension for T {
    type Concrete = <Self as NoGenericArgsGenericExtension>::Concrete;
    fn id() -> Option<GenericExtensionId> {
        <Self as NoGenericArgsGenericExtension>::id()
    }

    fn new() -> Option<Self> {
        <Self as NoGenericArgsGenericExtension>::new()
    }

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            Ok(self.specialize())
        } else {
            Err(SpecializationError::WrongNumberOfTemplateArgs)
        }
    }
}

/// Trait for a specialized extension.
pub trait ConcreteExtension {
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

#[macro_export]
macro_rules! extension_enum {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty)),* },
            $concrete_name:ident) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::GenericExtension for $name {
            type Concrete = $concrete_name;
            fn id() -> Option<$crate::ids::GenericExtensionId> { None }
            fn new() -> Option<Self> {
                None
            }
            fn by_id(id: &$crate::ids::GenericExtensionId) -> Option<Self> {
                $(
                    if let Some(res) = <$variant>::by_id(id){
                        return Some(Self::$variant_name(res));
                    }
                )*
                None
            }
            fn specialize(
                    &self, args: &[$crate::extensions::GenericArg]
            ) -> Result<Self::Concrete, $crate::extensions::SpecializationError>{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            Ok(Self::Concrete::$variant_name(<$variant as GenericExtension>::specialize(value, args)?.into()))
                        }
                    ),*
                }
            }
        }

        #[allow(clippy::enum_variant_names)]
        pub enum $concrete_name {
            $($variant_name (<$variant as GenericExtension> ::Concrete)),*
        }

        impl $crate::extensions::ConcreteExtension for $concrete_name {
            fn simulate(
                &self, inputs: Vec<Vec<MemCell>>
            ) -> Result<(Vec<Vec<MemCell>>, usize), InputError>{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            value.simulate(inputs)
                        }
                    ),*
                }
            }
        }
    }
}
