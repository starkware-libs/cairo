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

/// Trait for implementing a specialization generator.
pub trait GenericExtension: Sized {
    type Concrete: ConcreteExtension;

    /// Instantiates the extension by id.
    fn by_id(id: &GenericExtensionId) -> Option<Self>;
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

// TODO(spapini): If GenericExtensionId becomes constexpr, use it here instead of name.
/// Trait for implementing a specialization generator with with a simple id.
pub trait NamedExtension: Default {
    type Concrete: ConcreteExtension;
    const NAME: &'static str;
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedExtension: NamedExtension> GenericExtension for TNamedExtension {
    type Concrete = <Self as NamedExtension>::Concrete;

    fn by_id(id: &GenericExtensionId) -> Option<Self> {
        if &GenericExtensionId::from(Self::NAME.to_string()) == id {
            return Some(Self::default());
        }
        None
    }

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        <Self as NamedExtension>::specialize(self, args)
    }
}

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericExtension: Default {
    type Concrete: ConcreteExtension;
    const NAME: &'static str;
    fn specialize(&self) -> Self::Concrete;
}
impl<T: NoGenericArgsGenericExtension> NamedExtension for T {
    type Concrete = <Self as NoGenericArgsGenericExtension>::Concrete;
    const NAME: &'static str = <Self as NoGenericArgsGenericExtension>::NAME;

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            Ok(self.specialize())
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
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

/// Forms an extension type from an enum of extensions.
/// The new enum implements GenericExtension.
/// All the variant types must also implement GenericExtension.
/// Usage example:
/// ```ignore
/// define_extension_hierarchy! {
///     pub enum MyExtension {
///       Ext0(Extension0),
///       Ext1(Extension1),
///     }, MyExtensionConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_extension_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty)),* },
            $concrete_name:ident) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::GenericExtension for $name {
            type Concrete = $concrete_name;
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
