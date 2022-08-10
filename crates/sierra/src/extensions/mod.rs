pub mod core;
#[cfg(test)]
mod test;

use std::collections::HashMap;

use thiserror::Error;

pub use self::core::{CoreConcrete, CoreExtension, CoreType};
use crate::ids::{ConcreteExtensionId, ConcreteTypeId, GenericExtensionId, GenericTypeId};
use crate::program::GenericArg;

/// Error occurring while making a type or an extension concrete.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Could not find the requested generic id")]
    UnsupportedId,
    #[error("Expected a different number of generic arguments")]
    WrongNumberOfGenericArgs,
    #[error("Provided generic arg is unsupported")]
    UnsupportedGenericArg,
    #[error("Required type is missing in registry")]
    UsedUnregisteredType(ConcreteTypeId),
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
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
}

/// Trait for implementing an extension specialization generator.
pub trait GenericType: Sized {
    /// Instantiates the type by id.
    fn by_id(id: &GenericTypeId) -> Option<Self>;
    /// Creates the specialization with the template arguments.
    fn get_concrete_info(
        &self,
        concrete_type_registry: &ConcreteTypeRegistry,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, SpecializationError>;
}

pub type ConcreteTypeRegistry = HashMap<ConcreteTypeId, ConcreteTypeInfo>;

/// Trait for introducing helper methods on GenericType.
pub trait GenericTypeEx: GenericType {
    fn get_concrete_info_by_id(
        concrete_type_registry: &ConcreteTypeRegistry,
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, ExtensionError>;
}

impl<TGenericType: GenericType> GenericTypeEx for TGenericType {
    fn get_concrete_info_by_id(
        concrete_type_registry: &ConcreteTypeRegistry,
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, ExtensionError> {
        Self::by_id(type_id)
            .ok_or_else(move || ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .get_concrete_info(concrete_type_registry, args)
            .map_err(move |error| ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error,
            })
    }
}

// TODO(orizi): If GenericTypeId becomes constexpr, use it here instead of name.
/// Trait for implementing a type info generator with with a simple id.
pub trait NamedType: Default {
    const NAME: &'static str;
    /// Creates the specialization with the template arguments.
    fn get_concrete_info(
        &self,
        concrete_type_registry: &ConcreteTypeRegistry,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, SpecializationError>;
}
impl<TNamedType: NamedType> GenericType for TNamedType {
    fn by_id(id: &GenericTypeId) -> Option<Self> {
        if &GenericTypeId::from_string(Self::NAME) == id { Some(Self::default()) } else { None }
    }

    fn get_concrete_info(
        &self,
        concrete_type_registry: &ConcreteTypeRegistry,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, SpecializationError> {
        <Self as NamedType>::get_concrete_info(self, concrete_type_registry, args)
    }
}

pub trait NoGenericArgsNamedType: Default {
    const NAME: &'static str;
    const SIZE: usize;
}
impl<TNoGenericArgsNamedType: NoGenericArgsNamedType> NamedType for TNoGenericArgsNamedType {
    const NAME: &'static str = <Self as NoGenericArgsNamedType>::NAME;
    fn get_concrete_info(
        &self,
        _concrete_type_registry: &ConcreteTypeRegistry,
        args: &[GenericArg],
    ) -> Result<ConcreteTypeInfo, SpecializationError> {
        if args.is_empty() {
            Ok(ConcreteTypeInfo { size: Self::SIZE })
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

/// Forms an generic-type type from an enum of generic-types.
/// The new enum implements GenericType.
/// All the variant types must also implement GenericType.
/// Usage example:
/// ```ignore
/// define_type_hierarchy! {
///     pub enum MyType {
///       Ty0(Type0),
///       Ty1(Type1)
///     },
/// }
/// ```
#[macro_export]
macro_rules! define_type_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty)),* }) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::GenericType for $name {
            fn by_id(id: &$crate::ids::GenericTypeId) -> Option<Self> {
                $(
                    if let Some(res) = <$variant>::by_id(id){
                        return Some(Self::$variant_name(res));
                    }
                )*
                None
            }
            fn get_concrete_info(
                    &self,
                    concrete_type_registry: &$crate::extensions::ConcreteTypeRegistry,
                    args: &[$crate::extensions::GenericArg]
            ) -> Result<$crate::extensions::ConcreteTypeInfo, $crate::extensions::SpecializationError>{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            <$variant as $crate::extensions::GenericType>::get_concrete_info(value, concrete_type_registry, args)
                        }
                    ),*
                }
            }
        }
    }
}

/// Trait for implementing an extension specialization generator.
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
        if &GenericExtensionId::from_string(Self::NAME) == id {
            Some(Self::default())
        } else {
            None
        }
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
    fn input_types(&self) -> Vec<ConcreteTypeId>;
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>>;
    fn fallthrough(&self) -> Option<usize>;
}

/// Trait for a non branch specialized extension.
pub trait NonBranchConcreteExtension {
    fn input_types(&self) -> Vec<ConcreteTypeId>;
    fn output_types(&self) -> Vec<ConcreteTypeId>;
}
impl<TNonBranchConcreteExtension: NonBranchConcreteExtension> ConcreteExtension
    for TNonBranchConcreteExtension
{
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        <Self as NonBranchConcreteExtension>::input_types(self)
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![<Self as NonBranchConcreteExtension>::output_types(self)]
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(0)
    }
}

/// Forms a concrete extension type from an enum of extensions.
/// The new enum implements ConcreteExtension.
/// All the variant types must also implement ConcreteExtension.
/// Usage example:
/// ```ignore
/// define_concrete_extension_hierarchy! {
///     pub enum MyExtension {
///       Ext0(Extension0),
///       Ext1(Extension1),
///     }, MyExtensionConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_concrete_extension_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* }) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant),)*
        }

        impl $crate::extensions::ConcreteExtension for $name {
            fn input_types(&self) -> Vec<$crate::extensions::ConcreteTypeId> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteExtension>::input_types(value)),*
                }
            }
            fn output_types(&self) -> Vec<Vec<$crate::extensions::ConcreteTypeId>> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteExtension>::output_types(value)),*
                }
            }
            fn fallthrough(&self) -> Option<usize> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteExtension>::fallthrough(value)),*
                }
            }
        }
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
///       Ext1(Extension1)
///     }, MyExtensionConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_extension_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* },
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
                            Ok(Self::Concrete::$variant_name(<$variant as $crate::extensions::GenericExtension>::specialize(value, args)?.into()))
                        }
                    ),*
                }
            }
        }

        $crate::define_concrete_extension_hierarchy! {
            pub enum $concrete_name {
                $($variant_name (<$variant as $crate::extensions::GenericExtension> ::Concrete),)*
            }
        }
    }
}
