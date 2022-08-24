use super::error::{ExtensionError, SpecializationError};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Trait for implementing a specialization generator for types.
pub trait GenericType: Sized {
    type Concrete: ConcreteType;

    /// Instantiates the type by id.
    fn by_id(id: &GenericTypeId) -> Option<Self>;
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on GenericType.
pub trait GenericTypeEx: GenericType {
    fn specialize_by_id(
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericType: GenericType> GenericTypeEx for TGenericType {
    fn specialize_by_id(
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<TGenericType::Concrete, ExtensionError> {
        Self::by_id(type_id)
            .ok_or_else(move || ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(args)
            .map_err(move |error| ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error,
            })
    }
}

/// Trait for implementing a specialization generator with with a simple id.
pub trait NamedType: Default {
    type Concrete: ConcreteType;
    const ID: GenericTypeId;
    /// Returns the generic id of named types.
    fn id() -> GenericTypeId {
        Self::ID
    }
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedType: NamedType> GenericType for TNamedType {
    type Concrete = <Self as NamedType>::Concrete;

    fn by_id(id: &GenericTypeId) -> Option<Self> {
        if &Self::ID == id { Some(Self::default()) } else { None }
    }

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        <Self as NamedType>::specialize(self, args)
    }
}

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericType: Default {
    type Concrete: ConcreteType + Default;
    const ID: GenericTypeId;
}
impl<T: NoGenericArgsGenericType> NamedType for T {
    type Concrete = <Self as NoGenericArgsGenericType>::Concrete;
    const ID: GenericTypeId = <Self as NoGenericArgsGenericType>::ID;

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            Ok(Self::Concrete::default())
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Trait for a specialized type.
pub trait ConcreteType {}

/// Forms a Sierra type used by extensions type from an enum of such types.
/// The new enum implements GenericType.
/// All the variant types must also implement GenericType.
/// Usage example:
/// ```ignore
/// define_type_hierarchy! {
///     pub enum MyType {
///       Ty0(Type0),
///       Ty1(Type1),
///     }, MyTypeConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_type_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* },
    $concrete_name:ident) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::types::GenericType for $name {
            type Concrete = $concrete_name;
            fn by_id(id: &$crate::ids::GenericTypeId) -> Option<Self> {
                $(
                    if let Some(res) = <$variant>::by_id(id){
                        return Some(Self::$variant_name(res));
                    }
                )*
                None
            }
            fn specialize(
                    &self, args: &[$crate::program::GenericArg]
            ) -> Result<Self::Concrete, $crate::extensions::SpecializationError>{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            Ok(Self::Concrete::$variant_name(
                                <$variant as $crate::extensions::GenericType>::specialize(
                                    value, args,
                                )?
                                .into(),
                            ))
                        }
                    ),*
                }
            }
        }

        pub enum $concrete_name {
            $($variant_name (<$variant as $crate::extensions::GenericType> ::Concrete),)*
        }
        impl $crate::extensions::ConcreteType for $concrete_name {}
    }
}
