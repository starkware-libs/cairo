use super::args_as_single_type;
use super::error::{ExtensionError, SpecializationError};
use super::type_specialization_context::TypeSpecializationContext;
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

/// Trait for implementing a specialization generator for types.
pub trait GenericType: Sized {
    type Concrete: ConcreteType;

    /// Instantiates the type by id.
    fn by_id(id: &GenericTypeId) -> Option<Self>;
    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on GenericType.
pub trait GenericTypeEx: GenericType {
    fn specialize_by_id(
        context: &dyn TypeSpecializationContext,
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericType: GenericType> GenericTypeEx for TGenericType {
    fn specialize_by_id(
        context: &dyn TypeSpecializationContext,
        type_id: &GenericTypeId,
        args: &[GenericArg],
    ) -> Result<TGenericType::Concrete, ExtensionError> {
        Self::by_id(type_id)
            .ok_or_else(move || ExtensionError::TypeSpecialization {
                type_id: type_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(context, args)
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
    /// Returns the long ID of the concrete type with `ID` as the generic ID and the given args.
    fn concrete_type_long_id(generic_args: &[GenericArg]) -> ConcreteTypeLongId {
        ConcreteTypeLongId { generic_id: Self::id(), generic_args: generic_args.to_vec() }
    }
    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedType: NamedType> GenericType for TNamedType {
    type Concrete = <Self as NamedType>::Concrete;

    fn by_id(id: &GenericTypeId) -> Option<Self> {
        if &Self::ID == id { Some(Self::default()) } else { None }
    }

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        <Self as NamedType>::specialize(self, context, args)
    }
}

/// Trait for describing a generic type with no generic arguments.
pub trait NoGenericArgsGenericType: Default {
    const ID: GenericTypeId;
    const STORABLE: bool;
    const DUPLICATABLE: bool;
    const DROPPABLE: bool;
    const SIZE: i16;
}
impl<T: NoGenericArgsGenericType> NamedType for T {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = <Self as NoGenericArgsGenericType>::ID;

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            Ok(Self::Concrete {
                info: TypeInfo {
                    long_id: Self::concrete_type_long_id(args),
                    storable: T::STORABLE,
                    droppable: T::DROPPABLE,
                    duplicatable: T::DUPLICATABLE,
                    size: T::SIZE,
                },
            })
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Trait for describing a generic type with a single type arg.
pub trait GenericTypeArgGenericType: Default {
    const ID: GenericTypeId;

    /// Returns the type info of the wrapping type.
    fn calc_info(
        &self,
        long_id: ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError>;
}

/// Wrapper for a specialization generator with a single type arg.
#[derive(Default)]
pub struct GenericTypeArgGenericTypeWrapper<T: GenericTypeArgGenericType>(T);
impl<T: GenericTypeArgGenericType> NamedType for GenericTypeArgGenericTypeWrapper<T> {
    type Concrete = InfoAndTypeConcreteType;
    const ID: GenericTypeId = T::ID;

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let long_id = Self::concrete_type_long_id(args);
        let wrapped_info = context.get_type_info(ty.clone())?;
        Ok(Self::Concrete { info: self.0.calc_info(long_id, wrapped_info)?, ty })
    }
}

/// Information on Sierra types required for generic libfunc calls.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeInfo {
    /// The long ID of the concrete type.
    pub long_id: ConcreteTypeLongId,
    /// Can the type be stored by any of the store commands.
    pub storable: bool,
    /// Can the type be (trivially) dropped.
    pub droppable: bool,
    /// Can the type be (trivially) duplicated.
    pub duplicatable: bool,
    /// The size of an element of this type.
    pub size: i16,
}

/// Trait for a specialized type.
pub trait ConcreteType {
    fn info(&self) -> &TypeInfo;
}

/// Struct providing a ConcreteType only with the type info - should not be implemented for
/// concrete types that require any extra data.
pub struct InfoOnlyConcreteType {
    pub info: TypeInfo,
}

impl ConcreteType for InfoOnlyConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// Struct providing a ConcreteType with the type info and a wrapped type.
pub struct InfoAndTypeConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}

impl ConcreteType for InfoAndTypeConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

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
                    &self,
                    context: &dyn $crate::extensions::type_specialization_context::TypeSpecializationContext,
                    args: &[$crate::program::GenericArg]
            ) -> Result<Self::Concrete, $crate::extensions::SpecializationError>{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            Ok(Self::Concrete::$variant_name(
                                <$variant as $crate::extensions::GenericType>::specialize(
                                    value, context, args,
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
        impl $crate::extensions::ConcreteType for $concrete_name {
            fn info(&self) -> &$crate::extensions::types::TypeInfo {
                match self {
                    $(Self::$variant_name(value) => value.info()),*
                }
            }
        }
    }
}
