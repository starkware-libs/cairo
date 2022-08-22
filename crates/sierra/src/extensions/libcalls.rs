use super::error::{ExtensionError, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericLibcallId};
use crate::program::GenericArg;

/// Trait for implementing a specialization generator.
pub trait GenericLibcall: Sized {
    type Concrete: ConcreteLibcall;

    /// Instantiates the Libcall by id.
    fn by_id(id: &GenericLibcallId) -> Option<Self>;
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on GenericLibcall.
pub trait GenericLibcallEx: GenericLibcall {
    fn specialize_by_id(
        libcall_id: &GenericLibcallId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericLibcall: GenericLibcall> GenericLibcallEx for TGenericLibcall {
    fn specialize_by_id(
        libcall_id: &GenericLibcallId,
        args: &[GenericArg],
    ) -> Result<TGenericLibcall::Concrete, ExtensionError> {
        Self::by_id(libcall_id)
            .ok_or_else(move || ExtensionError::LibcallSpecialization {
                libcall_id: libcall_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(args)
            .map_err(move |error| ExtensionError::LibcallSpecialization {
                libcall_id: libcall_id.clone(),
                error,
            })
    }
}

// TODO(spapini): If GenericLibcallId becomes constexpr, use it here instead of name.
/// Trait for implementing a specialization generator with with a simple id.
pub trait NamedLibcall: Default {
    type Concrete: ConcreteLibcall;
    const NAME: &'static str;
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedLibcall: NamedLibcall> GenericLibcall for TNamedLibcall {
    type Concrete = <Self as NamedLibcall>::Concrete;

    fn by_id(id: &GenericLibcallId) -> Option<Self> {
        if &GenericLibcallId::from(Self::NAME.to_string()) == id {
            return Some(Self::default());
        }
        None
    }

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        <Self as NamedLibcall>::specialize(self, args)
    }
}

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericLibcall: Default {
    type Concrete: ConcreteLibcall;
    const NAME: &'static str;
    fn specialize(&self) -> Self::Concrete;
}
impl<T: NoGenericArgsGenericLibcall> NamedLibcall for T {
    type Concrete = <Self as NoGenericArgsGenericLibcall>::Concrete;
    const NAME: &'static str = <Self as NoGenericArgsGenericLibcall>::NAME;

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            Ok(self.specialize())
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Trait for a specialized library call.
pub trait ConcreteLibcall {
    fn input_types(&self) -> Vec<ConcreteTypeId>;
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>>;
    fn fallthrough(&self) -> Option<usize>;
}

/// Trait for a non branch specialized libcall.
pub trait NonBranchConcreteLibcall {
    fn input_types(&self) -> Vec<ConcreteTypeId>;
    fn output_types(&self) -> Vec<ConcreteTypeId>;
}
impl<TNonBranchConcreteLibcall: NonBranchConcreteLibcall> ConcreteLibcall
    for TNonBranchConcreteLibcall
{
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        <Self as NonBranchConcreteLibcall>::input_types(self)
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![<Self as NonBranchConcreteLibcall>::output_types(self)]
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(0)
    }
}

/// Forms a concrete library call type from an enum of library calls.
/// The new enum implements ConcreteLibcall.
/// All the variant types must also implement ConcreteLibcall.
/// Usage example:
/// ```ignore
/// define_concrete_libcall_hierarchy! {
///     pub enum MyLibcall {
///       LC0(Libcall0),
///       LC1(Libcall1),
///     }
/// }
/// ```
#[macro_export]
macro_rules! define_concrete_libcall_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* }) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant),)*
        }

        impl $crate::extensions::ConcreteLibcall for $name {
            fn input_types(&self) -> Vec<$crate::ids::ConcreteTypeId> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteLibcall>::input_types(value)),*
                }
            }
            fn output_types(&self) -> Vec<Vec<$crate::ids::ConcreteTypeId>> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteLibcall>::output_types(value)),*
                }
            }
            fn fallthrough(&self) -> Option<usize> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteLibcall>::fallthrough(value)),*
                }
            }
        }
    }
}

/// Forms a libcall type from an enum of libcalls.
/// The new enum implements GenericLibcall.
/// All the variant types must also implement GenericLibcall.
/// Usage example:
/// ```ignore
/// define_libcall_hierarchy! {
///     pub enum MyLibcall {
///       LC0(Libcall0),
///       LC1(Libcall1),
///     }, MyLibcallConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_libcall_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* },
    $concrete_name:ident) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::GenericLibcall for $name {
            type Concrete = $concrete_name;
            fn by_id(id: &$crate::ids::GenericLibcallId) -> Option<Self> {
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
                            Ok(Self::Concrete::$variant_name(<$variant as GenericLibcall>::specialize(value, args)?.into()))
                        }
                    ),*
                }
            }
        }

        $crate::define_concrete_libcall_hierarchy! {
            pub enum $concrete_name {
                $($variant_name (<$variant as GenericLibcall> ::Concrete),)*
            }
        }
    }
}
