use super::error::{ExtensionError, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};
use crate::program::GenericArg;

/// Trait for implementing a libfunc specialization generator.
pub trait GenericLibFunc: Sized {
    type Concrete: ConcreteLibFunc;

    /// Instantiates the libfunc by id.
    fn by_id(id: &GenericLibFuncId) -> Option<Self>;
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on GenericLibFunc.
pub trait GenericLibFuncEx: GenericLibFunc {
    fn specialize_by_id(
        libfunc_id: &GenericLibFuncId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericLibFunc: GenericLibFunc> GenericLibFuncEx for TGenericLibFunc {
    fn specialize_by_id(
        libfunc_id: &GenericLibFuncId,
        args: &[GenericArg],
    ) -> Result<TGenericLibFunc::Concrete, ExtensionError> {
        Self::by_id(libfunc_id)
            .ok_or_else(move || ExtensionError::LibFuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(args)
            .map_err(move |error| ExtensionError::LibFuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error,
            })
    }
}

// TODO(spapini): If GenericLibFuncId becomes constexpr, use it here instead of name.
/// Trait for implementing a specialization generator with with a simple id.
pub trait NamedLibFunc: Default {
    type Concrete: ConcreteLibFunc;
    const NAME: &'static str;
    /// Creates the specialization with the template arguments.
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedLibFunc: NamedLibFunc> GenericLibFunc for TNamedLibFunc {
    type Concrete = <Self as NamedLibFunc>::Concrete;

    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        if &GenericLibFuncId::from(Self::NAME.to_string()) == id {
            return Some(Self::default());
        }
        None
    }

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        <Self as NamedLibFunc>::specialize(self, args)
    }
}

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericLibFunc: Default {
    type Concrete: ConcreteLibFunc;
    const NAME: &'static str;
    fn specialize(&self) -> Self::Concrete;
}
impl<T: NoGenericArgsGenericLibFunc> NamedLibFunc for T {
    type Concrete = <Self as NoGenericArgsGenericLibFunc>::Concrete;
    const NAME: &'static str = <Self as NoGenericArgsGenericLibFunc>::NAME;

    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            Ok(self.specialize())
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Trait for a specialized library call.
pub trait ConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId>;
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>>;
    fn fallthrough(&self) -> Option<usize>;
}

/// Trait for a non branch specialized libfunc.
pub trait NonBranchConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId>;
    fn output_types(&self) -> Vec<ConcreteTypeId>;
}
impl<TNonBranchConcreteLibFunc: NonBranchConcreteLibFunc> ConcreteLibFunc
    for TNonBranchConcreteLibFunc
{
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        <Self as NonBranchConcreteLibFunc>::input_types(self)
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![<Self as NonBranchConcreteLibFunc>::output_types(self)]
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(0)
    }
}

/// Forms a concrete library function type from an enum of library calls.
/// The new enum implements ConcreteLibFunc.
/// All the variant types must also implement ConcreteLibFunc.
/// Usage example:
/// ```ignore
/// define_concrete_libfunc_hierarchy! {
///     pub enum MyLibFunc {
///       LF0(LibFunc0),
///       LF1(LibFunc1),
///     }
/// }
/// ```
#[macro_export]
macro_rules! define_concrete_libfunc_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* }) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant),)*
        }

        impl $crate::extensions::ConcreteLibFunc for $name {
            fn input_types(&self) -> Vec<$crate::ids::ConcreteTypeId> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteLibFunc>::input_types(value)),*
                }
            }
            fn output_types(&self) -> Vec<Vec<$crate::ids::ConcreteTypeId>> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteLibFunc>::output_types(value)),*
                }
            }
            fn fallthrough(&self) -> Option<usize> {
                match self {
                    $(Self::$variant_name(value) =>
                        <$variant as $crate::extensions::ConcreteLibFunc>::fallthrough(value)),*
                }
            }
        }
    }
}

/// Forms a libfunc type from an enum of libfuncs.
/// The new enum implements GenericLibFunc.
/// All the variant types must also implement GenericLibFunc.
/// Usage example:
/// ```ignore
/// define_libfunc_hierarchy! {
///     pub enum MyLibFunc {
///       LF0(LibFunc0),
///       LF1(LibFunc1),
///     }, MyLibFuncConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_libfunc_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* },
    $concrete_name:ident) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::GenericLibFunc for $name {
            type Concrete = $concrete_name;
            fn by_id(id: &$crate::ids::GenericLibFuncId) -> Option<Self> {
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
                            let inner = <$variant as GenericLibFunc>::specialize(value, args)?;
                            Ok(Self::Concrete::$variant_name(inner.into()))
                        }
                    ),*
                }
            }
        }

        $crate::define_concrete_libfunc_hierarchy! {
            pub enum $concrete_name {
                $($variant_name (<$variant as GenericLibFunc> ::Concrete),)*
            }
        }
    }
}
