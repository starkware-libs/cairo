/// Module for the set of core extensions.
pub mod core;
pub mod error;
pub mod lib_func;
/// All implementations of basic extensions are under this module.
pub mod modules;
pub mod type_specialization_context;
pub mod types;

use num_bigint::BigInt;

pub use self::error::{ExtensionError, SpecializationError};
pub use self::lib_func::{
    ConcreteLibfunc, GenericLibfunc, GenericLibfuncEx, NamedLibfunc, NoGenericArgsGenericLibfunc,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
};
pub use self::modules::*;
use self::type_specialization_context::TypeSpecializationContext;
pub use self::types::{
    ConcreteType, GenericType, GenericTypeEx, NamedType, NoGenericArgsGenericType,
};
use crate::ids::{ConcreteTypeId, FunctionId};
use crate::program::GenericArg;

/// Helper for extracting the value from the template arguments.
fn args_as_single_value(args: &[GenericArg]) -> Result<&BigInt, SpecializationError> {
    match args {
        [GenericArg::Value(c)] => Ok(c),
        [_] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}

/// Helper for extracting the type from the template arguments.
pub fn args_as_single_type(args: &[GenericArg]) -> Result<&ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty),
        [_] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}

/// Helper for extracting two types from the template arguments.
fn args_as_two_types(
    args: &[GenericArg],
) -> Result<(&ConcreteTypeId, &ConcreteTypeId), SpecializationError> {
    match args {
        [GenericArg::Type(ty0), GenericArg::Type(ty1)] => Ok((ty0, ty1)),
        [_, _] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}

/// Helper for extracting the type from the template arguments.
fn args_as_single_user_func(args: &[GenericArg]) -> Result<&FunctionId, SpecializationError> {
    match args {
        [GenericArg::UserFunc(function_id)] => Ok(function_id),
        [_] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}

/// Extracts the generic args of `ty`, additionally validates it is of generic type `T`.
fn extract_type_generic_args<T: NamedType>(
    context: &dyn TypeSpecializationContext,
    ty: &ConcreteTypeId,
) -> Result<Vec<GenericArg>, SpecializationError> {
    let long_id = context.get_type_info(ty)?.long_id;
    if long_id.generic_id != T::ID {
        Err(SpecializationError::UnsupportedGenericArg)
    } else {
        Ok(long_id.generic_args)
    }
}

#[cfg(test)]
mod test;
