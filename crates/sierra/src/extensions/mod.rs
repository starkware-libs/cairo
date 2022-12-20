/// Module for the set of core extensions.
pub mod core;
pub mod error;
pub mod lib_func;
/// All implementations of basic extensions are under this module.
pub mod modules;
pub mod type_specialization_context;
pub mod types;

pub use self::error::{ExtensionError, SpecializationError};
pub use self::lib_func::{
    ConcreteLibFunc, GenericLibFunc, GenericLibFuncEx, NamedLibFunc, NoGenericArgsGenericLibFunc,
    OutputVarReferenceInfo, SignatureBasedConcreteLibFunc,
};
pub use self::modules::*;
pub use self::types::{
    ConcreteType, GenericType, GenericTypeEx, NamedType, NoGenericArgsGenericType,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

/// Helper for extracting the type from the template arguments.
fn args_as_single_type(args: &[GenericArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty.clone()),
        [_] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}

#[cfg(test)]
mod test;
