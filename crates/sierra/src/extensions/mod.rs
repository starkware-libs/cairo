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

#[cfg(test)]
mod test;
