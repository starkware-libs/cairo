/// Module for the set of core extensions.
pub mod core;
pub mod error;
pub mod lib_func;
/// All implementations of basic extensions are under this module.
pub mod modules;
pub mod types;

pub use self::error::{ExtensionError, SpecializationError};
pub use self::lib_func::{
    ConcreteLibFunc, GenericLibFunc, GenericLibFuncEx, NamedLibFunc, NoGenericArgsGenericLibFunc,
    NonBranchConcreteLibFunc,
};
pub use self::modules::*;
pub use self::types::{
    ConcreteType, GenericType, GenericTypeEx, NamedType, NoGenericArgsGenericType,
};
pub trait ExtensionSuite {
    type Type: GenericType;
    type LibFunc: GenericLibFunc;
}

#[cfg(test)]
mod test;
