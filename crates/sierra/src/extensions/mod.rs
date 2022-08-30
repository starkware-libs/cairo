pub mod core;
pub mod error;
pub mod lib_func;
pub mod types;

pub use self::core::{CoreConcreteLibFunc, CoreLibFunc, CoreType, CoreTypeConcrete};
pub use self::error::{ExtensionError, SpecializationError};
pub use self::lib_func::{
    ConcreteLibFunc, GenericLibFunc, GenericLibFuncEx, NamedLibFunc, NoGenericArgsGenericLibFunc,
    NonBranchConcreteLibFunc,
};
pub use self::types::{
    ConcreteType, GenericType, GenericTypeEx, NamedType, NoGenericArgsGenericType,
};
pub trait ExtensionSuite {
    type Type: GenericType;
    type LibFunc: GenericLibFunc;
}

#[cfg(test)]
mod test;
