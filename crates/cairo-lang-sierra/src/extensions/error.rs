use num_bigint::BigInt;
use thiserror::Error;

use crate::ids::{ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Error occurring while specializing extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Could not find the requested extension")]
    UnsupportedId,
    #[error("Expected a different number of generic arguments")]
    WrongNumberOfGenericArgs,
    #[error("Provided generic arg is unsupported")]
    UnsupportedGenericArg,
    #[error("index is out of a relevant range")]
    IndexOutOfRange {
        index: BigInt,
        /// Range is [0, range_size - 1]
        range_size: usize,
    },
    #[error("Could not find the requested function")]
    MissingFunction(FunctionId),
    #[error("Generic type was not specialized with such arguments")]
    TypeWasNotDeclared(GenericTypeId, Vec<GenericArg>),
    #[error("Missing type info for the requested type")]
    MissingTypeInfo(ConcreteTypeId),
}

/// Extension related errors.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionError {
    #[error("Could not specialize type")]
    TypeSpecialization { type_id: GenericTypeId, error: SpecializationError },
    #[error("Could not specialize libfunc")]
    LibfuncSpecialization {
        libfunc_id: GenericLibfuncId,
        generic_args: Vec<GenericArg>,
        error: SpecializationError,
    },
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
}
