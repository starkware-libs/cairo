use itertools::Itertools;
use num_bigint::BigInt;
use smol_str::SmolStr;
use thiserror::Error;

use crate::ids::{ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Error occurring while specializing extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Could not find the requested extension: {0}")]
    UnsupportedId(SmolStr),
    #[error("Expected a different number of generic arguments")]
    WrongNumberOfGenericArgs,
    #[error("Provided generic argument is unsupported")]
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
    #[error(
        "Could not specialize libfunc `{libfunc_id}` with generic_args: [{}]. Error: {error}.",
        generic_args.iter().map(|arg| arg.to_string()).join(", ")
    )]
    LibfuncSpecialization {
        libfunc_id: GenericLibfuncId,
        generic_args: Vec<GenericArg>,
        error: SpecializationError,
    },
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
}
