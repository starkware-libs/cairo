use thiserror::Error;

use crate::ids::GenericLibcallId;

/// Error occurring while specializing extensions.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum SpecializationError {
    #[error("Could not find the requested extension")]
    UnsupportedId,
    #[error("Expected a different number of generic arguments")]
    WrongNumberOfGenericArgs,
    #[error("Provided generic arg is unsupported")]
    UnsupportedGenericArg,
}

/// Extension related errors.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionError {
    #[error("Could not specialize libcall")]
    LibcallSpecialization { libcall_id: GenericLibcallId, error: SpecializationError },
    #[error("The requested functionality is not implemented yet")]
    NotImplemented,
}
