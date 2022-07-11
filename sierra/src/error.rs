use crate::graph::{BlockId, Identifier, Type};

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(Identifier),
    MissingReference(Identifier),
    VariableOverride(Identifier),
    ArgumentSizeMismatch,
    ResultSizeMismatch,
    WrongNumberOfTypeArgs,
    UnsupportedTypeArg,
    UnsupportedTypeName(String),
    UnsupportedLibCallName(String),
    FunctionBlockOutOfBounds,
    FunctionRemainingOwnedObjects(Vec<Identifier>),
    FunctionBlockIdentifiersMismatch(BlockId, Vec<Identifier>, Vec<Identifier>),
    FunctionBlockIdentifierTypeMismatch(BlockId, Identifier, Type, Type),
    FunctionInvocationMismatch(String),
    FunctionJumpMismatch(String),
}
