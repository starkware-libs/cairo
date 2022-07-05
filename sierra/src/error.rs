use crate::graph::{Identifier, Type};

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(Identifier, Type, Type),
    MissingReference(Identifier, Type),
    VariableOverride(Identifier),
    ArgumentSizeMismatch,
    ResultSizeMismatch,
    WrongNumberOfTypeArgs,
    WrongNumberOfArgs(String),
    WrongNumberOfResults(String),
    WrongNumberOfBranches(String),
    UnsupportedTypeArg,
    UnsupportedLibCallName,
    FunctionBlockOutOfBounds,
    FunctionRemainingOwnedObjects,
    FunctionBlockMismatch,
}
