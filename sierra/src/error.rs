use crate::graph::Type;

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(String, Type, Type),
    MissingReference(String, Type),
    VariableOverride(String),
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
