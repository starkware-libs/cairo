#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch,
    MissingReference,
    VariableOverride,
    UnconsumedOwnedVar,
    WrongNumberOfTypeArgs(String),
    WrongNumberOfArgs(String),
    WrongNumberOfResults(String),
    WrongNumberOfBranches(String),
    UnsupportedTypeArg,
    UnsupportedLibCallName,
    FunctionBlockOutOfBounds,
    FunctionNoExit,
    FunctionTypeMismatch,
    FunctionRemainingOwnedObjects,
    FunctionBlockMismatch,
}
