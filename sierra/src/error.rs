#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch,
    MissingReference,
    VariableOverride,
    WrongNumberOfTypeArgs(String),
    WrongNumberOfArgs(String),
    WrongNumberOfResults(String),
    WrongNumberOfBranches(String),
    UnsupportedTypeArg,
    UnsupportedLibCallName,
    FunctionBlockOutOfBounds,
    FunctionTypeMismatch,
    FunctionRemainingOwnedObjects,
    FunctionBlockMismatch,
}
