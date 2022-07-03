#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch,
    MissingReference,
    VariableOverride,
    UnconsumedOwnedType,
    WrongNumberOfTypeArgs(String),
    WrongNumberOfArgs(String),
    WrongNumberOfResults(String),
    WrongNumberOfBranches(String),
    UnsupportedTypeArg,
    UnsupportedLibCallName,
}
